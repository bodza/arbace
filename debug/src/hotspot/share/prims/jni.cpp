#include "precompiled.hpp"

#include "jni.h"
#include "jvm.h"
#include "ci/ciReplay.hpp"
#include "classfile/altHashing.hpp"
#include "classfile/classFileStream.hpp"
#include "classfile/classLoader.hpp"
#include "classfile/javaClasses.hpp"
#include "classfile/javaClasses.inline.hpp"
#include "classfile/modules.hpp"
#include "classfile/symbolTable.hpp"
#include "classfile/systemDictionary.hpp"
#include "classfile/vmSymbols.hpp"
#include "gc/shared/gcLocker.inline.hpp"
#include "interpreter/linkResolver.hpp"
#include "memory/allocation.hpp"
#include "memory/allocation.inline.hpp"
#include "memory/oopFactory.hpp"
#include "memory/resourceArea.hpp"
#include "memory/universe.hpp"
#include "oops/access.inline.hpp"
#include "oops/arrayOop.inline.hpp"
#include "oops/instanceKlass.hpp"
#include "oops/instanceOop.hpp"
#include "oops/markOop.hpp"
#include "oops/method.hpp"
#include "oops/objArrayKlass.hpp"
#include "oops/objArrayOop.inline.hpp"
#include "oops/oop.inline.hpp"
#include "oops/symbol.hpp"
#include "oops/typeArrayKlass.hpp"
#include "oops/typeArrayOop.inline.hpp"
#include "prims/jniCheck.hpp"
#include "prims/jniFastGetField.hpp"
#include "prims/jvm_misc.hpp"
#include "runtime/atomic.hpp"
#include "runtime/compilationPolicy.hpp"
#include "runtime/fieldDescriptor.hpp"
#include "runtime/handles.inline.hpp"
#include "runtime/interfaceSupport.inline.hpp"
#include "runtime/java.hpp"
#include "runtime/javaCalls.hpp"
#include "runtime/jfieldIDWorkaround.hpp"
#include "runtime/jniHandles.inline.hpp"
#include "runtime/orderAccess.hpp"
#include "runtime/reflection.hpp"
#include "runtime/safepointVerifiers.hpp"
#include "runtime/sharedRuntime.hpp"
#include "runtime/signature.hpp"
#include "runtime/thread.inline.hpp"
#include "runtime/vm_operations.hpp"
#include "utilities/defaultStream.hpp"
#include "utilities/histogram.hpp"
#include "utilities/internalVMTests.hpp"
#include "utilities/macros.hpp"
#include "utilities/vmError.hpp"
#include "jvmci/jvmciCompiler.hpp"
#include "jvmci/jvmciRuntime.hpp"

static jint CurrentVersion = JNI_VERSION_10;

// out-of-line helpers for class jfieldIDWorkaround:

bool jfieldIDWorkaround::is_valid_jfieldID(Klass* k, jfieldID id) {
  if (jfieldIDWorkaround::is_instance_jfieldID(k, id)) {
    uintptr_t as_uint = (uintptr_t) id;
    intptr_t offset = raw_instance_offset(id);
    if (is_checked_jfieldID(id)) {
      if (!klass_hash_ok(k, id)) {
        return false;
      }
    }
    return InstanceKlass::cast(k)->contains_field_offset(offset);
  } else {
    JNIid* result = (JNIid*) id;
    return result != NULL;
  }
}

intptr_t jfieldIDWorkaround::encode_klass_hash(Klass* k, intptr_t offset) {
  if (offset <= small_offset_mask) {
    Klass* field_klass = k;
    Klass* super_klass = field_klass->super();
    // With compressed oops the most super class with nonstatic fields would
    // be the owner of fields embedded in the header.
    while (InstanceKlass::cast(super_klass)->has_nonstatic_fields() && InstanceKlass::cast(super_klass)->contains_field_offset(offset)) {
      field_klass = super_klass;   // super contains the field also
      super_klass = field_klass->super();
    }
    uintptr_t klass_hash = field_klass->identity_hash();
    return ((klass_hash & klass_mask) << klass_shift) | checked_mask_in_place;
  } else {
    return 0;
  }
}

bool jfieldIDWorkaround::klass_hash_ok(Klass* k, jfieldID id) {
  uintptr_t as_uint = (uintptr_t) id;
  intptr_t klass_hash = (as_uint >> klass_shift) & klass_mask;
  do {
    // Could use a non-blocking query for identity_hash here...
    if ((k->identity_hash() & klass_mask) == klass_hash)
      return true;
    k = k->super();
  } while (k != NULL);
  return false;
}

void jfieldIDWorkaround::verify_instance_jfieldID(Klass* k, jfieldID id) {
  guarantee(jfieldIDWorkaround::is_instance_jfieldID(k, id), "must be an instance field" );
  uintptr_t as_uint = (uintptr_t) id;
  intptr_t offset = raw_instance_offset(id);
  if (VerifyJNIFields) {
    if (is_checked_jfieldID(id)) {
      guarantee(klass_hash_ok(k, id), "Bug in native code: jfieldID class must match object");
    }
  }
  guarantee(InstanceKlass::cast(k)->contains_field_offset(offset), "Bug in native code: jfieldID offset must address interior of object");
}

// Implementation of JNI entries

JNI_ENTRY(jclass, jni_DefineClass(JNIEnv *env, const char *name, jobject loaderRef, const jbyte *buf, jsize bufLen))
  jclass cls = NULL;

  TempNewSymbol class_name = NULL;
  // Since exceptions can be thrown, class initialization can take place
  // if name is NULL no check for class name in .class stream has to be made.
  if (name != NULL) {
    const int str_len = (int)strlen(name);
    if (str_len > Symbol::max_length()) {
      // It's impossible to create this class;  the name cannot fit
      // into the constant pool.
      Exceptions::fthrow(THREAD_AND_LOCATION, vmSymbols::java_lang_NoClassDefFoundError(), "Class name exceeds maximum length of %d: %s", Symbol::max_length(), name);
      return 0;
    }
    class_name = SymbolTable::new_symbol(name, CHECK_NULL);
  }
  ResourceMark rm(THREAD);
  ClassFileStream st((u1*)buf, bufLen, NULL, ClassFileStream::verify);
  Handle class_loader (THREAD, JNIHandles::resolve(loaderRef));

  Klass* k = SystemDictionary::resolve_from_stream(class_name, class_loader, Handle(), &st, CHECK_NULL);

  cls = (jclass)JNIHandles::make_local(env, k->java_mirror());
  return cls;
JNI_END

static bool first_time_FindClass = true;

JNI_ENTRY(jclass, jni_FindClass(JNIEnv *env, const char *name))
  jclass result = NULL;

  // Remember if we are the first invocation of jni_FindClass
  bool first_time = first_time_FindClass;
  first_time_FindClass = false;

  // Sanity check the name:  it cannot be null or larger than the maximum size
  // name we can fit in the constant pool.
  if (name == NULL) {
    THROW_MSG_0(vmSymbols::java_lang_NoClassDefFoundError(), "No class name given");
  }
  if ((int)strlen(name) > Symbol::max_length()) {
    Exceptions::fthrow(THREAD_AND_LOCATION, vmSymbols::java_lang_NoClassDefFoundError(), "Class name exceeds maximum length of %d: %s", Symbol::max_length(), name);
    return 0;
  }

  //%note jni_3
  Handle protection_domain;
  // Find calling class
  Klass* k = thread->security_get_caller_class(0);
  // default to the system loader when no context
  Handle loader(THREAD, SystemDictionary::java_system_loader());
  if (k != NULL) {
    // Special handling to make sure JNI_OnLoad and JNI_OnUnload are executed
    // in the correct class context.
    if (k->class_loader() == NULL && k->name() == vmSymbols::java_lang_ClassLoader_NativeLibrary()) {
      JavaValue result(T_OBJECT);
      JavaCalls::call_static(&result, k, vmSymbols::getFromClass_name(), vmSymbols::void_class_signature(), CHECK_NULL);
      // When invoked from JNI_OnLoad, NativeLibrary::getFromClass returns
      // a non-NULL Class object.  When invoked from JNI_OnUnload,
      // it will return NULL to indicate no context.
      oop mirror = (oop) result.get_jobject();
      if (mirror != NULL) {
        Klass* fromClass = java_lang_Class::as_Klass(mirror);
        loader = Handle(THREAD, fromClass->class_loader());
        protection_domain = Handle(THREAD, fromClass->protection_domain());
      }
    } else {
      loader = Handle(THREAD, k->class_loader());
    }
  }

  TempNewSymbol sym = SymbolTable::new_symbol(name, CHECK_NULL);
  result = find_class_from_class_loader(env, sym, true, loader, protection_domain, true, thread);

  // If we were the first invocation of jni_FindClass, we enable compilation again
  // rather than just allowing invocation counter to overflow and decay.
  // Controlled by flag DelayCompilationDuringStartup.
  if (first_time && !CompileTheWorld)
    CompilationPolicy::completed_vm_startup();

  return result;
JNI_END

JNI_ENTRY(jmethodID, jni_FromReflectedMethod(JNIEnv *env, jobject method))
  // method is a handle to a java.lang.reflect.Method object
  oop reflected  = JNIHandles::resolve_non_null(method);
  oop mirror     = NULL;
  int slot       = 0;

  if (reflected->klass() == SystemDictionary::reflect_Constructor_klass()) {
    mirror = java_lang_reflect_Constructor::clazz(reflected);
    slot   = java_lang_reflect_Constructor::slot(reflected);
  } else {
    mirror = java_lang_reflect_Method::clazz(reflected);
    slot   = java_lang_reflect_Method::slot(reflected);
  }
  Klass* k1 = java_lang_Class::as_Klass(mirror);

  // Make sure class is initialized before handing id's out to methods
  k1->initialize(CHECK_NULL);
  Method* m = InstanceKlass::cast(k1)->method_with_idnum(slot);
  return (m == NULL) ? NULL : m->jmethod_id();  // return NULL if reflected method deleted
JNI_END

JNI_ENTRY(jfieldID, jni_FromReflectedField(JNIEnv *env, jobject field))
  // field is a handle to a java.lang.reflect.Field object
  oop reflected   = JNIHandles::resolve_non_null(field);
  oop mirror      = java_lang_reflect_Field::clazz(reflected);
  Klass* k1       = java_lang_Class::as_Klass(mirror);
  int slot        = java_lang_reflect_Field::slot(reflected);
  int modifiers   = java_lang_reflect_Field::modifiers(reflected);

  // Make sure class is initialized before handing id's out to fields
  k1->initialize(CHECK_NULL);

  // First check if this is a static field
  if (modifiers & JVM_ACC_STATIC) {
    intptr_t offset = InstanceKlass::cast(k1)->field_offset( slot );
    JNIid* id = InstanceKlass::cast(k1)->jni_id_for(offset);
    // A jfieldID for a static field is a JNIid specifying the field holder and the offset within the Klass*
    return jfieldIDWorkaround::to_static_jfieldID(id);
  }

  // The slot is the index of the field description in the field-array
  // The jfieldID is the offset of the field within the object
  // It may also have hash bits for k, if VerifyJNIFields is turned on.
  intptr_t offset = InstanceKlass::cast(k1)->field_offset( slot );
  return jfieldIDWorkaround::to_instance_jfieldID(k1, offset);
JNI_END

JNI_ENTRY(jobject, jni_ToReflectedMethod(JNIEnv *env, jclass cls, jmethodID method_id, jboolean isStatic))
  methodHandle m (THREAD, Method::resolve_jmethod_id(method_id));
  oop reflection_method;
  if (m->is_initializer()) {
    reflection_method = Reflection::new_constructor(m, CHECK_NULL);
  } else {
    reflection_method = Reflection::new_method(m, false, CHECK_NULL);
  }
  return JNIHandles::make_local(env, reflection_method);
JNI_END

JNI_ENTRY(jclass, jni_GetSuperclass(JNIEnv *env, jclass sub))
  jclass obj = NULL;

  oop mirror = JNIHandles::resolve_non_null(sub);
  // primitive classes return NULL
  if (java_lang_Class::is_primitive(mirror)) return NULL;

  // Rules of Class.getSuperClass as implemented by KLass::java_super:
  // arrays return Object
  // interfaces return NULL
  // proper classes return Klass::super()
  Klass* k = java_lang_Class::as_Klass(mirror);
  if (k->is_interface()) return NULL;

  // return mirror for superclass
  Klass* super = k->java_super();
  obj = (super == NULL) ? NULL : (jclass) JNIHandles::make_local(super->java_mirror());
  return obj;
JNI_END

JNI_QUICK_ENTRY(jboolean, jni_IsAssignableFrom(JNIEnv *env, jclass sub, jclass super))
  oop sub_mirror   = JNIHandles::resolve_non_null(sub);
  oop super_mirror = JNIHandles::resolve_non_null(super);
  if (java_lang_Class::is_primitive(sub_mirror) || java_lang_Class::is_primitive(super_mirror)) {
    return oopDesc::equals(sub_mirror, super_mirror);
  }
  Klass* sub_klass   = java_lang_Class::as_Klass(sub_mirror);
  Klass* super_klass = java_lang_Class::as_Klass(super_mirror);
  return sub_klass->is_subtype_of(super_klass) ? JNI_TRUE : JNI_FALSE;
JNI_END

JNI_ENTRY(jint, jni_Throw(JNIEnv *env, jthrowable obj))
  THROW_OOP_(JNIHandles::resolve(obj), JNI_OK);
  ShouldNotReachHere();
  return 0;  // Mute compiler.
JNI_END

JNI_ENTRY(jint, jni_ThrowNew(JNIEnv *env, jclass clazz, const char *message))
  InstanceKlass* k = InstanceKlass::cast(java_lang_Class::as_Klass(JNIHandles::resolve_non_null(clazz)));
  Symbol*  name = k->name();
  Handle class_loader (THREAD,  k->class_loader());
  Handle protection_domain (THREAD, k->protection_domain());
  THROW_MSG_LOADER_(name, (char *)message, class_loader, protection_domain, JNI_OK);
  ShouldNotReachHere();
  return 0;  // Mute compiler.
JNI_END

// JNI functions only transform a pending async exception to a synchronous
// exception in ExceptionOccurred and ExceptionCheck calls, since
// delivering an async exception in other places won't change the native
// code's control flow and would be harmful when native code further calls
// JNI functions with a pending exception. Async exception is also checked
// during the call, so ExceptionOccurred/ExceptionCheck won't return
// false but deliver the async exception at the very end during
// state transition.

static void jni_check_async_exceptions(JavaThread *thread) {
  thread->check_and_handle_async_exceptions();
}

JNI_ENTRY_NO_PRESERVE(jthrowable, jni_ExceptionOccurred(JNIEnv *env))
  jni_check_async_exceptions(thread);
  oop exception = thread->pending_exception();
  return (jthrowable) JNIHandles::make_local(env, exception);
JNI_END

JNI_ENTRY_NO_PRESERVE(void, jni_ExceptionDescribe(JNIEnv *env))
  if (thread->has_pending_exception()) {
    Handle ex(thread, thread->pending_exception());
    thread->clear_pending_exception();
    if (ex->is_a(SystemDictionary::ThreadDeath_klass())) {
      // Don't print anything if we are being killed.
    } else {
      jio_fprintf(defaultStream::error_stream(), "Exception ");
      if (thread != NULL && thread->threadObj() != NULL) {
        ResourceMark rm(THREAD);
        jio_fprintf(defaultStream::error_stream(), "in thread \"%s\" ", thread->get_thread_name());
      }
      if (ex->is_a(SystemDictionary::Throwable_klass())) {
        JavaValue result(T_VOID);
        JavaCalls::call_virtual(&result, ex, SystemDictionary::Throwable_klass(), vmSymbols::printStackTrace_name(), vmSymbols::void_method_signature(), THREAD);
        // If an exception is thrown in the call it gets thrown away. Not much
        // we can do with it. The native code that calls this, does not check
        // for the exception - hence, it might still be in the thread when DestroyVM gets
        // called, potentially causing a few asserts to trigger - since no pending exception
        // is expected.
        CLEAR_PENDING_EXCEPTION;
      } else {
        ResourceMark rm(THREAD);
        jio_fprintf(defaultStream::error_stream(), ". Uncaught exception of type %s.", ex->klass()->external_name());
      }
    }
  }
JNI_END

JNI_QUICK_ENTRY(void, jni_ExceptionClear(JNIEnv *env))
  thread->clear_pending_exception();
JNI_END

JNI_ENTRY(void, jni_FatalError(JNIEnv *env, const char *msg))
  tty->print_cr("FATAL ERROR in native method: %s", msg);
  thread->print_stack();
  os::abort(); // Dump core and abort
JNI_END

JNI_ENTRY(jint, jni_PushLocalFrame(JNIEnv *env, jint capacity))
  //%note jni_11
  if (capacity < 0 || ((MaxJNILocalCapacity > 0) && (capacity > MaxJNILocalCapacity))) {
    return JNI_ERR;
  }
  JNIHandleBlock* old_handles = thread->active_handles();
  JNIHandleBlock* new_handles = JNIHandleBlock::allocate_block(thread);
  new_handles->set_pop_frame_link(old_handles);
  thread->set_active_handles(new_handles);
  return JNI_OK;
JNI_END

JNI_ENTRY(jobject, jni_PopLocalFrame(JNIEnv *env, jobject result))
  //%note jni_11
  Handle result_handle(thread, JNIHandles::resolve(result));
  JNIHandleBlock* old_handles = thread->active_handles();
  JNIHandleBlock* new_handles = old_handles->pop_frame_link();
  if (new_handles != NULL) {
    // As a sanity check we only release the handle blocks if the pop_frame_link is not NULL.
    // This way code will still work if PopLocalFrame is called without a corresponding
    // PushLocalFrame call. Note that we set the pop_frame_link to NULL explicitly, otherwise
    // the release_block call will release the blocks.
    thread->set_active_handles(new_handles);
    old_handles->set_pop_frame_link(NULL);              // clear link we won't release new_handles below
    JNIHandleBlock::release_block(old_handles, thread); // may block
    result = JNIHandles::make_local(thread, result_handle());
  }
  return result;
JNI_END

JNI_ENTRY(jobject, jni_NewGlobalRef(JNIEnv *env, jobject ref))
  Handle ref_handle(thread, JNIHandles::resolve(ref));
  return JNIHandles::make_global(ref_handle);
JNI_END

// Must be JNI_ENTRY (with HandleMark)
JNI_ENTRY_NO_PRESERVE(void, jni_DeleteGlobalRef(JNIEnv *env, jobject ref))
  JNIHandles::destroy_global(ref);
JNI_END

JNI_QUICK_ENTRY(void, jni_DeleteLocalRef(JNIEnv *env, jobject obj))
  JNIHandles::destroy_local(obj);
JNI_END

JNI_QUICK_ENTRY(jboolean, jni_IsSameObject(JNIEnv *env, jobject r1, jobject r2))
  oop a = JNIHandles::resolve(r1);
  oop b = JNIHandles::resolve(r2);
  return oopDesc::equals(a, b) ? JNI_TRUE : JNI_FALSE;
JNI_END

JNI_ENTRY(jobject, jni_NewLocalRef(JNIEnv *env, jobject ref))
  return JNIHandles::make_local(env, JNIHandles::resolve(ref));
JNI_END

JNI_LEAF(jint, jni_EnsureLocalCapacity(JNIEnv *env, jint capacity))
  jint ret;
  if (capacity >= 0 && ((MaxJNILocalCapacity <= 0) || (capacity <= MaxJNILocalCapacity))) {
    ret = JNI_OK;
  } else {
    ret = JNI_ERR;
  }

  return ret;
JNI_END

// Return the Handle Type
JNI_LEAF(jobjectRefType, jni_GetObjectRefType(JNIEnv *env, jobject obj))
  jobjectRefType ret = JNIInvalidRefType;
  if (obj != NULL) {
    ret = JNIHandles::handle_type(thread, obj);
  }

  return ret;
JNI_END

class JNI_ArgumentPusher : public SignatureIterator {
 protected:
  JavaCallArguments*  _arguments;

  virtual void get_bool   () = 0;
  virtual void get_char   () = 0;
  virtual void get_short  () = 0;
  virtual void get_byte   () = 0;
  virtual void get_int    () = 0;
  virtual void get_long   () = 0;
  virtual void get_float  () = 0;
  virtual void get_double () = 0;
  virtual void get_object () = 0;

  JNI_ArgumentPusher(Symbol* signature) : SignatureIterator(signature) {
    this->_return_type = T_ILLEGAL;
    _arguments = NULL;
  }

 public:
  virtual void iterate( uint64_t fingerprint ) = 0;

  void set_java_argument_object(JavaCallArguments *arguments) { _arguments = arguments; }

  inline void do_bool()                     { if (!is_return_type()) get_bool();   }
  inline void do_char()                     { if (!is_return_type()) get_char();   }
  inline void do_short()                    { if (!is_return_type()) get_short();  }
  inline void do_byte()                     { if (!is_return_type()) get_byte();   }
  inline void do_int()                      { if (!is_return_type()) get_int();    }
  inline void do_long()                     { if (!is_return_type()) get_long();   }
  inline void do_float()                    { if (!is_return_type()) get_float();  }
  inline void do_double()                   { if (!is_return_type()) get_double(); }
  inline void do_object(int begin, int end) { if (!is_return_type()) get_object(); }
  inline void do_array(int begin, int end)  { if (!is_return_type()) get_object(); } // do_array uses get_object -- there is no get_array
  inline void do_void()                     { }

  JavaCallArguments* arguments()     { return _arguments; }
  void push_receiver(Handle h)       { _arguments->push_oop(h); }
};

class JNI_ArgumentPusherVaArg : public JNI_ArgumentPusher {
 protected:
  va_list _ap;

  inline void get_bool()   {
    // Normalize boolean arguments from native code by converting 1-255 to JNI_TRUE and
    // 0 to JNI_FALSE.  Boolean return values from native are normalized the same in
    // TemplateInterpreterGenerator::generate_result_handler_for and
    // SharedRuntime::generate_native_wrapper.
    jboolean b = va_arg(_ap, jint);
    _arguments->push_int((jint)(b == 0 ? JNI_FALSE : JNI_TRUE));
  }
  inline void get_char()   { _arguments->push_int(va_arg(_ap, jint)); } // char is coerced to int when using va_arg
  inline void get_short()  { _arguments->push_int(va_arg(_ap, jint)); } // short is coerced to int when using va_arg
  inline void get_byte()   { _arguments->push_int(va_arg(_ap, jint)); } // byte is coerced to int when using va_arg
  inline void get_int()    { _arguments->push_int(va_arg(_ap, jint)); }

  // each of these paths is exercized by the various jck Call[Static,Nonvirtual,][Void,Int,..]Method[A,V,] tests

  inline void get_long()   { _arguments->push_long(va_arg(_ap, jlong)); }
  inline void get_float()  { _arguments->push_float((jfloat)va_arg(_ap, jdouble)); } // float is coerced to double w/ va_arg
  inline void get_double() { _arguments->push_double(va_arg(_ap, jdouble)); }
  inline void get_object() { _arguments->push_jobject(va_arg(_ap, jobject)); }

  inline void set_ap(va_list rap) {
    va_copy(_ap, rap);
  }

 public:
  JNI_ArgumentPusherVaArg(Symbol* signature, va_list rap)
       : JNI_ArgumentPusher(signature) {
    set_ap(rap);
  }
  JNI_ArgumentPusherVaArg(jmethodID method_id, va_list rap)
      : JNI_ArgumentPusher(Method::resolve_jmethod_id(method_id)->signature()) {
    set_ap(rap);
  }

  // Optimized path if we have the bitvector form of signature
  void iterate( uint64_t fingerprint ) {
    if (fingerprint == (uint64_t)CONST64(-1)) {
      SignatureIterator::iterate(); // Must be too many arguments
    } else {
      _return_type = (BasicType)((fingerprint >> static_feature_size) & result_feature_mask);

      fingerprint = fingerprint >> (static_feature_size + result_feature_size);
      while (true) {
        switch (fingerprint & parameter_feature_mask) {
          case bool_parm:   get_bool();   break;
          case char_parm:   get_char();   break;
          case short_parm:  get_short();  break;
          case byte_parm:   get_byte();   break;
          case int_parm:    get_int();    break;
          case obj_parm:    get_object(); break;
          case long_parm:   get_long();   break;
          case float_parm:  get_float();  break;
          case double_parm: get_double(); break;
          case done_parm:
            return;
            break;
          default:
            ShouldNotReachHere();
            break;
        }
        fingerprint >>= parameter_feature_size;
      }
    }
  }
};

class JNI_ArgumentPusherArray : public JNI_ArgumentPusher {
 protected:
  const jvalue *_ap;

  inline void get_bool()   {
    // Normalize boolean arguments from native code by converting 1-255 to JNI_TRUE and
    // 0 to JNI_FALSE.  Boolean return values from native are normalized the same in
    // TemplateInterpreterGenerator::generate_result_handler_for and
    // SharedRuntime::generate_native_wrapper.
    jboolean b = (_ap++)->z;
    _arguments->push_int((jint)(b == 0 ? JNI_FALSE : JNI_TRUE));
  }
  inline void get_char()   { _arguments->push_int((jint)(_ap++)->c); }
  inline void get_short()  { _arguments->push_int((jint)(_ap++)->s); }
  inline void get_byte()   { _arguments->push_int((jint)(_ap++)->b); }
  inline void get_int()    { _arguments->push_int((jint)(_ap++)->i); }

  inline void get_long()   { _arguments->push_long((_ap++)->j); }
  inline void get_float()  { _arguments->push_float((_ap++)->f); }
  inline void get_double() { _arguments->push_double((_ap++)->d); }
  inline void get_object() { _arguments->push_jobject((_ap++)->l); }

  inline void set_ap(const jvalue *rap) { _ap = rap; }

 public:
  JNI_ArgumentPusherArray(Symbol* signature, const jvalue *rap)
       : JNI_ArgumentPusher(signature) {
    set_ap(rap);
  }
  JNI_ArgumentPusherArray(jmethodID method_id, const jvalue *rap)
      : JNI_ArgumentPusher(Method::resolve_jmethod_id(method_id)->signature()) {
    set_ap(rap);
  }

  // Optimized path if we have the bitvector form of signature
  void iterate( uint64_t fingerprint ) {
    if (fingerprint == (uint64_t)CONST64(-1)) {
      SignatureIterator::iterate(); // Must be too many arguments
    } else {
      _return_type = (BasicType)((fingerprint >> static_feature_size) & result_feature_mask);
      fingerprint = fingerprint >> (static_feature_size + result_feature_size);
      while (true) {
        switch (fingerprint & parameter_feature_mask) {
          case bool_parm:   get_bool();   break;
          case char_parm:   get_char();   break;
          case short_parm:  get_short();  break;
          case byte_parm:   get_byte();   break;
          case int_parm:    get_int();    break;
          case obj_parm:    get_object(); break;
          case long_parm:   get_long();   break;
          case float_parm:  get_float();  break;
          case double_parm: get_double(); break;
          case done_parm:
            return;
            break;
          default:
            ShouldNotReachHere();
            break;
        }
        fingerprint >>= parameter_feature_size;
      }
    }
  }
};

enum JNICallType {
  JNI_STATIC,
  JNI_VIRTUAL,
  JNI_NONVIRTUAL
};

static void jni_invoke_static(JNIEnv *env, JavaValue* result, jobject receiver, JNICallType call_type, jmethodID method_id, JNI_ArgumentPusher *args, TRAPS) {
  methodHandle method(THREAD, Method::resolve_jmethod_id(method_id));

  // Create object to hold arguments for the JavaCall, and associate it with
  // the jni parser
  ResourceMark rm(THREAD);
  int number_of_parameters = method->size_of_parameters();
  JavaCallArguments java_args(number_of_parameters);
  args->set_java_argument_object(&java_args);

  // Fill out JavaCallArguments object
  args->iterate( Fingerprinter(method).fingerprint());
  // Initialize result type
  result->set_type(args->get_ret_type());

  // Invoke the method. Result is returned as oop.
  JavaCalls::call(result, method, &java_args, CHECK);

  // Convert result
  if (result->get_type() == T_OBJECT || result->get_type() == T_ARRAY) {
    result->set_jobject(JNIHandles::make_local(env, (oop) result->get_jobject()));
  }
}

static void jni_invoke_nonstatic(JNIEnv *env, JavaValue* result, jobject receiver, JNICallType call_type, jmethodID method_id, JNI_ArgumentPusher *args, TRAPS) {
  oop recv = JNIHandles::resolve(receiver);
  if (recv == NULL) {
    THROW(vmSymbols::java_lang_NullPointerException());
  }
  Handle h_recv(THREAD, recv);

  int number_of_parameters;
  Method* selected_method;
  {
    Method* m = Method::resolve_jmethod_id(method_id);
    number_of_parameters = m->size_of_parameters();
    Klass* holder = m->method_holder();
    if (call_type != JNI_VIRTUAL) {
        selected_method = m;
    } else if (!m->has_itable_index()) {
      // jni_GetMethodID makes sure class is linked and initialized
      // so m should have a valid vtable index.
      int vtbl_index = m->vtable_index();
      if (vtbl_index != Method::nonvirtual_vtable_index) {
        selected_method = h_recv->klass()->method_at_vtable(vtbl_index);
      } else {
        // final method
        selected_method = m;
      }
    } else {
      // interface call
      int itbl_index = m->itable_index();
      Klass* k = h_recv->klass();
      selected_method = InstanceKlass::cast(k)->method_at_itable(holder, itbl_index, CHECK);
    }
  }

  methodHandle method(THREAD, selected_method);

  // Create object to hold arguments for the JavaCall, and associate it with
  // the jni parser
  ResourceMark rm(THREAD);
  JavaCallArguments java_args(number_of_parameters);
  args->set_java_argument_object(&java_args);

  // handle arguments
  args->push_receiver(h_recv); // Push jobject handle

  // Fill out JavaCallArguments object
  args->iterate( Fingerprinter(method).fingerprint());
  // Initialize result type
  result->set_type(args->get_ret_type());

  // Invoke the method. Result is returned as oop.
  JavaCalls::call(result, method, &java_args, CHECK);

  // Convert result
  if (result->get_type() == T_OBJECT || result->get_type() == T_ARRAY) {
    result->set_jobject(JNIHandles::make_local(env, (oop) result->get_jobject()));
  }
}

static instanceOop alloc_object(jclass clazz, TRAPS) {
  Klass* k = java_lang_Class::as_Klass(JNIHandles::resolve_non_null(clazz));
  if (k == NULL) {
    ResourceMark rm(THREAD);
    THROW_(vmSymbols::java_lang_InstantiationException(), NULL);
  }
  k->check_valid_for_instantiation(false, CHECK_NULL);
  k->initialize(CHECK_NULL);
  instanceOop ih = InstanceKlass::cast(k)->allocate_instance(THREAD);
  return ih;
}

JNI_ENTRY(jobject, jni_AllocObject(JNIEnv *env, jclass clazz))
  instanceOop i = alloc_object(clazz, CHECK_NULL);
  return JNIHandles::make_local(env, i);
JNI_END

JNI_ENTRY(jobject, jni_NewObjectA(JNIEnv *env, jclass clazz, jmethodID methodID, const jvalue *args))
  jobject obj = NULL;

  instanceOop i = alloc_object(clazz, CHECK_NULL);
  obj = JNIHandles::make_local(env, i);
  JavaValue jvalue(T_VOID);
  JNI_ArgumentPusherArray ap(methodID, args);
  jni_invoke_nonstatic(env, &jvalue, obj, JNI_NONVIRTUAL, methodID, &ap, CHECK_NULL);
  return obj;
JNI_END

JNI_ENTRY(jobject, jni_NewObjectV(JNIEnv *env, jclass clazz, jmethodID methodID, va_list args))
  jobject obj = NULL;

  instanceOop i = alloc_object(clazz, CHECK_NULL);
  obj = JNIHandles::make_local(env, i);
  JavaValue jvalue(T_VOID);
  JNI_ArgumentPusherVaArg ap(methodID, args);
  jni_invoke_nonstatic(env, &jvalue, obj, JNI_NONVIRTUAL, methodID, &ap, CHECK_NULL);
  return obj;
JNI_END

JNI_ENTRY(jobject, jni_NewObject(JNIEnv *env, jclass clazz, jmethodID methodID, ...))
  jobject obj = NULL;

  instanceOop i = alloc_object(clazz, CHECK_NULL);
  obj = JNIHandles::make_local(env, i);
  va_list args;
  va_start(args, methodID);
  JavaValue jvalue(T_VOID);
  JNI_ArgumentPusherVaArg ap(methodID, args);
  jni_invoke_nonstatic(env, &jvalue, obj, JNI_NONVIRTUAL, methodID, &ap, CHECK_NULL);
  va_end(args);
  return obj;
JNI_END

JNI_ENTRY(jclass, jni_GetObjectClass(JNIEnv *env, jobject obj))
  Klass* k = JNIHandles::resolve_non_null(obj)->klass();
  return (jclass) JNIHandles::make_local(env, k->java_mirror());
JNI_END

JNI_QUICK_ENTRY(jboolean, jni_IsInstanceOf(JNIEnv *env, jobject obj, jclass clazz))
  jboolean ret = JNI_TRUE;
  if (obj != NULL) {
    ret = JNI_FALSE;
    Klass* k = java_lang_Class::as_Klass(JNIHandles::resolve_non_null(clazz));
    if (k != NULL) {
      ret = JNIHandles::resolve_non_null(obj)->is_a(k) ? JNI_TRUE : JNI_FALSE;
    }
  }

  return ret;
JNI_END

static jmethodID get_method_id(JNIEnv *env, jclass clazz, const char *name_str, const char *sig, bool is_static, TRAPS) {
  // %%%% This code should probably just call into a method in the LinkResolver
  //
  // The class should have been loaded (we have an instance of the class
  // passed in) so the method and signature should already be in the symbol
  // table.  If they're not there, the method doesn't exist.
  const char *name_to_probe = (name_str == NULL) ? vmSymbols::object_initializer_name()->as_C_string() : name_str;
  TempNewSymbol name = SymbolTable::probe(name_to_probe, (int)strlen(name_to_probe));
  TempNewSymbol signature = SymbolTable::probe(sig, (int)strlen(sig));

  if (name == NULL || signature == NULL) {
    THROW_MSG_0(vmSymbols::java_lang_NoSuchMethodError(), name_str);
  }

  // Throw a NoSuchMethodError exception if we have an instance of a
  // primitive java.lang.Class
  if (java_lang_Class::is_primitive(JNIHandles::resolve_non_null(clazz))) {
    THROW_MSG_0(vmSymbols::java_lang_NoSuchMethodError(), name_str);
  }

  Klass* klass = java_lang_Class::as_Klass(JNIHandles::resolve_non_null(clazz));

  // Make sure class is linked and initialized before handing id's out to
  // Method*s.
  klass->initialize(CHECK_NULL);

  Method* m;
  if (name == vmSymbols::object_initializer_name() || name == vmSymbols::class_initializer_name()) {
    // Never search superclasses for constructors
    if (klass->is_instance_klass()) {
      m = InstanceKlass::cast(klass)->find_method(name, signature);
    } else {
      m = NULL;
    }
  } else {
    m = klass->lookup_method(name, signature);
    if (m == NULL && klass->is_instance_klass()) {
      m = InstanceKlass::cast(klass)->lookup_method_in_ordered_interfaces(name, signature);
    }
  }
  if (m == NULL || (m->is_static() != is_static)) {
    THROW_MSG_0(vmSymbols::java_lang_NoSuchMethodError(), name_str);
  }
  return m->jmethod_id();
}

JNI_ENTRY(jmethodID, jni_GetMethodID(JNIEnv *env, jclass clazz, const char *name, const char *sig))
  return get_method_id(env, clazz, name, sig, false, thread);
JNI_END

JNI_ENTRY(jmethodID, jni_GetStaticMethodID(JNIEnv *env, jclass clazz, const char *name, const char *sig))
  return get_method_id(env, clazz, name, sig, true, thread);
JNI_END

//
// Calling Methods
//

#define DEFINE_CALLMETHOD(ResultType, Result, Tag) \
\
JNI_ENTRY(ResultType, jni_Call##Result##Method(JNIEnv *env, jobject obj, jmethodID methodID, ...)) \
  va_list args; \
  va_start(args, methodID); \
  JavaValue jvalue(Tag); \
  JNI_ArgumentPusherVaArg ap(methodID, args); \
  jni_invoke_nonstatic(env, &jvalue, obj, JNI_VIRTUAL, methodID, &ap, CHECK_0); \
  va_end(args); \
  return jvalue.get_##ResultType(); \
JNI_END

DEFINE_CALLMETHOD(jboolean, Boolean, T_BOOLEAN)
DEFINE_CALLMETHOD(jbyte,    Byte,    T_BYTE)
DEFINE_CALLMETHOD(jchar,    Char,    T_CHAR)
DEFINE_CALLMETHOD(jshort,   Short,   T_SHORT)
DEFINE_CALLMETHOD(jobject,  Object,  T_OBJECT)
DEFINE_CALLMETHOD(jint,     Int,     T_INT)
DEFINE_CALLMETHOD(jlong,    Long,    T_LONG)
DEFINE_CALLMETHOD(jfloat,   Float,   T_FLOAT)
DEFINE_CALLMETHOD(jdouble,  Double,  T_DOUBLE)

#define DEFINE_CALLMETHODV(ResultType, Result, Tag) \
\
JNI_ENTRY(ResultType, jni_Call##Result##MethodV(JNIEnv *env, jobject obj, jmethodID methodID, va_list args)) \
  JavaValue jvalue(Tag); \
  JNI_ArgumentPusherVaArg ap(methodID, args); \
  jni_invoke_nonstatic(env, &jvalue, obj, JNI_VIRTUAL, methodID, &ap, CHECK_0); \
  return jvalue.get_##ResultType(); \
JNI_END

DEFINE_CALLMETHODV(jboolean, Boolean, T_BOOLEAN)
DEFINE_CALLMETHODV(jbyte,    Byte,    T_BYTE)
DEFINE_CALLMETHODV(jchar,    Char,    T_CHAR)
DEFINE_CALLMETHODV(jshort,   Short,   T_SHORT)
DEFINE_CALLMETHODV(jobject,  Object,  T_OBJECT)
DEFINE_CALLMETHODV(jint,     Int,     T_INT)
DEFINE_CALLMETHODV(jlong,    Long,    T_LONG)
DEFINE_CALLMETHODV(jfloat,   Float,   T_FLOAT)
DEFINE_CALLMETHODV(jdouble,  Double,  T_DOUBLE)

#define DEFINE_CALLMETHODA(ResultType, Result, Tag) \
\
JNI_ENTRY(ResultType, jni_Call##Result##MethodA(JNIEnv *env, jobject obj, jmethodID methodID, const jvalue *args)) \
  JavaValue jvalue(Tag); \
  JNI_ArgumentPusherArray ap(methodID, args); \
  jni_invoke_nonstatic(env, &jvalue, obj, JNI_VIRTUAL, methodID, &ap, CHECK_0); \
  return jvalue.get_##ResultType(); \
JNI_END

DEFINE_CALLMETHODA(jboolean, Boolean, T_BOOLEAN)
DEFINE_CALLMETHODA(jbyte,    Byte,    T_BYTE)
DEFINE_CALLMETHODA(jchar,    Char,    T_CHAR)
DEFINE_CALLMETHODA(jshort,   Short,   T_SHORT)
DEFINE_CALLMETHODA(jobject,  Object,  T_OBJECT)
DEFINE_CALLMETHODA(jint,     Int,     T_INT)
DEFINE_CALLMETHODA(jlong,    Long,    T_LONG)
DEFINE_CALLMETHODA(jfloat,   Float,   T_FLOAT)
DEFINE_CALLMETHODA(jdouble,  Double,  T_DOUBLE)

JNI_ENTRY(void, jni_CallVoidMethod(JNIEnv *env, jobject obj, jmethodID methodID, ...))
  va_list args;
  va_start(args, methodID);
  JavaValue jvalue(T_VOID);
  JNI_ArgumentPusherVaArg ap(methodID, args);
  jni_invoke_nonstatic(env, &jvalue, obj, JNI_VIRTUAL, methodID, &ap, CHECK);
  va_end(args);
JNI_END

JNI_ENTRY(void, jni_CallVoidMethodV(JNIEnv *env, jobject obj, jmethodID methodID, va_list args))
  JavaValue jvalue(T_VOID);
  JNI_ArgumentPusherVaArg ap(methodID, args);
  jni_invoke_nonstatic(env, &jvalue, obj, JNI_VIRTUAL, methodID, &ap, CHECK);
JNI_END

JNI_ENTRY(void, jni_CallVoidMethodA(JNIEnv *env, jobject obj, jmethodID methodID, const jvalue *args))
  JavaValue jvalue(T_VOID);
  JNI_ArgumentPusherArray ap(methodID, args);
  jni_invoke_nonstatic(env, &jvalue, obj, JNI_VIRTUAL, methodID, &ap, CHECK);
JNI_END

#define DEFINE_CALLNONVIRTUALMETHOD(ResultType, Result, Tag) \
\
JNI_ENTRY(ResultType, jni_CallNonvirtual##Result##Method(JNIEnv *env, jobject obj, jclass cls, jmethodID methodID, ...)) \
  va_list args; \
  va_start(args, methodID); \
  JavaValue jvalue(Tag); \
  JNI_ArgumentPusherVaArg ap(methodID, args); \
  jni_invoke_nonstatic(env, &jvalue, obj, JNI_NONVIRTUAL, methodID, &ap, CHECK_0); \
  va_end(args); \
  return jvalue.get_##ResultType(); \
JNI_END

DEFINE_CALLNONVIRTUALMETHOD(jboolean, Boolean, T_BOOLEAN)
DEFINE_CALLNONVIRTUALMETHOD(jbyte,    Byte,    T_BYTE)
DEFINE_CALLNONVIRTUALMETHOD(jchar,    Char,    T_CHAR)
DEFINE_CALLNONVIRTUALMETHOD(jshort,   Short,   T_SHORT)
DEFINE_CALLNONVIRTUALMETHOD(jobject,  Object,  T_OBJECT)
DEFINE_CALLNONVIRTUALMETHOD(jint,     Int,     T_INT)
DEFINE_CALLNONVIRTUALMETHOD(jlong,    Long,    T_LONG)
DEFINE_CALLNONVIRTUALMETHOD(jfloat,   Float,   T_FLOAT)
DEFINE_CALLNONVIRTUALMETHOD(jdouble,  Double,  T_DOUBLE)

#define DEFINE_CALLNONVIRTUALMETHODV(ResultType, Result, Tag) \
\
JNI_ENTRY(ResultType, jni_CallNonvirtual##Result##MethodV(JNIEnv *env, jobject obj, jclass cls, jmethodID methodID, va_list args)) \
  JavaValue jvalue(Tag); \
  JNI_ArgumentPusherVaArg ap(methodID, args); \
  jni_invoke_nonstatic(env, &jvalue, obj, JNI_NONVIRTUAL, methodID, &ap, CHECK_0); \
  return jvalue.get_##ResultType(); \
JNI_END

DEFINE_CALLNONVIRTUALMETHODV(jboolean, Boolean, T_BOOLEAN)
DEFINE_CALLNONVIRTUALMETHODV(jbyte,    Byte,    T_BYTE)
DEFINE_CALLNONVIRTUALMETHODV(jchar,    Char,    T_CHAR)
DEFINE_CALLNONVIRTUALMETHODV(jshort,   Short,   T_SHORT)
DEFINE_CALLNONVIRTUALMETHODV(jobject,  Object,  T_OBJECT)
DEFINE_CALLNONVIRTUALMETHODV(jint,     Int,     T_INT)
DEFINE_CALLNONVIRTUALMETHODV(jlong,    Long,    T_LONG)
DEFINE_CALLNONVIRTUALMETHODV(jfloat,   Float,   T_FLOAT)
DEFINE_CALLNONVIRTUALMETHODV(jdouble,  Double,  T_DOUBLE)

#define DEFINE_CALLNONVIRTUALMETHODA(ResultType, Result, Tag) \
\
JNI_ENTRY(ResultType, jni_CallNonvirtual##Result##MethodA(JNIEnv *env, jobject obj, jclass cls, jmethodID methodID, const jvalue *args)) \
  JavaValue jvalue(Tag); \
  JNI_ArgumentPusherArray ap(methodID, args); \
  jni_invoke_nonstatic(env, &jvalue, obj, JNI_NONVIRTUAL, methodID, &ap, CHECK_0); \
  return jvalue.get_##ResultType(); \
JNI_END

DEFINE_CALLNONVIRTUALMETHODA(jboolean, Boolean, T_BOOLEAN)
DEFINE_CALLNONVIRTUALMETHODA(jbyte,    Byte,    T_BYTE)
DEFINE_CALLNONVIRTUALMETHODA(jchar,    Char,    T_CHAR)
DEFINE_CALLNONVIRTUALMETHODA(jshort,   Short,   T_SHORT)
DEFINE_CALLNONVIRTUALMETHODA(jobject,  Object,  T_OBJECT)
DEFINE_CALLNONVIRTUALMETHODA(jint,     Int,     T_INT)
DEFINE_CALLNONVIRTUALMETHODA(jlong,    Long,    T_LONG)
DEFINE_CALLNONVIRTUALMETHODA(jfloat,   Float,   T_FLOAT)
DEFINE_CALLNONVIRTUALMETHODA(jdouble,  Double,  T_DOUBLE)

JNI_ENTRY(void, jni_CallNonvirtualVoidMethod(JNIEnv *env, jobject obj, jclass cls, jmethodID methodID, ...))
  va_list args;
  va_start(args, methodID);
  JavaValue jvalue(T_VOID);
  JNI_ArgumentPusherVaArg ap(methodID, args);
  jni_invoke_nonstatic(env, &jvalue, obj, JNI_NONVIRTUAL, methodID, &ap, CHECK);
  va_end(args);
JNI_END

JNI_ENTRY(void, jni_CallNonvirtualVoidMethodV(JNIEnv *env, jobject obj, jclass cls, jmethodID methodID, va_list args))
  JavaValue jvalue(T_VOID);
  JNI_ArgumentPusherVaArg ap(methodID, args);
  jni_invoke_nonstatic(env, &jvalue, obj, JNI_NONVIRTUAL, methodID, &ap, CHECK);
JNI_END

JNI_ENTRY(void, jni_CallNonvirtualVoidMethodA(JNIEnv *env, jobject obj, jclass cls, jmethodID methodID, const jvalue *args))
  JavaValue jvalue(T_VOID);
  JNI_ArgumentPusherArray ap(methodID, args);
  jni_invoke_nonstatic(env, &jvalue, obj, JNI_NONVIRTUAL, methodID, &ap, CHECK);
JNI_END

#define DEFINE_CALLSTATICMETHOD(ResultType, Result, Tag) \
\
JNI_ENTRY(ResultType, jni_CallStatic##Result##Method(JNIEnv *env, jclass cls, jmethodID methodID, ...)) \
  va_list args; \
  va_start(args, methodID); \
  JavaValue jvalue(Tag); \
  JNI_ArgumentPusherVaArg ap(methodID, args); \
  jni_invoke_static(env, &jvalue, NULL, JNI_STATIC, methodID, &ap, CHECK_0); \
  va_end(args); \
  return jvalue.get_##ResultType(); \
JNI_END

DEFINE_CALLSTATICMETHOD(jboolean, Boolean, T_BOOLEAN);
DEFINE_CALLSTATICMETHOD(jbyte,    Byte,    T_BYTE);
DEFINE_CALLSTATICMETHOD(jchar,    Char,    T_CHAR);
DEFINE_CALLSTATICMETHOD(jshort,   Short,   T_SHORT);
DEFINE_CALLSTATICMETHOD(jobject,  Object,  T_OBJECT);
DEFINE_CALLSTATICMETHOD(jint,     Int,     T_INT);
DEFINE_CALLSTATICMETHOD(jlong,    Long,    T_LONG);
DEFINE_CALLSTATICMETHOD(jfloat,   Float,   T_FLOAT);
DEFINE_CALLSTATICMETHOD(jdouble,  Double,  T_DOUBLE);

#define DEFINE_CALLSTATICMETHODV(ResultType, Result, Tag) \
\
JNI_ENTRY(ResultType, jni_CallStatic##Result##MethodV(JNIEnv *env, jclass cls, jmethodID methodID, va_list args)) \
  JavaValue jvalue(Tag); \
  JNI_ArgumentPusherVaArg ap(methodID, args); \
  /* Make sure class is initialized before trying to invoke its method */ \
  Klass* k = java_lang_Class::as_Klass(JNIHandles::resolve_non_null(cls)); \
  k->initialize(CHECK_0); \
  jni_invoke_static(env, &jvalue, NULL, JNI_STATIC, methodID, &ap, CHECK_0); \
  va_end(args); \
  return jvalue.get_##ResultType(); \
JNI_END

DEFINE_CALLSTATICMETHODV(jboolean, Boolean, T_BOOLEAN);
DEFINE_CALLSTATICMETHODV(jbyte,    Byte,    T_BYTE);
DEFINE_CALLSTATICMETHODV(jchar,    Char,    T_CHAR);
DEFINE_CALLSTATICMETHODV(jshort,   Short,   T_SHORT);
DEFINE_CALLSTATICMETHODV(jobject,  Object,  T_OBJECT);
DEFINE_CALLSTATICMETHODV(jint,     Int,     T_INT);
DEFINE_CALLSTATICMETHODV(jlong,    Long,    T_LONG);
DEFINE_CALLSTATICMETHODV(jfloat,   Float,   T_FLOAT);
DEFINE_CALLSTATICMETHODV(jdouble,  Double,  T_DOUBLE);

#define DEFINE_CALLSTATICMETHODA(ResultType, Result, Tag) \
\
JNI_ENTRY(ResultType, jni_CallStatic##Result##MethodA(JNIEnv *env, jclass cls, jmethodID methodID, const jvalue *args)) \
  JavaValue jvalue(Tag); \
  JNI_ArgumentPusherArray ap(methodID, args); \
  jni_invoke_static(env, &jvalue, NULL, JNI_STATIC, methodID, &ap, CHECK_0); \
  return jvalue.get_##ResultType(); \
JNI_END

DEFINE_CALLSTATICMETHODA(jboolean, Boolean, T_BOOLEAN);
DEFINE_CALLSTATICMETHODA(jbyte,    Byte,    T_BYTE);
DEFINE_CALLSTATICMETHODA(jchar,    Char,    T_CHAR);
DEFINE_CALLSTATICMETHODA(jshort,   Short,   T_SHORT);
DEFINE_CALLSTATICMETHODA(jobject,  Object,  T_OBJECT);
DEFINE_CALLSTATICMETHODA(jint,     Int,     T_INT);
DEFINE_CALLSTATICMETHODA(jlong,    Long,    T_LONG);
DEFINE_CALLSTATICMETHODA(jfloat,   Float,   T_FLOAT);
DEFINE_CALLSTATICMETHODA(jdouble,  Double,  T_DOUBLE);

JNI_ENTRY(void, jni_CallStaticVoidMethod(JNIEnv *env, jclass cls, jmethodID methodID, ...))
  va_list args;
  va_start(args, methodID);
  JavaValue jvalue(T_VOID);
  JNI_ArgumentPusherVaArg ap(methodID, args);
  jni_invoke_static(env, &jvalue, NULL, JNI_STATIC, methodID, &ap, CHECK);
  va_end(args);
JNI_END

JNI_ENTRY(void, jni_CallStaticVoidMethodV(JNIEnv *env, jclass cls, jmethodID methodID, va_list args))
  JavaValue jvalue(T_VOID);
  JNI_ArgumentPusherVaArg ap(methodID, args);
  jni_invoke_static(env, &jvalue, NULL, JNI_STATIC, methodID, &ap, CHECK);
JNI_END

JNI_ENTRY(void, jni_CallStaticVoidMethodA(JNIEnv *env, jclass cls, jmethodID methodID, const jvalue *args))
  JavaValue jvalue(T_VOID);
  JNI_ArgumentPusherArray ap(methodID, args);
  jni_invoke_static(env, &jvalue, NULL, JNI_STATIC, methodID, &ap, CHECK);
JNI_END

//
// Accessing Fields
//

JNI_ENTRY(jfieldID, jni_GetFieldID(JNIEnv *env, jclass clazz, const char *name, const char *sig))
  // The class should have been loaded (we have an instance of the class
  // passed in) so the field and signature should already be in the symbol
  // table.  If they're not there, the field doesn't exist.
  TempNewSymbol fieldname = SymbolTable::probe(name, (int)strlen(name));
  TempNewSymbol signame = SymbolTable::probe(sig, (int)strlen(sig));
  if (fieldname == NULL || signame == NULL) {
    THROW_MSG_0(vmSymbols::java_lang_NoSuchFieldError(), (char*) name);
  }
  Klass* k = java_lang_Class::as_Klass(JNIHandles::resolve_non_null(clazz));
  // Make sure class is initialized before handing id's out to fields
  k->initialize(CHECK_NULL);

  fieldDescriptor fd;
  if (!k->is_instance_klass() || !InstanceKlass::cast(k)->find_field(fieldname, signame, false, &fd)) {
    THROW_MSG_0(vmSymbols::java_lang_NoSuchFieldError(), (char*) name);
  }

  // A jfieldID for a non-static field is simply the offset of the field within the instanceOop
  // It may also have hash bits for k, if VerifyJNIFields is turned on.
  return jfieldIDWorkaround::to_instance_jfieldID(k, fd.offset());
JNI_END

JNI_ENTRY(jobject, jni_GetObjectField(JNIEnv *env, jobject obj, jfieldID fieldID))
  oop o = JNIHandles::resolve_non_null(obj);
  Klass* k = o->klass();
  int offset = jfieldIDWorkaround::from_instance_jfieldID(k, fieldID);
  oop loaded_obj = HeapAccess<ON_UNKNOWN_OOP_REF>::oop_load_at(o, offset);
  return JNIHandles::make_local(env, loaded_obj);
JNI_END

#define DEFINE_GETFIELD(Return, Fieldname, Result) \
\
JNI_QUICK_ENTRY(Return, jni_Get##Result##Field(JNIEnv *env, jobject obj, jfieldID fieldID)) \
  oop o = JNIHandles::resolve_non_null(obj); \
  Klass* k = o->klass(); \
  int offset = jfieldIDWorkaround::from_instance_jfieldID(k, fieldID); \
  return o->Fieldname##_field(offset); \
JNI_END

DEFINE_GETFIELD(jboolean, bool,   Boolean)
DEFINE_GETFIELD(jbyte,    byte,   Byte)
DEFINE_GETFIELD(jchar,    char,   Char)
DEFINE_GETFIELD(jshort,   short,  Short)
DEFINE_GETFIELD(jint,     int,    Int)
DEFINE_GETFIELD(jlong,    long,   Long)
DEFINE_GETFIELD(jfloat,   float,  Float)
DEFINE_GETFIELD(jdouble,  double, Double)

address jni_GetBooleanField_addr() { return (address)jni_GetBooleanField; }
address jni_GetByteField_addr()    { return (address)jni_GetByteField; }
address jni_GetCharField_addr()    { return (address)jni_GetCharField; }
address jni_GetShortField_addr()   { return (address)jni_GetShortField; }
address jni_GetIntField_addr()     { return (address)jni_GetIntField; }
address jni_GetLongField_addr()    { return (address)jni_GetLongField; }
address jni_GetFloatField_addr()   { return (address)jni_GetFloatField; }
address jni_GetDoubleField_addr()  { return (address)jni_GetDoubleField; }

JNI_QUICK_ENTRY(void, jni_SetObjectField(JNIEnv *env, jobject obj, jfieldID fieldID, jobject value))
  oop o = JNIHandles::resolve_non_null(obj);
  Klass* k = o->klass();
  int offset = jfieldIDWorkaround::from_instance_jfieldID(k, fieldID);
  HeapAccess<ON_UNKNOWN_OOP_REF>::oop_store_at(o, offset, JNIHandles::resolve(value));
JNI_END

#define DEFINE_SETFIELD(Argument, Fieldname, Result, SigType, unionType) \
\
JNI_QUICK_ENTRY(void, jni_Set##Result##Field(JNIEnv *env, jobject obj, jfieldID fieldID, Argument value)) \
  oop o = JNIHandles::resolve_non_null(obj); \
  Klass* k = o->klass(); \
  int offset = jfieldIDWorkaround::from_instance_jfieldID(k, fieldID); \
  if (SigType == 'Z') { value = ((jboolean)value) & 1; } \
  o->Fieldname##_field_put(offset, value); \
JNI_END

DEFINE_SETFIELD(jboolean, bool,   Boolean, 'Z', z)
DEFINE_SETFIELD(jbyte,    byte,   Byte,    'B', b)
DEFINE_SETFIELD(jchar,    char,   Char,    'C', c)
DEFINE_SETFIELD(jshort,   short,  Short,   'S', s)
DEFINE_SETFIELD(jint,     int,    Int,     'I', i)
DEFINE_SETFIELD(jlong,    long,   Long,    'J', j)
DEFINE_SETFIELD(jfloat,   float,  Float,   'F', f)
DEFINE_SETFIELD(jdouble,  double, Double,  'D', d)

JNI_ENTRY(jobject, jni_ToReflectedField(JNIEnv *env, jclass cls, jfieldID fieldID, jboolean isStatic))
  fieldDescriptor fd;
  bool found = false;
  Klass* k = java_lang_Class::as_Klass(JNIHandles::resolve_non_null(cls));

  if (isStatic) {
    // Static field. The fieldID a JNIid specifying the field holder and the offset within the Klass*.
    JNIid* id = jfieldIDWorkaround::from_static_jfieldID(fieldID);
    found = id->find_local_field(&fd);
  } else {
    // Non-static field. The fieldID is really the offset of the field within the instanceOop.
    int offset = jfieldIDWorkaround::from_instance_jfieldID(k, fieldID);
    found = InstanceKlass::cast(k)->find_field_from_offset(offset, false, &fd);
  }
  oop reflected = Reflection::new_field(&fd, CHECK_NULL);
  return JNIHandles::make_local(env, reflected);
JNI_END

//
// Accessing Static Fields
//

JNI_ENTRY(jfieldID, jni_GetStaticFieldID(JNIEnv *env, jclass clazz, const char *name, const char *sig))
  // The class should have been loaded (we have an instance of the class
  // passed in) so the field and signature should already be in the symbol
  // table.  If they're not there, the field doesn't exist.
  TempNewSymbol fieldname = SymbolTable::probe(name, (int)strlen(name));
  TempNewSymbol signame = SymbolTable::probe(sig, (int)strlen(sig));
  if (fieldname == NULL || signame == NULL) {
    THROW_MSG_0(vmSymbols::java_lang_NoSuchFieldError(), (char*) name);
  }
  Klass* k = java_lang_Class::as_Klass(JNIHandles::resolve_non_null(clazz));
  // Make sure class is initialized before handing id's out to static fields
  k->initialize(CHECK_NULL);

  fieldDescriptor fd;
  if (!k->is_instance_klass() || !InstanceKlass::cast(k)->find_field(fieldname, signame, true, &fd)) {
    THROW_MSG_0(vmSymbols::java_lang_NoSuchFieldError(), (char*) name);
  }

  // A jfieldID for a static field is a JNIid specifying the field holder and the offset within the Klass*
  JNIid* id = fd.field_holder()->jni_id_for(fd.offset());

  return jfieldIDWorkaround::to_static_jfieldID(id);
JNI_END

JNI_ENTRY(jobject, jni_GetStaticObjectField(JNIEnv *env, jclass clazz, jfieldID fieldID))
  JNIid* id = jfieldIDWorkaround::from_static_jfieldID(fieldID);
  return JNIHandles::make_local(id->holder()->java_mirror()->obj_field(id->offset()));
JNI_END

#define DEFINE_GETSTATICFIELD(Return, Fieldname, Result) \
\
JNI_ENTRY(Return, jni_GetStatic##Result##Field(JNIEnv *env, jclass clazz, jfieldID fieldID)) \
  JNIid* id = jfieldIDWorkaround::from_static_jfieldID(fieldID); \
  return id->holder()->java_mirror()-> Fieldname##_field (id->offset()); \
JNI_END

DEFINE_GETSTATICFIELD(jboolean, bool,   Boolean)
DEFINE_GETSTATICFIELD(jbyte,    byte,   Byte)
DEFINE_GETSTATICFIELD(jchar,    char,   Char)
DEFINE_GETSTATICFIELD(jshort,   short,  Short)
DEFINE_GETSTATICFIELD(jint,     int,    Int)
DEFINE_GETSTATICFIELD(jlong,    long,   Long)
DEFINE_GETSTATICFIELD(jfloat,   float,  Float)
DEFINE_GETSTATICFIELD(jdouble,  double, Double)

JNI_ENTRY(void, jni_SetStaticObjectField(JNIEnv *env, jclass clazz, jfieldID fieldID, jobject value))
  JNIid* id = jfieldIDWorkaround::from_static_jfieldID(fieldID);
  id->holder()->java_mirror()->obj_field_put(id->offset(), JNIHandles::resolve(value));
JNI_END

#define DEFINE_SETSTATICFIELD(Argument, Fieldname, Result, SigType, unionType) \
\
JNI_ENTRY(void, jni_SetStatic##Result##Field(JNIEnv *env, jclass clazz, jfieldID fieldID, Argument value)) \
  JNIid* id = jfieldIDWorkaround::from_static_jfieldID(fieldID); \
  if (SigType == 'Z') { value = ((jboolean)value) & 1; } \
  id->holder()->java_mirror()-> Fieldname##_field_put (id->offset(), value); \
JNI_END

DEFINE_SETSTATICFIELD(jboolean, bool,   Boolean, 'Z', z)
DEFINE_SETSTATICFIELD(jbyte,    byte,   Byte,    'B', b)
DEFINE_SETSTATICFIELD(jchar,    char,   Char,    'C', c)
DEFINE_SETSTATICFIELD(jshort,   short,  Short,   'S', s)
DEFINE_SETSTATICFIELD(jint,     int,    Int,     'I', i)
DEFINE_SETSTATICFIELD(jlong,    long,   Long,    'J', j)
DEFINE_SETSTATICFIELD(jfloat,   float,  Float,   'F', f)
DEFINE_SETSTATICFIELD(jdouble,  double, Double,  'D', d)

//
// String Operations
//

// Unicode Interface

JNI_ENTRY(jstring, jni_NewString(JNIEnv *env, const jchar *unicodeChars, jsize len))
  oop string=java_lang_String::create_oop_from_unicode((jchar*) unicodeChars, len, CHECK_NULL);
  return (jstring) JNIHandles::make_local(env, string);
JNI_END

JNI_QUICK_ENTRY(jsize, jni_GetStringLength(JNIEnv *env, jstring string))
  jsize ret = 0;
  oop s = JNIHandles::resolve_non_null(string);
  if (java_lang_String::value(s) != NULL) {
    ret = java_lang_String::length(s);
  }
  return ret;
JNI_END

JNI_QUICK_ENTRY(const jchar*, jni_GetStringChars(JNIEnv *env, jstring string, jboolean *isCopy))
  jchar* buf = NULL;
  oop s = JNIHandles::resolve_non_null(string);
  typeArrayOop s_value = java_lang_String::value(s);
  if (s_value != NULL) {
    int s_len = java_lang_String::length(s);
    bool is_latin1 = java_lang_String::is_latin1(s);
    buf = NEW_C_HEAP_ARRAY_RETURN_NULL(jchar, s_len + 1, mtInternal);  // add one for zero termination
    /* JNI Specification states return NULL on OOM */
    if (buf != NULL) {
      if (s_len > 0) {
        if (!is_latin1) {
          ArrayAccess<>::arraycopy_to_native(s_value, (size_t) typeArrayOopDesc::element_offset<jchar>(0), buf, s_len);
        } else {
          for (int i = 0; i < s_len; i++) {
            buf[i] = ((jchar) s_value->byte_at(i)) & 0xff;
          }
        }
      }
      buf[s_len] = 0;
      //%note jni_5
      if (isCopy != NULL) {
        *isCopy = JNI_TRUE;
      }
    }
  }
  return buf;
JNI_END

JNI_QUICK_ENTRY(void, jni_ReleaseStringChars(JNIEnv *env, jstring str, const jchar *chars))
  //%note jni_6
  if (chars != NULL) {
    // Since String objects are supposed to be immutable, don't copy any
    // new data back.  A bad user will have to go after the char array.
    FreeHeap((void*) chars);
  }
JNI_END

// UTF Interface

JNI_ENTRY(jstring, jni_NewStringUTF(JNIEnv *env, const char *bytes))
  oop result = java_lang_String::create_oop_from_str((char*) bytes, CHECK_NULL);
  return (jstring) JNIHandles::make_local(env, result);
JNI_END

JNI_ENTRY(jsize, jni_GetStringUTFLength(JNIEnv *env, jstring string))
  jsize ret = 0;
  oop java_string = JNIHandles::resolve_non_null(string);
  if (java_lang_String::value(java_string) != NULL) {
    ret = java_lang_String::utf8_length(java_string);
  }
  return ret;
JNI_END

JNI_ENTRY(const char*, jni_GetStringUTFChars(JNIEnv *env, jstring string, jboolean *isCopy))
  char* result = NULL;
  oop java_string = JNIHandles::resolve_non_null(string);
  if (java_lang_String::value(java_string) != NULL) {
    size_t length = java_lang_String::utf8_length(java_string);
    /* JNI Specification states return NULL on OOM */
    result = AllocateHeap(length + 1, mtInternal, AllocFailStrategy::RETURN_NULL);
    if (result != NULL) {
      java_lang_String::as_utf8_string(java_string, result, (int) length + 1);
      if (isCopy != NULL) {
        *isCopy = JNI_TRUE;
      }
    }
  }
  return result;
JNI_END

JNI_LEAF(void, jni_ReleaseStringUTFChars(JNIEnv *env, jstring str, const char *chars))
  if (chars != NULL) {
    FreeHeap((char*) chars);
  }
JNI_END

JNI_QUICK_ENTRY(jsize, jni_GetArrayLength(JNIEnv *env, jarray array))
  arrayOop a = arrayOop(JNIHandles::resolve_non_null(array));
  return a->length();
JNI_END

//
// Object Array Operations
//

JNI_ENTRY(jobjectArray, jni_NewObjectArray(JNIEnv *env, jsize length, jclass elementClass, jobject initialElement))
  Klass* ek = java_lang_Class::as_Klass(JNIHandles::resolve_non_null(elementClass));
  Klass* ak = ek->array_klass(CHECK_NULL);
  ObjArrayKlass::cast(ak)->initialize(CHECK_NULL);
  objArrayOop result = ObjArrayKlass::cast(ak)->allocate(length, CHECK_NULL);
  oop initial_value = JNIHandles::resolve(initialElement);
  if (initial_value != NULL) {  // array already initialized with NULL
    for (int index = 0; index < length; index++) {
      result->obj_at_put(index, initial_value);
    }
  }
  return (jobjectArray) JNIHandles::make_local(env, result);
JNI_END

JNI_ENTRY(jobject, jni_GetObjectArrayElement(JNIEnv *env, jobjectArray array, jsize index))
  objArrayOop a = objArrayOop(JNIHandles::resolve_non_null(array));
  if (a->is_within_bounds(index)) {
    return JNIHandles::make_local(env, a->obj_at(index));
  } else {
    ResourceMark rm(THREAD);
    stringStream ss;
    ss.print("Index %d out of bounds for length %d", index, a->length());
    THROW_MSG_0(vmSymbols::java_lang_ArrayIndexOutOfBoundsException(), ss.as_string());
  }
JNI_END

JNI_ENTRY(void, jni_SetObjectArrayElement(JNIEnv *env, jobjectArray array, jsize index, jobject value))
  objArrayOop a = objArrayOop(JNIHandles::resolve_non_null(array));
  oop v = JNIHandles::resolve(value);
  if (a->is_within_bounds(index)) {
    if (v == NULL || v->is_a(ObjArrayKlass::cast(a->klass())->element_klass())) {
      a->obj_at_put(index, v);
    } else {
      ResourceMark rm(THREAD);
      stringStream ss;
      Klass *bottom_kl = ObjArrayKlass::cast(a->klass())->bottom_klass();
      ss.print("type mismatch: can not store %s to %s[%d]",
               v->klass()->external_name(),
               bottom_kl->is_typeArray_klass() ? type2name_tab[ArrayKlass::cast(bottom_kl)->element_type()] : bottom_kl->external_name(),
               index);
      for (int dims = ArrayKlass::cast(a->klass())->dimension(); dims > 1; --dims) {
        ss.print("[]");
      }
      THROW_MSG(vmSymbols::java_lang_ArrayStoreException(), ss.as_string());
    }
  } else {
    ResourceMark rm(THREAD);
    stringStream ss;
    ss.print("Index %d out of bounds for length %d", index, a->length());
    THROW_MSG(vmSymbols::java_lang_ArrayIndexOutOfBoundsException(), ss.as_string());
  }
JNI_END

#define DEFINE_NEWSCALARARRAY(Return, Allocator, Result) \
\
JNI_ENTRY(Return, jni_New##Result##Array(JNIEnv *env, jsize len)) \
  oop obj= oopFactory::Allocator(len, CHECK_0); \
  return (Return) JNIHandles::make_local(env, obj); \
JNI_END

DEFINE_NEWSCALARARRAY(jbooleanArray, new_boolArray,   Boolean)
DEFINE_NEWSCALARARRAY(jbyteArray,    new_byteArray,   Byte)
DEFINE_NEWSCALARARRAY(jshortArray,   new_shortArray,  Short)
DEFINE_NEWSCALARARRAY(jcharArray,    new_charArray,   Char)
DEFINE_NEWSCALARARRAY(jintArray,     new_intArray,    Int)
DEFINE_NEWSCALARARRAY(jlongArray,    new_longArray,   Long)
DEFINE_NEWSCALARARRAY(jfloatArray,   new_singleArray, Float)
DEFINE_NEWSCALARARRAY(jdoubleArray,  new_doubleArray, Double)

// Return an address which will fault if the caller writes to it.

static char* get_bad_address() {
  static char* bad_address = NULL;
  if (bad_address == NULL) {
    size_t size = os::vm_allocation_granularity();
    bad_address = os::reserve_memory(size);
    if (bad_address != NULL) {
      os::protect_memory(bad_address, size, os::MEM_PROT_READ, /*is_committed*/false);
    }
  }
  return bad_address;
}

#define DEFINE_GETSCALARARRAYELEMENTS(ElementTag, ElementType, Result, Tag) \
\
JNI_QUICK_ENTRY(ElementType*, jni_Get##Result##ArrayElements(JNIEnv *env, ElementType##Array array, jboolean *isCopy)) \
  /* allocate an chunk of memory in c land */ \
  typeArrayOop a = typeArrayOop(JNIHandles::resolve_non_null(array)); \
  ElementType* result; \
  int len = a->length(); \
  if (len == 0) { \
    /* Empty array: legal but useless, can't return NULL. \
     * Return a pointer to something useless. \
     * Avoid asserts in typeArrayOop. */ \
    result = (ElementType*)get_bad_address(); \
  } else { \
    /* JNI Specification states return NULL on OOM */ \
    result = NEW_C_HEAP_ARRAY_RETURN_NULL(ElementType, len, mtInternal); \
    if (result != NULL) { \
      /* copy the array to the c chunk */ \
      ArrayAccess<>::arraycopy_to_native(a, typeArrayOopDesc::element_offset<ElementType>(0), result, len); \
      if (isCopy) { \
        *isCopy = JNI_TRUE; \
      } \
    } \
  } \
  return result; \
JNI_END

DEFINE_GETSCALARARRAYELEMENTS(T_BOOLEAN, jboolean, Boolean, bool)
DEFINE_GETSCALARARRAYELEMENTS(T_BYTE,    jbyte,    Byte,    byte)
DEFINE_GETSCALARARRAYELEMENTS(T_SHORT,   jshort,   Short,   short)
DEFINE_GETSCALARARRAYELEMENTS(T_CHAR,    jchar,    Char,    char)
DEFINE_GETSCALARARRAYELEMENTS(T_INT,     jint,     Int,     int)
DEFINE_GETSCALARARRAYELEMENTS(T_LONG,    jlong,    Long,    long)
DEFINE_GETSCALARARRAYELEMENTS(T_FLOAT,   jfloat,   Float,   float)
DEFINE_GETSCALARARRAYELEMENTS(T_DOUBLE,  jdouble,  Double,  double)

#define DEFINE_RELEASESCALARARRAYELEMENTS(ElementTag, ElementType, Result, Tag); \
\
JNI_QUICK_ENTRY(void, jni_Release##Result##ArrayElements(JNIEnv *env, ElementType##Array array, ElementType *buf, jint mode)) \
  typeArrayOop a = typeArrayOop(JNIHandles::resolve_non_null(array)); \
  int len = a->length(); \
  if (len != 0) {   /* Empty array:  nothing to free or copy. */ \
    if ((mode == 0) || (mode == JNI_COMMIT)) { \
      ArrayAccess<>::arraycopy_from_native(buf, a, typeArrayOopDesc::element_offset<ElementType>(0), len); \
    } \
    if ((mode == 0) || (mode == JNI_ABORT)) { \
      FreeHeap(buf); \
    } \
  } \
JNI_END

DEFINE_RELEASESCALARARRAYELEMENTS(T_BOOLEAN, jboolean, Boolean, bool)
DEFINE_RELEASESCALARARRAYELEMENTS(T_BYTE,    jbyte,    Byte,    byte)
DEFINE_RELEASESCALARARRAYELEMENTS(T_SHORT,   jshort,   Short,   short)
DEFINE_RELEASESCALARARRAYELEMENTS(T_CHAR,    jchar,    Char,    char)
DEFINE_RELEASESCALARARRAYELEMENTS(T_INT,     jint,     Int,     int)
DEFINE_RELEASESCALARARRAYELEMENTS(T_LONG,    jlong,    Long,    long)
DEFINE_RELEASESCALARARRAYELEMENTS(T_FLOAT,   jfloat,   Float,   float)
DEFINE_RELEASESCALARARRAYELEMENTS(T_DOUBLE,  jdouble,  Double,  double)

static void check_bounds(jsize start, jsize copy_len, jsize array_len, TRAPS) {
  ResourceMark rm(THREAD);
  if (copy_len < 0) {
    stringStream ss;
    ss.print("Length %d is negative", copy_len);
    THROW_MSG(vmSymbols::java_lang_ArrayIndexOutOfBoundsException(), ss.as_string());
  } else if (start < 0 || (start > array_len - copy_len)) {
    stringStream ss;
    ss.print("Array region %d.." INT64_FORMAT " out of bounds for length %d", start, (int64_t)start+(int64_t)copy_len, array_len);
    THROW_MSG(vmSymbols::java_lang_ArrayIndexOutOfBoundsException(), ss.as_string());
  }
}

#define DEFINE_GETSCALARARRAYREGION(ElementTag, ElementType, Result, Tag); \
\
JNI_ENTRY(void, jni_Get##Result##ArrayRegion(JNIEnv *env, ElementType##Array array, jsize start, jsize len, ElementType *buf)) \
  typeArrayOop src = typeArrayOop(JNIHandles::resolve_non_null(array)); \
  check_bounds(start, len, src->length(), CHECK); \
  if (len > 0) { \
    ArrayAccess<>::arraycopy_to_native(src, typeArrayOopDesc::element_offset<ElementType>(start), buf, len); \
  } \
JNI_END

DEFINE_GETSCALARARRAYREGION(T_BOOLEAN, jboolean,Boolean, bool);
DEFINE_GETSCALARARRAYREGION(T_BYTE,    jbyte,   Byte,    byte);
DEFINE_GETSCALARARRAYREGION(T_SHORT,   jshort,  Short,   short);
DEFINE_GETSCALARARRAYREGION(T_CHAR,    jchar,   Char,    char);
DEFINE_GETSCALARARRAYREGION(T_INT,     jint,    Int,     int);
DEFINE_GETSCALARARRAYREGION(T_LONG,    jlong,   Long,    long);
DEFINE_GETSCALARARRAYREGION(T_FLOAT,   jfloat,  Float,   float);
DEFINE_GETSCALARARRAYREGION(T_DOUBLE,  jdouble, Double,  double);

#define DEFINE_SETSCALARARRAYREGION(ElementTag, ElementType, Result, Tag); \
\
JNI_ENTRY(void, jni_Set##Result##ArrayRegion(JNIEnv *env, ElementType##Array array, jsize start, jsize len, const ElementType *buf)) \
  typeArrayOop dst = typeArrayOop(JNIHandles::resolve_non_null(array)); \
  check_bounds(start, len, dst->length(), CHECK); \
  if (len > 0) { \
    ArrayAccess<>::arraycopy_from_native(buf, dst, typeArrayOopDesc::element_offset<ElementType>(start), len); \
  } \
JNI_END

DEFINE_SETSCALARARRAYREGION(T_BOOLEAN, jboolean, Boolean, bool)
DEFINE_SETSCALARARRAYREGION(T_BYTE,    jbyte,    Byte,    byte)
DEFINE_SETSCALARARRAYREGION(T_SHORT,   jshort,   Short,   short)
DEFINE_SETSCALARARRAYREGION(T_CHAR,    jchar,    Char,    char)
DEFINE_SETSCALARARRAYREGION(T_INT,     jint,     Int,     int)
DEFINE_SETSCALARARRAYREGION(T_LONG,    jlong,    Long,    long)
DEFINE_SETSCALARARRAYREGION(T_FLOAT,   jfloat,   Float,   float)
DEFINE_SETSCALARARRAYREGION(T_DOUBLE,  jdouble,  Double,  double)

//
// Interception of natives
//

// The RegisterNatives call being attempted tried to register with a method that
// is not native.  Ask JVM TI what prefixes have been specified.  Then check
// to see if the native method is now wrapped with the prefixes.  See the
// SetNativeMethodPrefix(es) functions in the JVM TI Spec for details.
static Method* find_prefixed_native(Klass* k, Symbol* name, Symbol* signature, TRAPS) {
  return NULL; // not found
}

static bool register_native(Klass* k, Symbol* name, Symbol* signature, address entry, TRAPS) {
  Method* method = k->lookup_method(name, signature);
  if (method == NULL) {
    ResourceMark rm;
    stringStream st;
    st.print("Method %s name or signature does not match",
             Method::name_and_sig_as_C_string(k, name, signature));
    THROW_MSG_(vmSymbols::java_lang_NoSuchMethodError(), st.as_string(), false);
  }
  if (!method->is_native()) {
    // trying to register to a non-native method, see if a JVM TI agent has added prefix(es)
    method = find_prefixed_native(k, name, signature, THREAD);
    if (method == NULL) {
      ResourceMark rm;
      stringStream st;
      st.print("Method %s is not declared as native", Method::name_and_sig_as_C_string(k, name, signature));
      THROW_MSG_(vmSymbols::java_lang_NoSuchMethodError(), st.as_string(), false);
    }
  }

  if (entry != NULL) {
    method->set_native_function(entry,
      Method::native_bind_event_is_interesting);
  } else {
    method->clear_native_function();
  }
  return true;
}

JNI_ENTRY(jint, jni_RegisterNatives(JNIEnv *env, jclass clazz, const JNINativeMethod *methods, jint nMethods))
  jint ret = 0;

  Klass* k = java_lang_Class::as_Klass(JNIHandles::resolve_non_null(clazz));

  for (int index = 0; index < nMethods; index++) {
    const char* meth_name = methods[index].name;
    const char* meth_sig = methods[index].signature;
    int meth_name_len = (int)strlen(meth_name);

    // The class should have been loaded (we have an instance of the class
    // passed in) so the method and signature should already be in the symbol
    // table.  If they're not there, the method doesn't exist.
    TempNewSymbol  name = SymbolTable::probe(meth_name, meth_name_len);
    TempNewSymbol  signature = SymbolTable::probe(meth_sig, (int)strlen(meth_sig));

    if (name == NULL || signature == NULL) {
      ResourceMark rm;
      stringStream st;
      st.print("Method %s.%s%s not found", k->external_name(), meth_name, meth_sig);
      // Must return negative value on failure
      THROW_MSG_(vmSymbols::java_lang_NoSuchMethodError(), st.as_string(), -1);
    }

    bool res = register_native(k, name, signature, (address) methods[index].fnPtr, THREAD);
    if (!res) {
      ret = -1;
      break;
    }
  }
  return ret;
JNI_END

JNI_ENTRY(jint, jni_UnregisterNatives(JNIEnv *env, jclass clazz))
  Klass* k   = java_lang_Class::as_Klass(JNIHandles::resolve_non_null(clazz));
  //%note jni_2
  if (k->is_instance_klass()) {
    for (int index = 0; index < InstanceKlass::cast(k)->methods()->length(); index++) {
      Method* m = InstanceKlass::cast(k)->methods()->at(index);
      if (m->is_native()) {
        m->clear_native_function();
        m->set_signature_handler(NULL);
      }
    }
  }
  return 0;
JNI_END

//
// Monitor functions
//

JNI_ENTRY(jint, jni_MonitorEnter(JNIEnv *env, jobject jobj))
  // If the object is null, we can't do anything with it
  if (jobj == NULL) {
    THROW_(vmSymbols::java_lang_NullPointerException(), JNI_ERR);
  }

  Handle obj(thread, JNIHandles::resolve_non_null(jobj));
  ObjectSynchronizer::jni_enter(obj, CHECK_(JNI_ERR));
  return JNI_OK;
JNI_END

JNI_ENTRY(jint, jni_MonitorExit(JNIEnv *env, jobject jobj))
  // Don't do anything with a null object
  if (jobj == NULL) {
    THROW_(vmSymbols::java_lang_NullPointerException(), JNI_ERR);
  }

  Handle obj(THREAD, JNIHandles::resolve_non_null(jobj));
  ObjectSynchronizer::jni_exit(obj(), CHECK_(JNI_ERR));

  return JNI_OK;
JNI_END

//
// Extensions
//

JNI_ENTRY(void, jni_GetStringRegion(JNIEnv *env, jstring string, jsize start, jsize len, jchar *buf))
  oop s = JNIHandles::resolve_non_null(string);
  int s_len = java_lang_String::length(s);
  if (start < 0 || len < 0 || start > s_len - len) {
    THROW(vmSymbols::java_lang_StringIndexOutOfBoundsException());
  } else {
    if (len > 0) {
      typeArrayOop s_value = java_lang_String::value(s);
      bool is_latin1 = java_lang_String::is_latin1(s);
      if (!is_latin1) {
        ArrayAccess<>::arraycopy_to_native(s_value, typeArrayOopDesc::element_offset<jchar>(start), buf, len);
      } else {
        for (int i = 0; i < len; i++) {
          buf[i] = ((jchar) s_value->byte_at(i + start)) & 0xff;
        }
      }
    }
  }
JNI_END

JNI_ENTRY(void, jni_GetStringUTFRegion(JNIEnv *env, jstring string, jsize start, jsize len, char *buf))
  oop s = JNIHandles::resolve_non_null(string);
  int s_len = java_lang_String::length(s);
  if (start < 0 || len < 0 || start > s_len - len) {
    THROW(vmSymbols::java_lang_StringIndexOutOfBoundsException());
  } else {
    //%note jni_7
    if (len > 0) {
      // Assume the buffer is large enough as the JNI spec. does not require user error checking
      java_lang_String::as_utf8_string(s, start, len, buf, INT_MAX);
      // as_utf8_string null-terminates the result string
    } else {
      // JDK null-terminates the buffer even in len is zero
      if (buf != NULL) {
        buf[0] = 0;
      }
    }
  }
JNI_END

static oop lock_gc_or_pin_object(JavaThread* thread, jobject obj) {
  if (Universe::heap()->supports_object_pinning()) {
    const oop o = JNIHandles::resolve_non_null(obj);
    return Universe::heap()->pin_object(thread, o);
  } else {
    GCLocker::lock_critical(thread);
    return JNIHandles::resolve_non_null(obj);
  }
}

static void unlock_gc_or_unpin_object(JavaThread* thread, jobject obj) {
  if (Universe::heap()->supports_object_pinning()) {
    const oop o = JNIHandles::resolve_non_null(obj);
    return Universe::heap()->unpin_object(thread, o);
  } else {
    GCLocker::unlock_critical(thread);
  }
}

JNI_ENTRY(void*, jni_GetPrimitiveArrayCritical(JNIEnv *env, jarray array, jboolean *isCopy))
  if (isCopy != NULL) {
    *isCopy = JNI_FALSE;
  }
  oop a = lock_gc_or_pin_object(thread, array);
  BasicType type;
  if (a->is_objArray()) {
    type = T_OBJECT;
  } else {
    type = TypeArrayKlass::cast(a->klass())->element_type();
  }
  return arrayOop(a)->base(type);
JNI_END

JNI_ENTRY(void, jni_ReleasePrimitiveArrayCritical(JNIEnv *env, jarray array, void *carray, jint mode))
  unlock_gc_or_unpin_object(thread, array);
JNI_END

JNI_ENTRY(const jchar*, jni_GetStringCritical(JNIEnv *env, jstring string, jboolean *isCopy))
  oop s = lock_gc_or_pin_object(thread, string);
  typeArrayOop s_value = java_lang_String::value(s);
  bool is_latin1 = java_lang_String::is_latin1(s);
  if (isCopy != NULL) {
    *isCopy = is_latin1 ? JNI_TRUE : JNI_FALSE;
  }
  jchar* ret;
  if (!is_latin1) {
    ret = (jchar*) s_value->base(T_CHAR);
  } else {
    // Inflate latin1 encoded string to UTF16
    int s_len = java_lang_String::length(s);
    ret = NEW_C_HEAP_ARRAY_RETURN_NULL(jchar, s_len + 1, mtInternal);  // add one for zero termination
    /* JNI Specification states return NULL on OOM */
    if (ret != NULL) {
      for (int i = 0; i < s_len; i++) {
        ret[i] = ((jchar) s_value->byte_at(i)) & 0xff;
      }
      ret[s_len] = 0;
    }
  }
  return ret;
JNI_END

JNI_ENTRY(void, jni_ReleaseStringCritical(JNIEnv *env, jstring str, const jchar *chars))
  // The str and chars arguments are ignored for UTF16 strings
  oop s = JNIHandles::resolve_non_null(str);
  bool is_latin1 = java_lang_String::is_latin1(s);
  if (is_latin1) {
    // For latin1 string, free jchar array allocated by earlier call to GetStringCritical.
    // This assumes that ReleaseStringCritical bookends GetStringCritical.
    FREE_C_HEAP_ARRAY(jchar, chars);
  }
  unlock_gc_or_unpin_object(thread, str);
JNI_END

JNI_ENTRY(jweak, jni_NewWeakGlobalRef(JNIEnv *env, jobject ref))
  Handle ref_handle(thread, JNIHandles::resolve(ref));
  return JNIHandles::make_weak_global(ref_handle);
JNI_END

// Must be JNI_ENTRY (with HandleMark)
JNI_ENTRY(void, jni_DeleteWeakGlobalRef(JNIEnv *env, jweak ref))
  JNIHandles::destroy_weak_global(ref);
JNI_END

JNI_QUICK_ENTRY(jboolean, jni_ExceptionCheck(JNIEnv *env))
  jni_check_async_exceptions(thread);
  return (thread->has_pending_exception()) ? JNI_TRUE : JNI_FALSE;
JNI_END

// Initialization state for three routines below relating to
// java.nio.DirectBuffers
static          int directBufferSupportInitializeStarted = 0;
static volatile int directBufferSupportInitializeEnded   = 0;
static volatile int directBufferSupportInitializeFailed  = 0;
static jclass    bufferClass                 = NULL;
static jclass    directBufferClass           = NULL;
static jclass    directByteBufferClass       = NULL;
static jmethodID directByteBufferConstructor = NULL;
static jfieldID  directBufferAddressField    = NULL;
static jfieldID  bufferCapacityField         = NULL;

static jclass lookupOne(JNIEnv* env, const char* name, TRAPS) {
  Handle loader;            // null (bootstrap) loader
  Handle protection_domain; // null protection domain

  TempNewSymbol sym = SymbolTable::new_symbol(name, CHECK_NULL);
  jclass result =  find_class_from_class_loader(env, sym, true, loader, protection_domain, true, CHECK_NULL);

  return result;
}

// These lookups are done with the NULL (bootstrap) ClassLoader to
// circumvent any security checks that would be done by jni_FindClass.
JNI_ENTRY(bool, lookupDirectBufferClasses(JNIEnv* env))
{
  if ((bufferClass           = lookupOne(env, "java/nio/Buffer", thread))           == NULL) { return false; }
  if ((directBufferClass     = lookupOne(env, "sun/nio/ch/DirectBuffer", thread))   == NULL) { return false; }
  if ((directByteBufferClass = lookupOne(env, "java/nio/DirectByteBuffer", thread)) == NULL) { return false; }
  return true;
}
JNI_END

static bool initializeDirectBufferSupport(JNIEnv* env, JavaThread* thread) {
  if (directBufferSupportInitializeFailed) {
    return false;
  }

  if (Atomic::cmpxchg(1, &directBufferSupportInitializeStarted, 0) == 0) {
    if (!lookupDirectBufferClasses(env)) {
      directBufferSupportInitializeFailed = 1;
      return false;
    }

    // Make global references for these
    bufferClass           = (jclass) env->NewGlobalRef(bufferClass);
    directBufferClass     = (jclass) env->NewGlobalRef(directBufferClass);
    directByteBufferClass = (jclass) env->NewGlobalRef(directByteBufferClass);

    // Get needed field and method IDs
    directByteBufferConstructor = env->GetMethodID(directByteBufferClass, "<init>", "(JI)V");
    if (env->ExceptionCheck()) {
      env->ExceptionClear();
      directBufferSupportInitializeFailed = 1;
      return false;
    }
    directBufferAddressField = env->GetFieldID(bufferClass, "address", "J");
    if (env->ExceptionCheck()) {
      env->ExceptionClear();
      directBufferSupportInitializeFailed = 1;
      return false;
    }
    bufferCapacityField = env->GetFieldID(bufferClass, "capacity", "I");
    if (env->ExceptionCheck()) {
      env->ExceptionClear();
      directBufferSupportInitializeFailed = 1;
      return false;
    }

    if ((directByteBufferConstructor == NULL) || (directBufferAddressField == NULL) || (bufferCapacityField == NULL)) {
      directBufferSupportInitializeFailed = 1;
      return false;
    }

    directBufferSupportInitializeEnded = 1;
  } else {
    while (!directBufferSupportInitializeEnded && !directBufferSupportInitializeFailed) {
      os::naked_yield();
    }
  }

  return !directBufferSupportInitializeFailed;
}

extern "C" jobject JNICALL jni_NewDirectByteBuffer(JNIEnv *env, void* address, jlong capacity)
{
  // thread_from_jni_environment() will block if VM is gone.
  JavaThread* thread = JavaThread::thread_from_jni_environment(env);

  if (!directBufferSupportInitializeEnded) {
    if (!initializeDirectBufferSupport(env, thread)) {
      return NULL;
    }
  }

  // Being paranoid about accidental sign extension on address
  jlong addr = (jlong) ((uintptr_t) address);
  // NOTE that package-private DirectByteBuffer constructor currently
  // takes int capacity
  jint cap = (jint) capacity;
  return env->NewObject(directByteBufferClass, directByteBufferConstructor, addr, cap);
}

extern "C" void* JNICALL jni_GetDirectBufferAddress(JNIEnv *env, jobject buf) {
  // thread_from_jni_environment() will block if VM is gone.
  JavaThread* thread = JavaThread::thread_from_jni_environment(env);

  if (!directBufferSupportInitializeEnded) {
    if (!initializeDirectBufferSupport(env, thread)) {
      return 0;
    }
  }

  if ((buf != NULL) && (!env->IsInstanceOf(buf, directBufferClass))) {
    return 0;
  }

  return (void*)(intptr_t)env->GetLongField(buf, directBufferAddressField);
}

extern "C" jlong JNICALL jni_GetDirectBufferCapacity(JNIEnv *env, jobject buf) {
  // thread_from_jni_environment() will block if VM is gone.
  JavaThread* thread = JavaThread::thread_from_jni_environment(env);

  if (!directBufferSupportInitializeEnded) {
    if (!initializeDirectBufferSupport(env, thread)) {
      return 0;
    }
  }

  if (buf == NULL) {
    return -1;
  }

  if (!env->IsInstanceOf(buf, directBufferClass)) {
    return -1;
  }

  // NOTE that capacity is currently an int in the implementation
  return env->GetIntField(buf, bufferCapacityField);
}

JNI_LEAF(jint, jni_GetVersion(JNIEnv *env))
  return CurrentVersion;
JNI_END

extern struct JavaVM_ main_vm;

JNI_LEAF(jint, jni_GetJavaVM(JNIEnv *env, JavaVM **vm))
  *vm  = (JavaVM *)(&main_vm);
  return JNI_OK;
JNI_END

JNI_ENTRY(jobject, jni_GetModule(JNIEnv* env, jclass clazz))
  return Modules::get_module(clazz, THREAD);
JNI_END

// Structure containing all jni functions
struct JNINativeInterface_ jni_NativeInterface = {
    NULL,
    NULL,
    NULL,

    NULL,

    jni_GetVersion,

    jni_DefineClass,
    jni_FindClass,

    jni_FromReflectedMethod,
    jni_FromReflectedField,

    jni_ToReflectedMethod,

    jni_GetSuperclass,
    jni_IsAssignableFrom,

    jni_ToReflectedField,

    jni_Throw,
    jni_ThrowNew,
    jni_ExceptionOccurred,
    jni_ExceptionDescribe,
    jni_ExceptionClear,
    jni_FatalError,

    jni_PushLocalFrame,
    jni_PopLocalFrame,

    jni_NewGlobalRef,
    jni_DeleteGlobalRef,
    jni_DeleteLocalRef,
    jni_IsSameObject,

    jni_NewLocalRef,
    jni_EnsureLocalCapacity,

    jni_AllocObject,
    jni_NewObject,
    jni_NewObjectV,
    jni_NewObjectA,

    jni_GetObjectClass,
    jni_IsInstanceOf,

    jni_GetMethodID,

    jni_CallObjectMethod,
    jni_CallObjectMethodV,
    jni_CallObjectMethodA,
    jni_CallBooleanMethod,
    jni_CallBooleanMethodV,
    jni_CallBooleanMethodA,
    jni_CallByteMethod,
    jni_CallByteMethodV,
    jni_CallByteMethodA,
    jni_CallCharMethod,
    jni_CallCharMethodV,
    jni_CallCharMethodA,
    jni_CallShortMethod,
    jni_CallShortMethodV,
    jni_CallShortMethodA,
    jni_CallIntMethod,
    jni_CallIntMethodV,
    jni_CallIntMethodA,
    jni_CallLongMethod,
    jni_CallLongMethodV,
    jni_CallLongMethodA,
    jni_CallFloatMethod,
    jni_CallFloatMethodV,
    jni_CallFloatMethodA,
    jni_CallDoubleMethod,
    jni_CallDoubleMethodV,
    jni_CallDoubleMethodA,
    jni_CallVoidMethod,
    jni_CallVoidMethodV,
    jni_CallVoidMethodA,

    jni_CallNonvirtualObjectMethod,
    jni_CallNonvirtualObjectMethodV,
    jni_CallNonvirtualObjectMethodA,
    jni_CallNonvirtualBooleanMethod,
    jni_CallNonvirtualBooleanMethodV,
    jni_CallNonvirtualBooleanMethodA,
    jni_CallNonvirtualByteMethod,
    jni_CallNonvirtualByteMethodV,
    jni_CallNonvirtualByteMethodA,
    jni_CallNonvirtualCharMethod,
    jni_CallNonvirtualCharMethodV,
    jni_CallNonvirtualCharMethodA,
    jni_CallNonvirtualShortMethod,
    jni_CallNonvirtualShortMethodV,
    jni_CallNonvirtualShortMethodA,
    jni_CallNonvirtualIntMethod,
    jni_CallNonvirtualIntMethodV,
    jni_CallNonvirtualIntMethodA,
    jni_CallNonvirtualLongMethod,
    jni_CallNonvirtualLongMethodV,
    jni_CallNonvirtualLongMethodA,
    jni_CallNonvirtualFloatMethod,
    jni_CallNonvirtualFloatMethodV,
    jni_CallNonvirtualFloatMethodA,
    jni_CallNonvirtualDoubleMethod,
    jni_CallNonvirtualDoubleMethodV,
    jni_CallNonvirtualDoubleMethodA,
    jni_CallNonvirtualVoidMethod,
    jni_CallNonvirtualVoidMethodV,
    jni_CallNonvirtualVoidMethodA,

    jni_GetFieldID,

    jni_GetObjectField,
    jni_GetBooleanField,
    jni_GetByteField,
    jni_GetCharField,
    jni_GetShortField,
    jni_GetIntField,
    jni_GetLongField,
    jni_GetFloatField,
    jni_GetDoubleField,

    jni_SetObjectField,
    jni_SetBooleanField,
    jni_SetByteField,
    jni_SetCharField,
    jni_SetShortField,
    jni_SetIntField,
    jni_SetLongField,
    jni_SetFloatField,
    jni_SetDoubleField,

    jni_GetStaticMethodID,

    jni_CallStaticObjectMethod,
    jni_CallStaticObjectMethodV,
    jni_CallStaticObjectMethodA,
    jni_CallStaticBooleanMethod,
    jni_CallStaticBooleanMethodV,
    jni_CallStaticBooleanMethodA,
    jni_CallStaticByteMethod,
    jni_CallStaticByteMethodV,
    jni_CallStaticByteMethodA,
    jni_CallStaticCharMethod,
    jni_CallStaticCharMethodV,
    jni_CallStaticCharMethodA,
    jni_CallStaticShortMethod,
    jni_CallStaticShortMethodV,
    jni_CallStaticShortMethodA,
    jni_CallStaticIntMethod,
    jni_CallStaticIntMethodV,
    jni_CallStaticIntMethodA,
    jni_CallStaticLongMethod,
    jni_CallStaticLongMethodV,
    jni_CallStaticLongMethodA,
    jni_CallStaticFloatMethod,
    jni_CallStaticFloatMethodV,
    jni_CallStaticFloatMethodA,
    jni_CallStaticDoubleMethod,
    jni_CallStaticDoubleMethodV,
    jni_CallStaticDoubleMethodA,
    jni_CallStaticVoidMethod,
    jni_CallStaticVoidMethodV,
    jni_CallStaticVoidMethodA,

    jni_GetStaticFieldID,

    jni_GetStaticObjectField,
    jni_GetStaticBooleanField,
    jni_GetStaticByteField,
    jni_GetStaticCharField,
    jni_GetStaticShortField,
    jni_GetStaticIntField,
    jni_GetStaticLongField,
    jni_GetStaticFloatField,
    jni_GetStaticDoubleField,

    jni_SetStaticObjectField,
    jni_SetStaticBooleanField,
    jni_SetStaticByteField,
    jni_SetStaticCharField,
    jni_SetStaticShortField,
    jni_SetStaticIntField,
    jni_SetStaticLongField,
    jni_SetStaticFloatField,
    jni_SetStaticDoubleField,

    jni_NewString,
    jni_GetStringLength,
    jni_GetStringChars,
    jni_ReleaseStringChars,

    jni_NewStringUTF,
    jni_GetStringUTFLength,
    jni_GetStringUTFChars,
    jni_ReleaseStringUTFChars,

    jni_GetArrayLength,

    jni_NewObjectArray,
    jni_GetObjectArrayElement,
    jni_SetObjectArrayElement,

    jni_NewBooleanArray,
    jni_NewByteArray,
    jni_NewCharArray,
    jni_NewShortArray,
    jni_NewIntArray,
    jni_NewLongArray,
    jni_NewFloatArray,
    jni_NewDoubleArray,

    jni_GetBooleanArrayElements,
    jni_GetByteArrayElements,
    jni_GetCharArrayElements,
    jni_GetShortArrayElements,
    jni_GetIntArrayElements,
    jni_GetLongArrayElements,
    jni_GetFloatArrayElements,
    jni_GetDoubleArrayElements,

    jni_ReleaseBooleanArrayElements,
    jni_ReleaseByteArrayElements,
    jni_ReleaseCharArrayElements,
    jni_ReleaseShortArrayElements,
    jni_ReleaseIntArrayElements,
    jni_ReleaseLongArrayElements,
    jni_ReleaseFloatArrayElements,
    jni_ReleaseDoubleArrayElements,

    jni_GetBooleanArrayRegion,
    jni_GetByteArrayRegion,
    jni_GetCharArrayRegion,
    jni_GetShortArrayRegion,
    jni_GetIntArrayRegion,
    jni_GetLongArrayRegion,
    jni_GetFloatArrayRegion,
    jni_GetDoubleArrayRegion,

    jni_SetBooleanArrayRegion,
    jni_SetByteArrayRegion,
    jni_SetCharArrayRegion,
    jni_SetShortArrayRegion,
    jni_SetIntArrayRegion,
    jni_SetLongArrayRegion,
    jni_SetFloatArrayRegion,
    jni_SetDoubleArrayRegion,

    jni_RegisterNatives,
    jni_UnregisterNatives,

    jni_MonitorEnter,
    jni_MonitorExit,

    jni_GetJavaVM,

    jni_GetStringRegion,
    jni_GetStringUTFRegion,

    jni_GetPrimitiveArrayCritical,
    jni_ReleasePrimitiveArrayCritical,

    jni_GetStringCritical,
    jni_ReleaseStringCritical,

    jni_NewWeakGlobalRef,
    jni_DeleteWeakGlobalRef,

    jni_ExceptionCheck,

    jni_NewDirectByteBuffer,
    jni_GetDirectBufferAddress,
    jni_GetDirectBufferCapacity,

    // New 1_6 features

    jni_GetObjectRefType,

    // Module features

    jni_GetModule
};

// For jvmti use to modify jni function table.
// Java threads in native contiues to run until it is transitioned
// to VM at safepoint. Before the transition or before it is blocked
// for safepoint it may access jni function table. VM could crash if
// any java thread access the jni function table in the middle of memcpy.
// To avoid this each function pointers are copied automically.
void copy_jni_function_table(const struct JNINativeInterface_ *new_jni_NativeInterface) {
  intptr_t *a = (intptr_t *) jni_functions();
  intptr_t *b = (intptr_t *) new_jni_NativeInterface;
  for (uint i = 0; i < sizeof(struct JNINativeInterface_) / sizeof(void *); i++) {
    Atomic::store(*b++, a++);
  }
}

void quicken_jni_functions() {
  // Replace Get<Primitive>Field with fast versions
  if (UseFastJNIAccessors && !VerifyJNIFields && !CountJNICalls) {
    address func;
    func = JNI_FastGetField::generate_fast_get_boolean_field();
    if (func != (address)-1) {
      jni_NativeInterface.GetBooleanField = (GetBooleanField_t)func;
    }
    func = JNI_FastGetField::generate_fast_get_byte_field();
    if (func != (address)-1) {
      jni_NativeInterface.GetByteField = (GetByteField_t)func;
    }
    func = JNI_FastGetField::generate_fast_get_char_field();
    if (func != (address)-1) {
      jni_NativeInterface.GetCharField = (GetCharField_t)func;
    }
    func = JNI_FastGetField::generate_fast_get_short_field();
    if (func != (address)-1) {
      jni_NativeInterface.GetShortField = (GetShortField_t)func;
    }
    func = JNI_FastGetField::generate_fast_get_int_field();
    if (func != (address)-1) {
      jni_NativeInterface.GetIntField = (GetIntField_t)func;
    }
    func = JNI_FastGetField::generate_fast_get_long_field();
    if (func != (address)-1) {
      jni_NativeInterface.GetLongField = (GetLongField_t)func;
    }
    func = JNI_FastGetField::generate_fast_get_float_field();
    if (func != (address)-1) {
      jni_NativeInterface.GetFloatField = (GetFloatField_t)func;
    }
    func = JNI_FastGetField::generate_fast_get_double_field();
    if (func != (address)-1) {
      jni_NativeInterface.GetDoubleField = (GetDoubleField_t)func;
    }
  }
}

// Returns the function structure
struct JNINativeInterface_* jni_functions() {
  return &jni_NativeInterface;
}

// Returns the function structure
struct JNINativeInterface_* jni_functions_nocheck() {
  return &jni_NativeInterface;
}

// Invocation API

// Forward declaration
extern const struct JNIInvokeInterface_ jni_InvokeInterface;

// Global invocation API vars
volatile int vm_created = 0;
// Indicate whether it is safe to recreate VM
volatile int safe_to_recreate_vm = 1;
struct JavaVM_ main_vm = { &jni_InvokeInterface };

#define JAVASTACKSIZE (400 * 1024)    /* Default size of a thread java stack */
enum { VERIFY_NONE, VERIFY_REMOTE, VERIFY_ALL };

_JNI_IMPORT_OR_EXPORT_ jint JNICALL JNI_GetDefaultJavaVMInitArgs(void *args_) {
  JDK1_1InitArgs *args = (JDK1_1InitArgs *)args_;
  jint ret = JNI_ERR;

  if (Threads::is_supported_jni_version(args->version)) {
    ret = JNI_OK;
  }
  // 1.1 style no longer supported in hotspot.
  // According the JNI spec, we should update args->version on return.
  // We also use the structure to communicate with launcher about default
  // stack size.
  if (args->version == JNI_VERSION_1_1) {
    args->version = JNI_VERSION_1_2;
    // javaStackSize is int in arguments structure
    args->javaStackSize = (jint)(ThreadStackSize * K);
  }
  return ret;
}

static jint JNI_CreateJavaVM_inner(JavaVM **vm, void **penv, void *args) {
  jint result = JNI_ERR;

  // At the moment it's only possible to have one Java VM,
  // since some of the runtime state is in global variables.

  // We cannot use our mutex locks here, since they only work on
  // Threads. We do an atomic compare and exchange to ensure only
  // one thread can call this method at a time

  // We use Atomic::xchg rather than Atomic::add/dec since on some platforms
  // the add/dec implementations are dependent on whether we are running
  // on a multiprocessor, and at this stage of initialization the os::is_MP
  // function used to determine this will always return false. Atomic::xchg
  // does not have this problem.
  if (Atomic::xchg(1, &vm_created) == 1) {
    return JNI_EEXIST;   // already created, or create attempt in progress
  }
  if (Atomic::xchg(0, &safe_to_recreate_vm) == 0) {
    return JNI_ERR;  // someone tried and failed and retry not allowed.
  }

  /**
   * Certain errors during initialization are recoverable and do not
   * prevent this method from being called again at a later time
   * (perhaps with different arguments).  However, at a certain
   * point during initialization if an error occurs we cannot allow
   * this function to be called again (or it will crash).  In those
   * situations, the 'canTryAgain' flag is set to false, which atomically
   * sets safe_to_recreate_vm to 1, such that any new call to
   * JNI_CreateJavaVM will immediately fail using the above logic.
   */
  bool can_try_again = true;

  result = Threads::create_vm((JavaVMInitArgs*) args, &can_try_again);
  if (result == JNI_OK) {
    JavaThread *thread = JavaThread::current();
    /* thread is thread_in_vm here */
    *vm = (JavaVM *)(&main_vm);
    *(JNIEnv**)penv = thread->jni_environment();

    if (EnableJVMCI) {
      if (UseJVMCICompiler) {
        // JVMCI is initialized on a CompilerThread
        if (BootstrapJVMCI) {
          JavaThread* THREAD = thread;
          JVMCICompiler* compiler = JVMCICompiler::instance(true, CATCH);
          compiler->bootstrap(THREAD);
          if (HAS_PENDING_EXCEPTION) {
            HandleMark hm;
            vm_exit_during_initialization(Handle(THREAD, PENDING_EXCEPTION));
          }
        }
      }
    }

    // Since this is not a JVM_ENTRY we have to set the thread state manually before leaving.
    ThreadStateTransition::transition_and_fence(thread, _thread_in_vm, _thread_in_native);
  } else {
    // If create_vm exits because of a pending exception, exit with that
    // exception.  In the future when we figure out how to reclaim memory,
    // we may be able to exit with JNI_ERR and allow the calling application
    // to continue.
    if (Universe::is_fully_initialized()) {
      // otherwise no pending exception possible - VM will already have aborted
      JavaThread* THREAD = JavaThread::current();
      if (HAS_PENDING_EXCEPTION) {
        HandleMark hm;
        vm_exit_during_initialization(Handle(THREAD, PENDING_EXCEPTION));
      }
    }

    if (can_try_again) {
      // reset safe_to_recreate_vm to 1 so that retrial would be possible
      safe_to_recreate_vm = 1;
    }

    // Creation failed. We must reset vm_created
    *vm = 0;
    *(JNIEnv**)penv = 0;
    // reset vm_created last to avoid race condition. Use OrderAccess to
    // control both compiler and architectural-based reordering.
    OrderAccess::release_store(&vm_created, 0);
  }

  // Flush stdout and stderr before exit.
  fflush(stdout);
  fflush(stderr);

  return result;
}

_JNI_IMPORT_OR_EXPORT_ jint JNICALL JNI_CreateJavaVM(JavaVM **vm, void **penv, void *args) {
  jint result = JNI_ERR;
    result = JNI_CreateJavaVM_inner(vm, penv, args);
  return result;
}

_JNI_IMPORT_OR_EXPORT_ jint JNICALL JNI_GetCreatedJavaVMs(JavaVM **vm_buf, jsize bufLen, jsize *numVMs) {
  if (vm_created == 1) {
    if (numVMs != NULL) *numVMs = 1;
    if (bufLen > 0)     *vm_buf = (JavaVM *)(&main_vm);
  } else {
    if (numVMs != NULL) *numVMs = 0;
  }
  return JNI_OK;
}

extern "C" {
static jint JNICALL jni_DestroyJavaVM_inner(JavaVM *vm) {
  jint res = JNI_ERR;

  if (vm_created == 0) {
    res = JNI_ERR;
    return res;
  }

  JNIEnv *env;
  JavaVMAttachArgs destroyargs;
  destroyargs.version = CurrentVersion;
  destroyargs.name = (char *)"DestroyJavaVM";
  destroyargs.group = NULL;
  res = vm->AttachCurrentThread((void **)&env, (void *)&destroyargs);
  if (res != JNI_OK) {
    return res;
  }

  // Since this is not a JVM_ENTRY we have to set the thread state manually before entering.
  JavaThread* thread = JavaThread::current();
  ThreadStateTransition::transition_from_native(thread, _thread_in_vm);
  if (Threads::destroy_vm()) {
    // Should not change thread state, VM is gone
    vm_created = 0;
    res = JNI_OK;
    return res;
  } else {
    ThreadStateTransition::transition_and_fence(thread, _thread_in_vm, _thread_in_native);
    res = JNI_ERR;
    return res;
  }
}

jint JNICALL jni_DestroyJavaVM(JavaVM *vm) {
  jint result = JNI_ERR;
    result = jni_DestroyJavaVM_inner(vm);
  return result;
}

static jint attach_current_thread(JavaVM *vm, void **penv, void *_args, bool daemon) {
  JavaVMAttachArgs *args = (JavaVMAttachArgs *) _args;

  // Check below commented out from JDK1.2fcs as well
  /*
  if (args && (args->version != JNI_VERSION_1_1 || args->version != JNI_VERSION_1_2)) {
    return JNI_EVERSION;
  }
  */

  Thread* t = Thread::current_or_null();
  if (t != NULL) {
    // If the thread has been attached this operation is a no-op
    *(JNIEnv**)penv = ((JavaThread*) t)->jni_environment();
    return JNI_OK;
  }

  // Create a thread and mark it as attaching so it will be skipped by the
  // ThreadsListEnumerator - see CR 6404306
  JavaThread* thread = new JavaThread(true);

  // Set correct safepoint info. The thread is going to call into Java when
  // initializing the Java level thread object. Hence, the correct state must
  // be set in order for the Safepoint code to deal with it correctly.
  thread->set_thread_state(_thread_in_vm);
  thread->record_stack_base_and_size();
  thread->register_thread_stack_with_NMT();
  thread->initialize_thread_current();

  if (!os::create_attached_thread(thread)) {
    thread->smr_delete();
    return JNI_ERR;
  }
  // Enable stack overflow checks
  thread->create_stack_guard_pages();

  thread->initialize_tlab();

  thread->cache_global_variables();

  // Crucial that we do not have a safepoint check for this thread, since it has
  // not been added to the Thread list yet.
  { Threads_lock->lock_without_safepoint_check();
    // This must be inside this lock in order to get FullGCALot to work properly, i.e., to
    // avoid this thread trying to do a GC before it is added to the thread-list
    thread->set_active_handles(JNIHandleBlock::allocate_block());
    Threads::add(thread, daemon);
    Threads_lock->unlock();
  }
  // Create thread group and name info from attach arguments
  oop group = NULL;
  char* thread_name = NULL;
  if (args != NULL && Threads::is_supported_jni_version(args->version)) {
    group = JNIHandles::resolve(args->group);
    thread_name = args->name; // may be NULL
  }
  if (group == NULL) group = Universe::main_thread_group();

  // Create Java level thread object and attach it to this thread
  bool attach_failed = false;
  {
    EXCEPTION_MARK;
    HandleMark hm(THREAD);
    Handle thread_group(THREAD, group);
    thread->allocate_threadObj(thread_group, thread_name, daemon, THREAD);
    if (HAS_PENDING_EXCEPTION) {
      CLEAR_PENDING_EXCEPTION;
      // cleanup outside the handle mark.
      attach_failed = true;
    }
  }

  if (attach_failed) {
    // Added missing cleanup
    thread->cleanup_failed_attach_current_thread();
    return JNI_ERR;
  }

  // mark the thread as no longer attaching
  // this uses a fence to push the change through so we don't have
  // to regrab the threads_lock
  thread->set_done_attaching_via_jni();

  // Set java thread status.
  java_lang_Thread::set_thread_status(thread->threadObj(), java_lang_Thread::RUNNABLE);

  *(JNIEnv**)penv = thread->jni_environment();

  // Now leaving the VM, so change thread_state. This is normally automatically taken care
  // of in the JVM_ENTRY. But in this situation we have to do it manually. Notice, that by
  // using ThreadStateTransition::transition, we do a callback to the safepoint code if
  // needed.

  ThreadStateTransition::transition_and_fence(thread, _thread_in_vm, _thread_in_native);

  // Perform any platform dependent FPU setup
  os::setup_fpu();

  return JNI_OK;
}

jint JNICALL jni_AttachCurrentThread(JavaVM *vm, void **penv, void *_args) {
  if (vm_created == 0) {
    return JNI_ERR;
  }
  return attach_current_thread(vm, penv, _args, false);
}

jint JNICALL jni_DetachCurrentThread(JavaVM *vm) {
  // If the thread has already been detached the operation is a no-op
  if (Thread::current_or_null() == NULL) {
    return JNI_OK;
  }

  VM_Exit::block_if_vm_exited();

  JavaThread* thread = JavaThread::current();
  if (thread->has_last_Java_frame()) {
    // Can't detach a thread that's running java, that can't work.
    return JNI_ERR;
  }

  // Safepoint support. Have to do call-back to safepoint code, if in the
  // middle of a safepoint operation
  ThreadStateTransition::transition_from_native(thread, _thread_in_vm);

  // XXX: Note that JavaThread::exit() call below removes the guards on the
  // stack pages set up via enable_stack_{red,yellow}_zone() calls
  // above in jni_AttachCurrentThread. Unfortunately, while the setting
  // of the guards is visible in jni_AttachCurrentThread above,
  // the removal of the guards is buried below in JavaThread::exit()
  // here. The abstraction should be more symmetrically either exposed
  // or hidden (e.g. it could probably be hidden in the same
  // (platform-dependent) methods where we do alternate stack
  // maintenance work?)
  thread->exit(false, JavaThread::jni_detach);
  thread->smr_delete();

  return JNI_OK;
}

jint JNICALL jni_GetEnv(JavaVM *vm, void **penv, jint version) {
  if (vm_created == 0) {
    *penv = NULL;
    return JNI_EDETACHED;
  }

#ifndef JVMPI_VERSION_1
// need these in order to be polite about older agents
#define JVMPI_VERSION_1   ((jint)0x10000001)
#define JVMPI_VERSION_1_1 ((jint)0x10000002)
#define JVMPI_VERSION_1_2 ((jint)0x10000003)
#endif

  Thread* thread = Thread::current_or_null();
  if (thread != NULL && thread->is_Java_thread()) {
    if (Threads::is_supported_jni_version_including_1_1(version)) {
      *(JNIEnv**)penv = ((JavaThread*) thread)->jni_environment();
      return JNI_OK;
    } else if (version == JVMPI_VERSION_1 || version == JVMPI_VERSION_1_1 || version == JVMPI_VERSION_1_2) {
      tty->print_cr("ERROR: JVMPI, an experimental interface, is no longer supported.");
      tty->print_cr("Please use the supported interface: the JVM Tool Interface (JVM TI).");
      return JNI_EVERSION;
    } else {
      *penv = NULL;
      return JNI_EVERSION;
    }
  } else {
    *penv = NULL;
    return JNI_EDETACHED;
  }
}

jint JNICALL jni_AttachCurrentThreadAsDaemon(JavaVM *vm, void **penv, void *_args) {
  if (vm_created == 0) {
    return JNI_ERR;
  }
  return attach_current_thread(vm, penv, _args, true);
}
}

const struct JNIInvokeInterface_ jni_InvokeInterface = {
    NULL,
    NULL,
    NULL,

    jni_DestroyJavaVM,
    jni_AttachCurrentThread,
    jni_DetachCurrentThread,
    jni_GetEnv,
    jni_AttachCurrentThreadAsDaemon
};
