#include "precompiled.hpp"

#include "ci/ciUtilities.inline.hpp"
#include "classfile/javaClasses.inline.hpp"
#include "code/scopeDesc.hpp"
#include "interpreter/linkResolver.hpp"
#include "memory/oopFactory.hpp"
#include "oops/cpCache.inline.hpp"
#include "oops/generateOopMap.hpp"
#include "oops/method.inline.hpp"
#include "oops/objArrayOop.inline.hpp"
#include "oops/typeArrayOop.inline.hpp"
#include "compiler/compileBroker.hpp"
#include "compiler/disassembler.hpp"
#include "jvmci/jvmciCompilerToVM.hpp"
#include "jvmci/jvmciCodeInstaller.hpp"
#include "jvmci/jvmciRuntime.hpp"
#include "runtime/flags/jvmFlag.hpp"
#include "runtime/frame.inline.hpp"
#include "runtime/interfaceSupport.inline.hpp"
#include "runtime/jniHandles.inline.hpp"
#include "runtime/vframe_hp.hpp"

JVMCIKlassHandle::JVMCIKlassHandle(Thread* thread, Klass* klass) {
  _thread = thread;
  _klass = klass;
  if (klass != NULL) {
    _holder = Handle(_thread, klass->holder_phantom());
  }
}

JVMCIKlassHandle& JVMCIKlassHandle::operator=(Klass* klass) {
  _klass = klass;
  if (klass != NULL) {
    _holder = Handle(_thread, klass->holder_phantom());
  }
  return *this;
}

void JNIHandleMark::push_jni_handle_block() {
  JavaThread* thread = JavaThread::current();
  if (thread != NULL) {
    // Allocate a new block for JNI handles.
    // Inlined code from jni_PushLocalFrame()
    JNIHandleBlock* java_handles = ((JavaThread*)thread)->active_handles();
    JNIHandleBlock* compile_handles = JNIHandleBlock::allocate_block(thread);
    compile_handles->set_pop_frame_link(java_handles);
    thread->set_active_handles(compile_handles);
  }
}

void JNIHandleMark::pop_jni_handle_block() {
  JavaThread* thread = JavaThread::current();
  if (thread != NULL) {
    // Release our JNI handle block
    JNIHandleBlock* compile_handles = thread->active_handles();
    JNIHandleBlock* java_handles = compile_handles->pop_frame_link();
    thread->set_active_handles(java_handles);
    compile_handles->set_pop_frame_link(NULL);
    JNIHandleBlock::release_block(compile_handles, thread); // may block
  }
}

// Entry to native method implementation that transitions current thread to '_thread_in_vm'.
#define C2V_VMENTRY(result_type, name, signature) \
  JNIEXPORT result_type JNICALL c2v_ ## name signature { \
  JVMCI_VM_ENTRY_MARK;

#define C2V_END }

oop CompilerToVM::get_jvmci_method(const methodHandle& method, TRAPS) {
  if (method() != NULL) {
    JavaValue result(T_OBJECT);
    JavaCallArguments args;
    args.push_long((jlong) (address) method());
    JavaCalls::call_static(&result, SystemDictionary::HotSpotResolvedJavaMethodImpl_klass(), vmSymbols::fromMetaspace_name(), vmSymbols::method_fromMetaspace_signature(), &args, CHECK_NULL);

    return (oop)result.get_jobject();
  }
  return NULL;
}

oop CompilerToVM::get_jvmci_type(JVMCIKlassHandle& klass, TRAPS) {
  if (!klass.is_null()) {
    JavaValue result(T_OBJECT);
    JavaCallArguments args;
    args.push_oop(Handle(THREAD, klass->java_mirror()));
    JavaCalls::call_static(&result, SystemDictionary::HotSpotResolvedObjectTypeImpl_klass(), vmSymbols::fromMetaspace_name(), vmSymbols::klass_fromMetaspace_signature(), &args, CHECK_NULL);

    return (oop)result.get_jobject();
  }
  return NULL;
}

Handle JavaArgumentUnboxer::next_arg(BasicType expectedType) {
  oop arg=((objArrayOop) (_args))->obj_at(_index++);
  return Handle(Thread::current(), arg);
}

jobjectArray readConfiguration0(JNIEnv *env, TRAPS);

C2V_VMENTRY(jobjectArray, readConfiguration, (JNIEnv *env))
   jobjectArray config = readConfiguration0(env, CHECK_NULL);
   return config;
C2V_END

C2V_VMENTRY(jobject, getFlagValue, (JNIEnv *, jobject c2vm, jobject name_handle))
#define RETURN_BOXED_LONG(value) oop box; jvalue p; p.j = (jlong) (value); box = java_lang_boxing_object::create(T_LONG, &p, CHECK_NULL); return JNIHandles::make_local(THREAD, box);
#define RETURN_BOXED_DOUBLE(value) oop box; jvalue p; p.d = (jdouble) (value); box = java_lang_boxing_object::create(T_DOUBLE, &p, CHECK_NULL); return JNIHandles::make_local(THREAD, box);
  Handle name(THREAD, JNIHandles::resolve(name_handle));
  if (name.is_null()) {
    THROW_0(vmSymbols::java_lang_NullPointerException());
  }
  ResourceMark rm;
  const char* cstring = java_lang_String::as_utf8_string(name());
  JVMFlag* flag = JVMFlag::find_flag(cstring, strlen(cstring), /* allow_locked */ true, /* return_flag */ true);
  if (flag == NULL) {
    return c2vm;
  }
  if (flag->is_bool()) {
    jvalue prim;
    prim.z = flag->get_bool();
    oop box = java_lang_boxing_object::create(T_BOOLEAN, &prim, CHECK_NULL);
    return JNIHandles::make_local(THREAD, box);
  } else if (flag->is_ccstr()) {
    Handle value = java_lang_String::create_from_str(flag->get_ccstr(), CHECK_NULL);
    return JNIHandles::make_local(THREAD, value());
  } else if (flag->is_intx()) {
    RETURN_BOXED_LONG(flag->get_intx());
  } else if (flag->is_int()) {
    RETURN_BOXED_LONG(flag->get_int());
  } else if (flag->is_uint()) {
    RETURN_BOXED_LONG(flag->get_uint());
  } else if (flag->is_uint64_t()) {
    RETURN_BOXED_LONG(flag->get_uint64_t());
  } else if (flag->is_size_t()) {
    RETURN_BOXED_LONG(flag->get_size_t());
  } else if (flag->is_uintx()) {
    RETURN_BOXED_LONG(flag->get_uintx());
  } else if (flag->is_double()) {
    RETURN_BOXED_DOUBLE(flag->get_double());
  } else {
    JVMCI_ERROR_NULL("VM flag %s has unsupported type %s", flag->_name, flag->_type);
  }
#undef RETURN_BOXED_LONG
#undef RETURN_BOXED_DOUBLE
C2V_END

C2V_VMENTRY(jobject, asResolvedJavaMethod, (JNIEnv *, jobject, jobject executable_handle))
  oop executable = JNIHandles::resolve(executable_handle);
  oop mirror = NULL;
  int slot = 0;

  if (executable->klass() == SystemDictionary::reflect_Constructor_klass()) {
    mirror = java_lang_reflect_Constructor::clazz(executable);
    slot = java_lang_reflect_Constructor::slot(executable);
  } else {
    mirror = java_lang_reflect_Method::clazz(executable);
    slot = java_lang_reflect_Method::slot(executable);
  }
  Klass* holder = java_lang_Class::as_Klass(mirror);
  methodHandle method = InstanceKlass::cast(holder)->method_with_idnum(slot);
  oop result = CompilerToVM::get_jvmci_method(method, CHECK_NULL);
  return JNIHandles::make_local(THREAD, result);
}

C2V_VMENTRY(jobject, getResolvedJavaMethod, (JNIEnv *, jobject, jobject base, jlong offset))
  methodHandle method;
  oop base_object = JNIHandles::resolve(base);
  if (base_object == NULL) {
    method = *((Method**)(offset));
  } else if (base_object->is_a(SystemDictionary::HotSpotResolvedJavaMethodImpl_klass())) {
    method = *((Method**)(HotSpotResolvedJavaMethodImpl::metaspaceMethod(base_object) + offset));
  } else {
    THROW_MSG_0(vmSymbols::java_lang_IllegalArgumentException(), err_msg("Unexpected type: %s", base_object->klass()->external_name()));
  }
  oop result = CompilerToVM::get_jvmci_method(method, CHECK_NULL);
  return JNIHandles::make_local(THREAD, result);
}

C2V_VMENTRY(jobject, getResolvedJavaType, (JNIEnv *, jobject, jobject base, jlong offset, jboolean compressed))
  JVMCIKlassHandle klass(THREAD);
  oop base_object = JNIHandles::resolve(base);
  jlong base_address = 0;
  if (base_object != NULL && offset == oopDesc::klass_offset_in_bytes()) {
    klass = base_object->klass();
  } else if (!compressed) {
    if (base_object != NULL) {
      if (base_object->is_a(SystemDictionary::HotSpotResolvedJavaMethodImpl_klass())) {
        base_address = HotSpotResolvedJavaMethodImpl::metaspaceMethod(base_object);
      } else if (base_object->is_a(SystemDictionary::HotSpotConstantPool_klass())) {
        base_address = HotSpotConstantPool::metaspaceConstantPool(base_object);
      } else if (base_object->is_a(SystemDictionary::HotSpotResolvedObjectTypeImpl_klass())) {
        base_address = (jlong) CompilerToVM::asKlass(base_object);
      } else if (base_object->is_a(SystemDictionary::Class_klass())) {
        base_address = (jlong) (address) base_object;
      } else {
        THROW_MSG_0(vmSymbols::java_lang_IllegalArgumentException(),
                    err_msg("Unexpected arguments: %s " JLONG_FORMAT " %s", base_object->klass()->external_name(), offset, compressed ? "true" : "false"));
      }
    }
    klass = *((Klass**) (intptr_t) (base_address + offset));
  } else {
    THROW_MSG_0(vmSymbols::java_lang_IllegalArgumentException(), err_msg("Unexpected arguments: %s " JLONG_FORMAT " %s", base_object != NULL ? base_object->klass()->external_name() : "null", offset, compressed ? "true" : "false"));
  }
  oop result = CompilerToVM::get_jvmci_type(klass, CHECK_NULL);
  return JNIHandles::make_local(THREAD, result);
}

C2V_VMENTRY(jint, installCode, (JNIEnv *jniEnv, jobject, jobject target, jobject compiled_code, jobject installed_code, jobject speculation_log))
  ResourceMark rm;
  HandleMark hm;
  JNIHandleMark jni_hm;

  Handle target_handle(THREAD, JNIHandles::resolve(target));
  Handle compiled_code_handle(THREAD, JNIHandles::resolve(compiled_code));
  CodeBlob* cb = NULL;
  Handle installed_code_handle(THREAD, JNIHandles::resolve(installed_code));
  Handle speculation_log_handle(THREAD, JNIHandles::resolve(speculation_log));

  JVMCICompiler* compiler = JVMCICompiler::instance(true, CHECK_JNI_ERR);

  bool is_immutable_PIC = HotSpotCompiledCode::isImmutablePIC(compiled_code_handle) > 0;
  CodeInstaller installer(is_immutable_PIC);
  JVMCIEnv::CodeInstallResult result = installer.install(compiler, target_handle, compiled_code_handle, cb, installed_code_handle, speculation_log_handle, CHECK_0);

  if (result == JVMCIEnv::ok) {
    if (installed_code_handle.not_null()) {
      nmethod::invalidate_installed_code(installed_code_handle, CHECK_0);
      {
        // Ensure that all updates to the InstalledCode fields are consistent.
        MutexLockerEx pl(Patching_lock, Mutex::_no_safepoint_check_flag);
        InstalledCode::set_address(installed_code_handle, (jlong) cb);
        InstalledCode::set_version(installed_code_handle, InstalledCode::version(installed_code_handle) + 1);
        if (cb->is_nmethod()) {
          InstalledCode::set_entryPoint(installed_code_handle, (jlong) cb->as_nmethod_or_null()->verified_entry_point());
        } else {
          InstalledCode::set_entryPoint(installed_code_handle, (jlong) cb->code_begin());
        }
        if (installed_code_handle->is_a(HotSpotInstalledCode::klass())) {
          HotSpotInstalledCode::set_size(installed_code_handle, cb->size());
          HotSpotInstalledCode::set_codeStart(installed_code_handle, (jlong) cb->code_begin());
          HotSpotInstalledCode::set_codeSize(installed_code_handle, cb->code_size());
        }
      }
    }
  }
  return result;
C2V_END

C2V_VMENTRY(jobject, disassembleCodeBlob, (JNIEnv *jniEnv, jobject, jobject installedCode))
  ResourceMark rm;
  HandleMark hm;

  if (installedCode == NULL) {
    THROW_MSG_NULL(vmSymbols::java_lang_NullPointerException(), "installedCode is null");
  }

  jlong codeBlob = InstalledCode::address(installedCode);
  if (codeBlob == 0L) {
    return NULL;
  }

  CodeBlob* cb = (CodeBlob*) (address) codeBlob;
  if (cb == NULL) {
    return NULL;
  }

  // We don't want the stringStream buffer to resize during disassembly as it
  // uses scoped resource memory. If a nested function called during disassembly uses
  // a ResourceMark and the buffer expands within the scope of the mark,
  // the buffer becomes garbage when that scope is exited. Experience shows that
  // the disassembled code is typically about 10x the code size so a fixed buffer
  // sized to 20x code size plus a fixed amount for header info should be sufficient.
  int bufferSize = cb->code_size() * 20 + 1024;
  char* buffer = NEW_RESOURCE_ARRAY(char, bufferSize);
  stringStream st(buffer, bufferSize);
  if (cb->is_nmethod()) {
    nmethod* nm = (nmethod*) cb;
    if (!nm->is_alive()) {
      return NULL;
    }
  }
  Disassembler::decode(cb, &st);
  if (st.size() <= 0) {
    return NULL;
  }

  Handle result = java_lang_String::create_from_platform_dependent_str(st.as_string(), CHECK_NULL);
  return JNIHandles::make_local(THREAD, result());
C2V_END

C2V_VMENTRY(jobject, executeInstalledCode, (JNIEnv*, jobject, jobject args, jobject hotspotInstalledCode))
  ResourceMark rm;
  HandleMark hm;

  jlong nmethodValue = InstalledCode::address(hotspotInstalledCode);
  if (nmethodValue == 0L) {
    THROW_NULL(vmSymbols::jdk_vm_ci_code_InvalidInstalledCodeException());
  }
  nmethod* nm = (nmethod*) (address) nmethodValue;
  methodHandle mh = nm->method();
  Symbol* signature = mh->signature();
  JavaCallArguments jca(mh->size_of_parameters());

  JavaArgumentUnboxer jap(signature, &jca, (arrayOop) JNIHandles::resolve(args), mh->is_static());
  JavaValue result(jap.get_ret_type());
  jca.set_alternative_target(nm);
  JavaCalls::call(&result, mh, &jca, CHECK_NULL);

  if (jap.get_ret_type() == T_VOID) {
    return NULL;
  } else if (jap.get_ret_type() == T_OBJECT || jap.get_ret_type() == T_ARRAY) {
    return JNIHandles::make_local(THREAD, (oop) result.get_jobject());
  } else {
    jvalue *value = (jvalue *) result.get_value_addr();
    // Narrow the value down if required (Important on big endian machines)
    switch (jap.get_ret_type()) {
      case T_BOOLEAN:
       value->z = (jboolean) value->i;
       break;
      case T_BYTE:
       value->b = (jbyte) value->i;
       break;
      case T_CHAR:
       value->c = (jchar) value->i;
       break;
      case T_SHORT:
       value->s = (jshort) value->i;
       break;
      default:
        break;
    }
    oop o = java_lang_boxing_object::create(jap.get_ret_type(), value, CHECK_NULL);
    return JNIHandles::make_local(THREAD, o);
  }
C2V_END

C2V_VMENTRY(void, invalidateInstalledCode, (JNIEnv*, jobject, jobject installed_code))
  Handle installed_code_handle(THREAD, JNIHandles::resolve(installed_code));
  nmethod::invalidate_installed_code(installed_code_handle, CHECK);
C2V_END

C2V_VMENTRY(int, allocateCompileId, (JNIEnv*, jobject, jobject jvmci_method, int entry_bci))
  HandleMark hm;
  ResourceMark rm;
  if (JNIHandles::resolve(jvmci_method) == NULL) {
    THROW_0(vmSymbols::java_lang_NullPointerException());
  }
  Method* method = CompilerToVM::asMethod(jvmci_method);
  if (entry_bci >= method->code_size() || entry_bci < -1) {
    THROW_MSG_0(vmSymbols::java_lang_IllegalArgumentException(), err_msg("Unexpected bci %d", entry_bci));
  }
  return CompileBroker::assign_compile_id_unlocked(THREAD, method);
C2V_END

C2V_VMENTRY(jobject, getSymbol, (JNIEnv*, jobject, jlong symbol))
  Handle sym = java_lang_String::create_from_symbol((Symbol*)(address)symbol, CHECK_NULL);
  return JNIHandles::make_local(THREAD, sym());
C2V_END

bool matches(jobjectArray methods, Method* method) {
  objArrayOop methods_oop = (objArrayOop) JNIHandles::resolve(methods);

  for (int i = 0; i < methods_oop->length(); i++) {
    oop resolved = methods_oop->obj_at(i);
    if (resolved->is_a(HotSpotResolvedJavaMethodImpl::klass()) && CompilerToVM::asMethod(resolved) == method) {
      return true;
    }
  }
  return false;
}

void call_interface(JavaValue* result, Klass* spec_klass, Symbol* name, Symbol* signature, JavaCallArguments* args, TRAPS) {
  CallInfo callinfo;
  Handle receiver = args->receiver();
  Klass* recvrKlass = receiver.is_null() ? (Klass*)NULL : receiver->klass();
  LinkInfo link_info(spec_klass, name, signature);
  LinkResolver::resolve_interface_call(callinfo, receiver, recvrKlass, link_info, true, CHECK);
  methodHandle method = callinfo.selected_method();

  // Invoke the method
  JavaCalls::call(result, method, args, CHECK);
}

#define CC (char*)  /*cast a literal from (const char*)*/
#define FN_PTR(f) CAST_FROM_FN_PTR(void*, &(c2v_ ## f))

#define STRING                  "Ljava/lang/String;"
#define OBJECT                  "Ljava/lang/Object;"
#define EXECUTABLE              "Ljava/lang/reflect/Executable;"
#define INSTALLED_CODE          "Ljdk/vm/ci/code/InstalledCode;"
#define TARGET_DESCRIPTION      "Ljdk/vm/ci/code/TargetDescription;"
#define HS_RESOLVED_METHOD      "Ljdk/vm/ci/hotspot/HotSpotResolvedJavaMethodImpl;"
#define HS_RESOLVED_KLASS       "Ljdk/vm/ci/hotspot/HotSpotResolvedObjectTypeImpl;"
#define HS_COMPILED_CODE        "Ljdk/vm/ci/hotspot/HotSpotCompiledCode;"
#define HS_SPECULATION_LOG      "Ljdk/vm/ci/hotspot/HotSpotSpeculationLog;"

JNINativeMethod CompilerToVM::methods[] = {
  { CC "asResolvedJavaMethod",    CC "(" EXECUTABLE ")" HS_RESOLVED_METHOD,                                          FN_PTR(asResolvedJavaMethod)},
  { CC "getResolvedJavaMethod",   CC "(Ljava/lang/Object;J)" HS_RESOLVED_METHOD,                                     FN_PTR(getResolvedJavaMethod)},
  { CC "getResolvedJavaType",     CC "(Ljava/lang/Object;JZ)" HS_RESOLVED_KLASS,                                     FN_PTR(getResolvedJavaType)},
  { CC "readConfiguration",       CC "()[" OBJECT,                                                                   FN_PTR(readConfiguration)},
  { CC "installCode",             CC "(" TARGET_DESCRIPTION HS_COMPILED_CODE INSTALLED_CODE HS_SPECULATION_LOG ")I", FN_PTR(installCode)},
  { CC "disassembleCodeBlob",     CC "(" INSTALLED_CODE ")" STRING,                                                  FN_PTR(disassembleCodeBlob)},
  { CC "executeInstalledCode",    CC "([" OBJECT INSTALLED_CODE ")" OBJECT,                                          FN_PTR(executeInstalledCode)},
  { CC "invalidateInstalledCode", CC "(" INSTALLED_CODE ")V",                                                        FN_PTR(invalidateInstalledCode)},
  { CC "allocateCompileId",       CC "(" HS_RESOLVED_METHOD "I)I",                                                   FN_PTR(allocateCompileId)},
  { CC "getSymbol",               CC "(J)" STRING,                                                                   FN_PTR(getSymbol)},
  { CC "getFlagValue",            CC "(" STRING ")" OBJECT,                                                          FN_PTR(getFlagValue)},
};

int CompilerToVM::methods_count() {
  return sizeof(methods) / sizeof(JNINativeMethod);
}
