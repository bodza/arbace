#include "precompiled.hpp"
#include "classfile/javaClasses.hpp"
#include "classfile/systemDictionary.hpp"
#include "classfile/vmSymbols.hpp"
#include "memory/oopFactory.hpp"
#include "memory/resourceArea.hpp"
#include "memory/universe.hpp"
#include "oops/instanceKlass.hpp"
#include "oops/method.hpp"
#include "oops/oop.inline.hpp"
#include "oops/symbol.hpp"
#include "prims/jvm_misc.hpp"
#include "prims/nativeLookup.hpp"
#include "prims/unsafe.hpp"
#include "runtime/arguments.hpp"
#include "runtime/handles.inline.hpp"
#include "runtime/javaCalls.hpp"
#include "runtime/sharedRuntime.hpp"
#include "runtime/signature.hpp"
#include "utilities/macros.hpp"

static void mangle_name_on(outputStream* st, Symbol* name, int begin, int end) {
  char* bytes = (char*)name->bytes() + begin;
  char* end_bytes = (char*)name->bytes() + end;
  while (bytes < end_bytes) {
    jchar c;
    bytes = UTF8::next(bytes, &c);
    if (c <= 0x7f && isalnum(c)) {
      st->put((char) c);
    } else {
           if (c == '_') st->print("_1");
      else if (c == '/') st->print("_");
      else if (c == ';') st->print("_2");
      else if (c == '[') st->print("_3");
      else               st->print("_%.5x", c);
    }
  }
}

static void mangle_name_on(outputStream* st, Symbol* name) {
  mangle_name_on(st, name, 0, name->utf8_length());
}

char* NativeLookup::pure_jni_name(const methodHandle& method) {
  stringStream st;
  // Prefix
  st.print("Java_");
  // Klass name
  mangle_name_on(&st, method->klass_name());
  st.print("_");
  // Method name
  mangle_name_on(&st, method->name());
  return st.as_string();
}

char* NativeLookup::critical_jni_name(const methodHandle& method) {
  stringStream st;
  // Prefix
  st.print("JavaCritical_");
  // Klass name
  mangle_name_on(&st, method->klass_name());
  st.print("_");
  // Method name
  mangle_name_on(&st, method->name());
  return st.as_string();
}

char* NativeLookup::long_jni_name(const methodHandle& method) {
  // Signature ignore the wrapping parenteses and the trailing return type
  stringStream st;
  Symbol* signature = method->signature();
  st.print("__");
  // find ')'
  int end;
  for (end = 0; end < signature->utf8_length() && signature->byte_at(end) != ')'; end++);
  // skip first '('
  mangle_name_on(&st, signature, 1, end);
  return st.as_string();
}

extern "C" {
  void JNICALL JVM_RegisterMethodHandleMethods(JNIEnv *env, jclass unsafecls);
  void JNICALL JVM_RegisterPerfMethods(JNIEnv *env, jclass perfclass);
  void JNICALL JVM_RegisterWhiteBoxMethods(JNIEnv *env, jclass wbclass);
  jobject  JNICALL JVM_GetJVMCIRuntime(JNIEnv *env, jclass c);
  void     JNICALL JVM_RegisterJVMCINatives(JNIEnv *env, jclass compilerToVMClass);
}

#define CC (char*)  /* cast a literal from (const char*) */
#define FN_PTR(f) CAST_FROM_FN_PTR(void*, &f)

static JNINativeMethod lookup_special_native_methods[] = {
  { CC"Java_jdk_internal_misc_Unsafe_registerNatives",             NULL, FN_PTR(JVM_RegisterJDKInternalMiscUnsafeMethods) },
  { CC"Java_java_lang_invoke_MethodHandleNatives_registerNatives", NULL, FN_PTR(JVM_RegisterMethodHandleMethods) },
  { CC"Java_jdk_internal_perf_Perf_registerNatives",               NULL, FN_PTR(JVM_RegisterPerfMethods) },
  { CC"Java_sun_hotspot_WhiteBox_registerNatives",                 NULL, FN_PTR(JVM_RegisterWhiteBoxMethods) },
  { CC"Java_jdk_vm_ci_runtime_JVMCI_initializeRuntime",            NULL, FN_PTR(JVM_GetJVMCIRuntime) },
  { CC"Java_jdk_vm_ci_hotspot_CompilerToVM_registerNatives",       NULL, FN_PTR(JVM_RegisterJVMCINatives) },
};

static address lookup_special_native(char* jni_name) {
  int count = sizeof(lookup_special_native_methods) / sizeof(JNINativeMethod);
  for (int i = 0; i < count; i++) {
    // NB: To ignore the jni prefix and jni postfix strstr is used matching.
    if (strstr(jni_name, lookup_special_native_methods[i].name) != NULL) {
      return CAST_FROM_FN_PTR(address, lookup_special_native_methods[i].fnPtr);
    }
  }
  return NULL;
}

address NativeLookup::lookup_style(const methodHandle& method, char* pure_name, const char* long_name, int args_size, bool os_style, bool& in_base_library, TRAPS) {
  address entry;
  // Compute complete JNI name for style
  stringStream st;
  if (os_style) os::print_jni_name_prefix_on(&st, args_size);
  st.print_raw(pure_name);
  st.print_raw(long_name);
  if (os_style) os::print_jni_name_suffix_on(&st, args_size);
  char* jni_name = st.as_string();

  // If the loader is null we have a system class, so we attempt a lookup in
  // the native Java library. This takes care of any bootstrapping problems.
  // Note: It is critical for bootstrapping that Java_java_lang_ClassLoader_00024NativeLibrary_find
  // gets found the first time around - otherwise an infinite loop can occure. This is
  // another VM/library dependency
  Handle loader(THREAD, method->method_holder()->class_loader());
  if (loader.is_null()) {
    entry = lookup_special_native(jni_name);
    if (entry == NULL) {
       entry = (address) os::dll_lookup(os::native_java_library(), jni_name);
    }
    if (entry != NULL) {
      in_base_library = true;
      return entry;
    }
  }

  // Otherwise call static method findNative in ClassLoader
  Klass*   klass = SystemDictionary::ClassLoader_klass();
  Handle name_arg = java_lang_String::create_from_str(jni_name, CHECK_NULL);

  JavaValue result(T_LONG);
  JavaCalls::call_static(&result,
                         klass,
                         vmSymbols::findNative_name(),
                         vmSymbols::classloader_string_long_signature(),
                         // Arguments
                         loader,
                         name_arg,
                         CHECK_NULL);
  entry = (address) (intptr_t) result.get_jlong();

  if (entry == NULL) {
    // findNative didn't find it, if there are any agent libraries look in them
    AgentLibrary* agent;
    for (agent = Arguments::agents(); agent != NULL; agent = agent->next()) {
      entry = (address) os::dll_lookup(agent->os_lib(), jni_name);
      if (entry != NULL) {
        return entry;
      }
    }
  }

  return entry;
}

address NativeLookup::lookup_critical_style(const methodHandle& method, char* pure_name, const char* long_name, int args_size, bool os_style) {
  if (!method->has_native_function()) {
    return NULL;
  }

  address current_entry = method->native_function();

  char dll_name[JVM_MAXPATHLEN];
  int offset;
  if (os::dll_address_to_library_name(current_entry, dll_name, sizeof(dll_name), &offset)) {
    char ebuf[32];
    void* dll = os::dll_load(dll_name, ebuf, sizeof(ebuf));
    if (dll != NULL) {
      // Compute complete JNI name for style
      stringStream st;
      if (os_style) os::print_jni_name_prefix_on(&st, args_size);
      st.print_raw(pure_name);
      st.print_raw(long_name);
      if (os_style) os::print_jni_name_suffix_on(&st, args_size);
      char* jni_name = st.as_string();
      address critical_entry = (address)os::dll_lookup(dll, jni_name);
      // Close the handle to avoid keeping the library alive if the native method holder is unloaded.
      // This is fine because the library is still kept alive by JNI (see JVM_LoadLibrary). As soon
      // as the holder class and the library are unloaded (see JVM_UnloadLibrary), the native wrapper
      // that calls 'critical_entry' becomes unreachable and is unloaded as well.
      os::dll_unload(dll);
      return critical_entry;
    }
  }

  return NULL;
}

// Check all the formats of native implementation name to see if there is one
// for the specified method.
address NativeLookup::lookup_entry(const methodHandle& method, bool& in_base_library, TRAPS) {
  address entry = NULL;
  in_base_library = false;
  // Compute pure name
  char* pure_name = pure_jni_name(method);

  // Compute argument size
  int args_size = 1                             // JNIEnv
                + (method->is_static() ? 1 : 0) // class for static methods
                + method->size_of_parameters(); // actual parameters

  // 1) Try JNI short style
  entry = lookup_style(method, pure_name, "",        args_size, true,  in_base_library, CHECK_NULL);
  if (entry != NULL) return entry;

  // Compute long name
  char* long_name = long_jni_name(method);

  // 2) Try JNI long style
  entry = lookup_style(method, pure_name, long_name, args_size, true,  in_base_library, CHECK_NULL);
  if (entry != NULL) return entry;

  // 3) Try JNI short style without os prefix/suffix
  entry = lookup_style(method, pure_name, "",        args_size, false, in_base_library, CHECK_NULL);
  if (entry != NULL) return entry;

  // 4) Try JNI long style without os prefix/suffix
  entry = lookup_style(method, pure_name, long_name, args_size, false, in_base_library, CHECK_NULL);

  return entry; // NULL indicates not found
}

// Check all the formats of native implementation name to see if there is one
// for the specified method.
address NativeLookup::lookup_critical_entry(const methodHandle& method) {
  if (!CriticalJNINatives) return NULL;

  if (method->is_synchronized() || !method->is_static()) {
    // Only static non-synchronized methods are allowed
    return NULL;
  }

  ResourceMark rm;
  address entry = NULL;

  Symbol* signature = method->signature();
  for (int end = 0; end < signature->utf8_length(); end++) {
    if (signature->byte_at(end) == 'L') {
      // Don't allow object types
      return NULL;
    }
  }

  // Compute critical name
  char* critical_name = critical_jni_name(method);

  // Compute argument size
  int args_size = method->size_of_parameters();
  for (SignatureStream ss(signature); !ss.at_return_type(); ss.next()) {
    if (ss.is_array()) {
      args_size += T_INT_size; // array length parameter
    }
  }

  // 1) Try JNI short style
  entry = lookup_critical_style(method, critical_name, "",        args_size, true);
  if (entry != NULL) return entry;

  // Compute long name
  char* long_name = long_jni_name(method);

  // 2) Try JNI long style
  entry = lookup_critical_style(method, critical_name, long_name, args_size, true);
  if (entry != NULL) return entry;

  // 3) Try JNI short style without os prefix/suffix
  entry = lookup_critical_style(method, critical_name, "",        args_size, false);
  if (entry != NULL) return entry;

  // 4) Try JNI long style without os prefix/suffix
  entry = lookup_critical_style(method, critical_name, long_name, args_size, false);

  return entry; // NULL indicates not found
}

// Check if there are any JVM TI prefixes which have been applied to the native method name.
// If any are found, remove them before attemping the look up of the
// native implementation again.
// See SetNativeMethodPrefix in the JVM TI Spec for more details.
address NativeLookup::lookup_entry_prefixed(const methodHandle& method, bool& in_base_library, TRAPS) {
  return NULL;
}

address NativeLookup::lookup_base(const methodHandle& method, bool& in_base_library, TRAPS) {
  address entry = NULL;
  ResourceMark rm(THREAD);

  entry = lookup_entry(method, in_base_library, THREAD);
  if (entry != NULL) return entry;

  // standard native method resolution has failed.  Check if there are any
  // JVM TI prefixes which have been applied to the native method name.
  entry = lookup_entry_prefixed(method, in_base_library, THREAD);
  if (entry != NULL) return entry;

  // Native function not found, throw UnsatisfiedLinkError
  THROW_MSG_0(vmSymbols::java_lang_UnsatisfiedLinkError(),
              method->name_and_sig_as_C_string());
}

address NativeLookup::lookup(const methodHandle& method, bool& in_base_library, TRAPS) {
  if (!method->has_native_function()) {
    address entry = lookup_base(method, in_base_library, CHECK_NULL);
    method->set_native_function(entry,
      Method::native_bind_event_is_interesting);
    // -verbose:jni printing
    if (PrintJNIResolving) {
      ResourceMark rm(THREAD);
      tty->print_cr("[Dynamic-linking native method %s.%s ... JNI]",
        method->method_holder()->external_name(),
        method->name()->as_C_string());
    }
  }
  return method->native_function();
}

address NativeLookup::base_library_lookup(const char* class_name, const char* method_name, const char* signature) {
  EXCEPTION_MARK;
  bool in_base_library = true;  // SharedRuntime inits some math methods.
  TempNewSymbol c_name = SymbolTable::new_symbol(class_name,  CATCH);
  TempNewSymbol m_name = SymbolTable::new_symbol(method_name, CATCH);
  TempNewSymbol s_name = SymbolTable::new_symbol(signature,   CATCH);

  // Find the class
  Klass* k = SystemDictionary::resolve_or_fail(c_name, true, CATCH);
  InstanceKlass* klass  = InstanceKlass::cast(k);

  // Find method and invoke standard lookup
  methodHandle method (THREAD,
                       klass->uncached_lookup_method(m_name, s_name, Klass::find_overpass));
  address result = lookup(method, in_base_library, CATCH);
  guarantee(result != NULL, "must be non NULL");
  return result;
}
