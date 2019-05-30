#ifndef SHARE_VM_PRIMS_NATIVELOOKUP_HPP
#define SHARE_VM_PRIMS_NATIVELOOKUP_HPP

#include "memory/allocation.hpp"
#include "runtime/handles.hpp"

// NativeLookup provides an interface for finding DLL entry points for
// Java native functions.

class NativeLookup : AllStatic {
 private:
  // JNI name computation
  static char* pure_jni_name(const methodHandle& method);
  static char* long_jni_name(const methodHandle& method);
  static char* critical_jni_name(const methodHandle& method);

  // Style specific lookup
  static address lookup_style(const methodHandle& method, char* pure_name, const char* long_name, int args_size, bool os_style, bool& in_base_library, TRAPS);
  static address lookup_critical_style(const methodHandle& method, char* pure_name, const char* long_name, int args_size, bool os_style);
  static address lookup_base (const methodHandle& method, bool& in_base_library, TRAPS);
  static address lookup_entry(const methodHandle& method, bool& in_base_library, TRAPS);
  static address lookup_entry_prefixed(const methodHandle& method, bool& in_base_library, TRAPS);
 public:
  // Lookup native function. May throw UnsatisfiedLinkError.
  static address lookup(const methodHandle& method, bool& in_base_library, TRAPS);
  static address lookup_critical_entry(const methodHandle& method);

  // Lookup native functions in base library.
  static address base_library_lookup(const char* class_name, const char* method_name, const char* signature);
};

#endif
