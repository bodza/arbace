#ifndef SHARE_VM_RUNTIME_COMPILERRUNTIME_HPP
#define SHARE_VM_RUNTIME_COMPILERRUNTIME_HPP

#include "memory/allocation.hpp"
#include "memory/resourceArea.hpp"
#include "oops/klass.hpp"
#include "oops/method.hpp"
#include "utilities/exceptions.hpp"

class CompilerRuntime : AllStatic {
 public:
  // Resolves klass for aot compiled method.
  static Klass* resolve_klass_helper(JavaThread *thread, const char* name, int len, TRAPS);
  // Resolves method for aot compiled method.
  static Method* resolve_method_helper(Klass* klass, const char* method_name, int method_name_len, const char* signature_name, int signature_name_len);
  // Resolution methods for aot compiled code.
  static void resolve_string_by_symbol(JavaThread *thread, void* string_result, const char* name);
  static void resolve_dynamic_invoke(JavaThread *thread, oop* appendix_result);

  static Klass* resolve_klass_by_symbol(JavaThread *thread, Klass** klass_result, const char* name);
  static Klass* initialize_klass_by_symbol(JavaThread *thread, Klass** klass_result, const char* name);
  static MethodCounters* resolve_method_by_symbol_and_load_counters(JavaThread *thread, MethodCounters** counters_result, Klass* klass_hint, const char* data);
  static void invocation_event(JavaThread *thread, MethodCounters* counters);
  static void backedge_event(JavaThread *thread, MethodCounters* counters, int branch_bci, int target_bci);
};

#endif
