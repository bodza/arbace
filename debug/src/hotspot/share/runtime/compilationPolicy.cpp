#include "precompiled.hpp"

#include "classfile/classLoaderData.inline.hpp"
#include "code/compiledIC.hpp"
#include "code/nmethod.hpp"
#include "code/scopeDesc.hpp"
#include "memory/resourceArea.hpp"
#include "oops/methodData.hpp"
#include "oops/method.inline.hpp"
#include "oops/oop.inline.hpp"
#include "prims/nativeLookup.hpp"
#include "runtime/compilationPolicy.hpp"
#include "runtime/frame.hpp"
#include "runtime/handles.inline.hpp"
#include "runtime/rframe.hpp"
#include "runtime/stubRoutines.hpp"
#include "runtime/thread.hpp"
#include "runtime/timer.hpp"
#include "runtime/vframe.hpp"
#include "runtime/vm_operations.hpp"
#include "utilities/globalDefinitions.hpp"

CompilationPolicy* CompilationPolicy::_policy;
elapsedTimer       CompilationPolicy::_accumulated_time;
bool               CompilationPolicy::_in_vm_startup;

// Determine compilation policy based on command line argument
void compilationPolicy_init() {
  CompilationPolicy::set_in_vm_startup(DelayCompilationDuringStartup);

  switch (CompilationPolicyChoice) {
  case 0:
    CompilationPolicy::set_policy(new SimpleCompPolicy());
    break;

  case 1:
    Unimplemented();
    break;
  case 2:
    Unimplemented();
    break;
  default:
    fatal("CompilationPolicyChoice must be in the range: [0-2]");
  }
  CompilationPolicy::policy()->initialize();
}

void CompilationPolicy::completed_vm_startup() {
  _in_vm_startup = false;
}

// Returns true if m must be compiled before executing it
bool CompilationPolicy::must_be_compiled(const methodHandle& m, int comp_level) {
  if (m->has_compiled_code()) return false; // already compiled
  if (!can_be_compiled(m, comp_level)) return false;

  return true; // must compile all methods
}

void CompilationPolicy::compile_if_required(const methodHandle& selected_method, TRAPS) {
  if (must_be_compiled(selected_method)) {
    // This path is unusual, mostly used by the '-Xcomp' stress test mode.

    if (!THREAD->can_call_java() || THREAD->is_Compiler_thread()) {
      // don't force compilation, resolve was on behalf of compiler
      return;
    }
    if (selected_method->method_holder()->is_not_initialized()) {
      // 'is_not_initialized' means not only '!is_initialized', but also that
      // initialization has not been started yet ('!being_initialized')
      // Do not force compilation of methods in uninitialized classes.
      // Note that doing this would throw an assert later,
      // in CompileBroker::compile_method.
      // We sometimes use the link resolver to do reflective lookups
      // even before classes are initialized.
      return;
    }
    CompileBroker::compile_method(selected_method, CompilationPolicy::policy()->initial_compile_level(), methodHandle(), 0, CompileTask::Reason_MustBeCompiled, CHECK);
  }
}

// Returns true if m is allowed to be compiled
bool CompilationPolicy::can_be_compiled(const methodHandle& m, int comp_level) {
  if (m->is_abstract()) return false;
  if (DontCompileHugeMethods && m->code_size() > HugeMethodLimit) return false;

  if (comp_level == CompLevel_all) {
    // must be compilable at available level for non-tiered
    return !m->is_not_compilable(CompLevel_highest_tier);
  } else if (is_compile(comp_level)) {
    return !m->is_not_compilable(comp_level);
  }
  return false;
}

bool CompilationPolicy::is_compilation_enabled() {
  // NOTE: CompileBroker::should_compile_new_jobs() checks for UseCompiler
  return !delay_compilation_during_startup() && CompileBroker::should_compile_new_jobs();
}

CompileTask* CompilationPolicy::select_task_helper(CompileQueue* compile_queue) {
  return compile_queue->first();
}

void NonTieredCompPolicy::initialize() {
  // Setup the compiler thread numbers
  if (CICompilerCountPerCPU) {
    // Example: if CICompilerCountPerCPU is true, then we get
    // max(log2(8)-1,1) = 2 compiler threads on an 8-way machine.
    // May help big-app startup time.
    _compiler_count = MAX2(log2_int(os::active_processor_count())-1,1);
    FLAG_SET_ERGO(intx, CICompilerCount, _compiler_count);
  } else {
    _compiler_count = CICompilerCount;
  }
}

// compiler_count() behaves the following way:
// - with TIERED build (with both COMPILER1 and COMPILER2 defined) it should return
//   zero for the c1 compilation levels in server compilation mode runs
//   and c2 compilation levels in client compilation mode runs.
// - with COMPILER2 not defined it should return zero for c2 compilation levels.
// - with COMPILER1 not defined it should return zero for c1 compilation levels.
// - if neither is defined - always return zero.
int NonTieredCompPolicy::compiler_count(CompLevel comp_level) {
  if (is_client_compilation_mode_vm() && is_c1_compile(comp_level)) {
    return _compiler_count;
  }
  return 0;
}

void NonTieredCompPolicy::reset_counter_for_invocation_event(const methodHandle& m) {
  // Make sure invocation and backedge counter doesn't overflow again right away
  // as would be the case for native methods.

  // BUT also make sure the method doesn't look like it was never executed.
  // Set carry bit and reduce counter's value to min(count, CompileThreshold/2).
  MethodCounters* mcs = m->method_counters();
  mcs->invocation_counter()->set_carry();
  mcs->backedge_counter()->set_carry();
}

void NonTieredCompPolicy::reset_counter_for_back_branch_event(const methodHandle& m) {
  // Delay next back-branch event but pump up invocation counter to trigger
  // whole method compilation.
  MethodCounters* mcs = m->method_counters();
  InvocationCounter* i = mcs->invocation_counter();
  InvocationCounter* b = mcs->backedge_counter();

  // Don't set invocation_counter's value too low otherwise the method will
  // look like immature (ic < ~5300) which prevents the inlining based on
  // the type profiling.
  i->set(i->state(), CompileThreshold);
  // Don't reset counter too low - it is used to check if OSR method is ready.
  b->set(b->state(), CompileThreshold / 2);
}

//
// CounterDecay
//
// Iterates through invocation counters and decrements them. This
// is done at each safepoint.
//
class CounterDecay : public AllStatic {
  static jlong _last_timestamp;
  static void do_method(Method* m) {
    MethodCounters* mcs = m->method_counters();
    if (mcs != NULL) {
      mcs->invocation_counter()->decay();
    }
  }
public:
  static void decay();
  static bool is_decay_needed() {
    return (os::javaTimeMillis() - _last_timestamp) > CounterDecayMinIntervalLength;
  }
};

jlong CounterDecay::_last_timestamp = 0;

void CounterDecay::decay() {
  _last_timestamp = os::javaTimeMillis();

  // This operation is going to be performed only at the end of a safepoint
  // and hence GC's will not be going on, all Java mutators are suspended
  // at this point and hence SystemDictionary_lock is also not needed.
  size_t nclasses = ClassLoaderDataGraph::num_instance_classes();
  size_t classes_per_tick = nclasses * (CounterDecayMinIntervalLength * 1e-3 / CounterHalfLifeTime);
  for (size_t i = 0; i < classes_per_tick; i++) {
    InstanceKlass* k = ClassLoaderDataGraph::try_get_next_class();
    if (k != NULL) {
      k->methods_do(do_method);
    }
  }
}

// Called at the end of the safepoint
void NonTieredCompPolicy::do_safepoint_work() {
  if (UseCounterDecay && CounterDecay::is_decay_needed()) {
    CounterDecay::decay();
  }
}

// This method can be called by any component of the runtime to notify the policy
// that it's recommended to delay the compilation of this method.
void NonTieredCompPolicy::delay_compilation(Method* method) {
  MethodCounters* mcs = method->method_counters();
  if (mcs != NULL) {
    mcs->invocation_counter()->decay();
    mcs->backedge_counter()->decay();
  }
}

void NonTieredCompPolicy::disable_compilation(Method* method) {
  MethodCounters* mcs = method->method_counters();
  if (mcs != NULL) {
    mcs->invocation_counter()->set_state(InvocationCounter::wait_for_nothing);
    mcs->backedge_counter()->set_state(InvocationCounter::wait_for_nothing);
  }
}

CompileTask* NonTieredCompPolicy::select_task(CompileQueue* compile_queue) {
  return select_task_helper(compile_queue);
}

bool NonTieredCompPolicy::is_mature(Method* method) {
  MethodData* mdo = method->method_data();
  uint current = mdo->mileage_of(method);
  uint initial = mdo->creation_mileage();
  if (current < initial)
    return true;  // some sort of overflow
  uint target;
  if (ProfileMaturityPercentage <= 0)
    target = (uint) -ProfileMaturityPercentage;  // absolute value
  else
    target = (uint)((ProfileMaturityPercentage * CompileThreshold) / 100);
  return (current >= initial + target);
}

nmethod* NonTieredCompPolicy::event(const methodHandle& method, const methodHandle& inlinee, int branch_bci, int bci, CompLevel comp_level, CompiledMethod* nm, JavaThread* thread) {
  if (bci == InvocationEntryBci) {
    // when code cache is full, compilation gets switched off, UseCompiler
    // is set to false
    if (!method->has_compiled_code() && UseCompiler) {
      method_invocation_event(method, thread);
    } else {
      // Force counter overflow on method entry, even if no compilation
      // happened.  (The method_invocation_event call does this also.)
      reset_counter_for_invocation_event(method);
    }
    // compilation at an invocation overflow no longer goes and retries test for
    // compiled method. We always run the loser of the race as interpreted.
    // so return NULL
    return NULL;
  } else {
    // when code cache is full, we should not compile any more...
    reset_counter_for_back_branch_event(method);
    return NULL;
  }
  return NULL;
}

// SimpleCompPolicy - compile current method

void SimpleCompPolicy::method_invocation_event(const methodHandle& m, JavaThread* thread) {
  const int comp_level = CompLevel_highest_tier;
  const int hot_count = m->invocation_count();
  reset_counter_for_invocation_event(m);

  if (is_compilation_enabled() && can_be_compiled(m, comp_level)) {
    CompiledMethod* nm = m->code();
    if (nm == NULL ) {
      CompileBroker::compile_method(m, comp_level, m, hot_count, CompileTask::Reason_InvocationCount, thread);
    }
  }
}
