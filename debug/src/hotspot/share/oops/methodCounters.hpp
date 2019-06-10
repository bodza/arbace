#ifndef SHARE_VM_OOPS_METHODCOUNTERS_HPP
#define SHARE_VM_OOPS_METHODCOUNTERS_HPP

#include "oops/metadata.hpp"
#include "compiler/compilerDefinitions.hpp"
#include "compiler/compilerOracle.hpp"
#include "interpreter/invocationCounter.hpp"
#include "runtime/arguments.hpp"
#include "utilities/align.hpp"

class MethodCounters : public Metadata {
 friend class VMStructs;
 friend class JVMCIVMStructs;
 private:
  // If you add a new field that points to any metaspace object, you
  // must add this field to MethodCounters::metaspace_pointers_do().
  int               _interpreter_invocation_count; // Count of times invoked (reused as prev_event_count in tiered)
  u2                _interpreter_throwout_count; // Count of times method was exited via exception while interpreting
  InvocationCounter _invocation_counter;         // Incremented before each activation of the method - used to trigger frequency-based optimizations
  InvocationCounter _backedge_counter;           // Incremented before each backedge taken - used to trigger frequencey-based optimizations
  // NMethod age is a counter for warm methods detection in the code cache sweeper.
  // The counter is reset by the sweeper and is decremented by some of the compiled
  // code. The counter values are interpreted as follows:
  // 1. (HotMethodDetection..INT_MAX] - initial value, no counters inserted
  // 2. [1..HotMethodDetectionLimit)  - the method is warm, the counter is used
  //                                    to figure out which methods can be flushed.
  // 3. (INT_MIN..0]                  - method is hot and will deopt and get
  //                                    recompiled without the counters
  int               _nmethod_age;
  int               _interpreter_invocation_limit;        // per-method InterpreterInvocationLimit
  int               _invoke_mask;                         // per-method Tier0InvokeNotifyFreqLog
  int               _backedge_mask;                       // per-method Tier0BackedgeNotifyFreqLog

  MethodCounters(const methodHandle& mh) : _nmethod_age(INT_MAX)
  {
    set_interpreter_invocation_count(0);
    set_interpreter_throwout_count(0);
    invocation_counter()->init();
    backedge_counter()->init();

    // Set per-method thresholds.
    double scale = 1.0;
    CompilerOracle::has_option_value(mh, "CompileThresholdScaling", scale);

    int compile_threshold = CompilerConfig::scaled_compile_threshold(CompileThreshold, scale);
    _interpreter_invocation_limit = compile_threshold << InvocationCounter::count_shift;
    _invoke_mask = right_n_bits(CompilerConfig::scaled_freq_log(Tier0InvokeNotifyFreqLog, scale)) << InvocationCounter::count_shift;
    _backedge_mask = right_n_bits(CompilerConfig::scaled_freq_log(Tier0BackedgeNotifyFreqLog, scale)) << InvocationCounter::count_shift;
  }

 public:
  virtual bool is_methodCounters() const volatile { return true; }

  static MethodCounters* allocate(const methodHandle& mh, TRAPS);

  void deallocate_contents(ClassLoaderData* loader_data) { }

  static int method_counters_size() {
    return align_up((int)sizeof(MethodCounters), wordSize) / wordSize;
  }
  virtual int size() const {
    return method_counters_size();
  }
  void metaspace_pointers_do(MetaspaceClosure* it);
  MetaspaceObj::Type type() const { return MethodCountersType; }
  void clear_counters();

  int interpreter_invocation_count() {
    return _interpreter_invocation_count;
  }
  void set_interpreter_invocation_count(int count) {
    _interpreter_invocation_count = count;
  }
  int increment_interpreter_invocation_count() {
    return ++_interpreter_invocation_count;
  }

  void interpreter_throwout_increment() {
    if (_interpreter_throwout_count < 65534) {
      _interpreter_throwout_count++;
    }
  }
  int  interpreter_throwout_count() const {
    return _interpreter_throwout_count;
  }
  void set_interpreter_throwout_count(int count) {
    _interpreter_throwout_count = count;
  }

  int highest_comp_level() const;
  void set_highest_comp_level(int level);

  // invocation counter
  InvocationCounter* invocation_counter() { return &_invocation_counter; }
  InvocationCounter* backedge_counter()   { return &_backedge_counter; }

  int nmethod_age() {
    return _nmethod_age;
  }
  void set_nmethod_age(int age) {
    _nmethod_age = age;
  }
  void reset_nmethod_age() {
    set_nmethod_age(HotMethodDetectionLimit);
  }

  static bool is_nmethod_hot(int age)       { return age <= 0; }
  static bool is_nmethod_warm(int age)      { return age < HotMethodDetectionLimit; }
  static bool is_nmethod_age_unset(int age) { return age > HotMethodDetectionLimit; }

  static ByteSize nmethod_age_offset()                        { return byte_offset_of(MethodCounters, _nmethod_age); }
  static ByteSize interpreter_invocation_counter_offset()     { return byte_offset_of(MethodCounters, _interpreter_invocation_count); }
  static int interpreter_invocation_counter_offset_in_bytes() { return offset_of(MethodCounters, _interpreter_invocation_count); }
  static ByteSize invocation_counter_offset()                 { return byte_offset_of(MethodCounters, _invocation_counter); }
  static ByteSize backedge_counter_offset()                   { return byte_offset_of(MethodCounters, _backedge_counter); }
  static ByteSize interpreter_invocation_limit_offset()       { return byte_offset_of(MethodCounters, _interpreter_invocation_limit); }
  static ByteSize invoke_mask_offset()                        { return byte_offset_of(MethodCounters, _invoke_mask); }
  static ByteSize backedge_mask_offset()                      { return byte_offset_of(MethodCounters, _backedge_mask); }

  virtual const char* internal_name() const { return "{method counters}"; }
  virtual void print_value_on(outputStream* st) const;
};
#endif
