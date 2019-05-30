#ifndef SHARE_VM_GC_G1_G1YOUNGREMSETSAMPLINGTHREAD_HPP
#define SHARE_VM_GC_G1_G1YOUNGREMSETSAMPLINGTHREAD_HPP

#include "gc/shared/concurrentGCThread.hpp"

// The G1YoungRemSetSamplingThread is used to re-assess the validity of
// the prediction for the remembered set lengths of the young generation.
//
// At the end of the GC G1 determines the length of the young gen based on
// how much time the next GC can take, and when the next GC may occur
// according to the MMU.
//
// The assumption is that a significant part of the GC is spent on scanning
// the remembered sets (and many other components), so this thread constantly
// reevaluates the prediction for the remembered set scanning costs, and potentially
// G1Policy resizes the young gen. This may do a premature GC or even
// increase the young gen size to keep pause time length goal.
class G1YoungRemSetSamplingThread: public ConcurrentGCThread {
private:
  Monitor _monitor;

  void sample_young_list_rs_lengths();

  void run_service();
  void stop_service();

  void sleep_before_next_cycle();

  double _vtime_accum;  // Accumulated virtual time.

public:
  G1YoungRemSetSamplingThread();
  double vtime_accum() { return _vtime_accum; }
};

#endif
