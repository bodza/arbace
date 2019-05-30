#ifndef SHARE_VM_GC_G1_G1CONCURRENTMARKTHREAD_INLINE_HPP
#define SHARE_VM_GC_G1_G1CONCURRENTMARKTHREAD_INLINE_HPP

#include "gc/g1/g1ConcurrentMark.hpp"
#include "gc/g1/g1ConcurrentMarkThread.hpp"

  // Total virtual time so far.
inline double G1ConcurrentMarkThread::vtime_accum() {
  return _vtime_accum + _cm->all_task_accum_vtime();
}

// Marking virtual time so far
inline double G1ConcurrentMarkThread::vtime_mark_accum() {
  return _vtime_mark_accum + _cm->all_task_accum_vtime();
}

#endif
