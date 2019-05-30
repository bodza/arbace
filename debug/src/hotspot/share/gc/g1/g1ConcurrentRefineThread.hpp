#ifndef SHARE_VM_GC_G1_G1CONCURRENTREFINETHREAD_HPP
#define SHARE_VM_GC_G1_G1CONCURRENTREFINETHREAD_HPP

#include "gc/g1/dirtyCardQueue.hpp"
#include "gc/shared/concurrentGCThread.hpp"

// Forward Decl.
class CardTableEntryClosure;
class G1ConcurrentRefine;

// One or more G1 Concurrent Refinement Threads may be active if concurrent
// refinement is in progress.
class G1ConcurrentRefineThread: public ConcurrentGCThread {
  friend class VMStructs;
  friend class G1CollectedHeap;

  double _vtime_start;  // Initial virtual time.
  double _vtime_accum;  // Accumulated virtual time.
  uint _worker_id;
  uint _worker_id_offset;

  bool _active;
  Monitor* _monitor;
  G1ConcurrentRefine* _cr;

  void wait_for_completed_buffers();

  void set_active(bool x) { _active = x; }
  // Deactivate this thread.
  void deactivate();

  bool is_primary() { return (_worker_id == 0); }

  void run_service();
  void stop_service();
public:
  G1ConcurrentRefineThread(G1ConcurrentRefine* cg1r, uint worker_id);

  bool is_active();
  // Activate this thread.
  void activate();

  // Total virtual time so far.
  double vtime_accum() { return _vtime_accum; }
};

#endif
