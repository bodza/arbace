#ifndef SHARE_VM_GC_G1_G1CONCURRENTMARKTHREAD_HPP
#define SHARE_VM_GC_G1_G1CONCURRENTMARKTHREAD_HPP

#include "gc/shared/concurrentGCPhaseManager.hpp"
#include "gc/shared/concurrentGCThread.hpp"

class G1ConcurrentMark;
class G1Policy;

// The concurrent mark thread triggers the various steps of the concurrent marking
// cycle, including various marking cleanup.
class G1ConcurrentMarkThread: public ConcurrentGCThread {
  friend class VMStructs;

  double _vtime_start;  // Initial virtual time.
  double _vtime_accum;  // Accumulated virtual time.
  double _vtime_mark_accum;

  G1ConcurrentMark* _cm;

  enum State {
    Idle,
    Started,
    InProgress
  };

  volatile State _state;

  ConcurrentGCPhaseManager::Stack _phase_manager_stack;

  void sleep_before_next_cycle();
  // Delay marking to meet MMU.
  void delay_to_keep_mmu(G1Policy* g1_policy, bool remark);
  double mmu_sleep_time(G1Policy* g1_policy, bool remark);

  void run_service();
  void stop_service();

 public:
  // Constructor
  G1ConcurrentMarkThread(G1ConcurrentMark* cm);

  // Total virtual time so far for this thread and concurrent marking tasks.
  double vtime_accum();
  // Marking virtual time so far this thread and concurrent marking tasks.
  double vtime_mark_accum();

  G1ConcurrentMark* cm()   { return _cm; }

  void set_idle()          { _state = Idle; }
  bool idle()              { return _state == Idle; }
  void set_started()       { _state = Started; }
  bool started()           { return _state == Started; }
  void set_in_progress()   { _state = InProgress; }
  bool in_progress()       { return _state == InProgress; }

  // Returns true from the moment a marking cycle is
  // initiated (during the initial-mark pause when started() is set)
  // to the moment when the cycle completes (just after the next
  // marking bitmap has been cleared and in_progress() is
  // cleared). While during_cycle() is true we will not start another cycle
  // so that cycles do not overlap. We cannot use just in_progress()
  // as the CM thread might take some time to wake up before noticing
  // that started() is set and set in_progress().
  bool during_cycle()      { return !idle(); }

  ConcurrentGCPhaseManager::Stack* phase_manager_stack() {
    return &_phase_manager_stack;
  }
};

#endif
