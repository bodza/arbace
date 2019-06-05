#ifndef SHARE_VM_GC_CONCURRENTGCPHASEMANAGER_HPP
#define SHARE_VM_GC_CONCURRENTGCPHASEMANAGER_HPP

#include "memory/allocation.hpp"

// Manage concurrent phase information.
// Managers are stack allocated.  Managers may be nested, to support
// nested subphases.
class ConcurrentGCPhaseManager : public StackObj {
public:
  // Special phase ids used by all GC's that use this facility.
  static const int UNCONSTRAINED_PHASE = 0; // Unconstrained or no request.
  static const int IDLE_PHASE = 1;          // Concurrent processing is idle.

  // Stack of phase managers.
  class Stack {
    friend class ConcurrentGCPhaseManager;

  public:
    // Create an empty stack of phase managers.
    Stack();

  private:
    int _requested_phase;
    ConcurrentGCPhaseManager* _top;

    // Non-copyable - never defined.
    Stack(const Stack&);
    Stack& operator=(const Stack&);
  };

  // Construct and push a new manager on the stack, activating phase.
  // Notifies callers in wait_for_phase of the phase change.
  //
  // Preconditions:
  // - Calling thread must be a ConcurrentGC thread
  // - phase != UNCONSTRAINED_PHASE
  // - stack != NULL
  // - other managers on stack must all be active.
  ConcurrentGCPhaseManager(int phase, Stack* stack);

  // Pop this manager off the stack, deactivating phase.  Before
  // changing phases, if is_requested() is true, wait until the
  // request is changed.  After changing phases, notifies callers of
  // wait_for_phase of the phase change.
  //
  // Preconditions:
  // - Calling thread must be a ConcurrentGC thread
  // - this must be the current top of the manager stack
  ~ConcurrentGCPhaseManager();

  // Returns true if this phase is active and is currently requested.
  //
  // Preconditions:
  // - Calling thread must be a ConcurrentGC thread
  // - this must be the current top of manager stack
  bool is_requested() const;

  // Wait until is_requested() is false.  Returns true if waited.
  //
  // Preconditions:
  // - Calling thread must be a ConcurrentGC thread
  // - this must be the current top of manager stack
  bool wait_when_requested() const;

  // Directly step from one phase to another, without needing to pop a
  // manager from the stack and allocate a new one.  Before changing
  // phases, if is_requested() is true and force is false, wait until
  // the request is changed.  After changing phases, notifies callers
  // of wait_for_phase of the phase change.
  //
  // Preconditions:
  // - Calling thread must be a ConcurrentGC thread
  // - phase != UNCONSTRAINED_PHASE
  // - this must be the current top of manager stack
  void set_phase(int phase, bool force);

  // Deactivate the manager.  An inactive manager no longer blocks
  // transitions out of the associated phase when that phase has been
  // requested.
  //
  // Preconditions:
  // - Calling thread must be a ConcurrentGC thread
  // - this must be the current top of manager stack
  void deactivate();

  // Updates request to the new phase, and notifies threads blocked on
  // the old request of the change.  Returns true if the phase is
  // UNCONSTRAINED_PHASE.  Otherwise, waits until an active phase is
  // the requested phase (returning true) or IDLE_PHASE (returning
  // false if not also the requested phase).
  //
  // Preconditions:
  // - Calling thread must be a Java thread
  // - stack must be non-NULL
  static bool wait_for_phase(int phase, Stack* stack);

private:
  int _phase;
  bool _active;
  ConcurrentGCPhaseManager* _prev;
  Stack* _stack;

  // Non-copyable - never defined.
  ConcurrentGCPhaseManager(const ConcurrentGCPhaseManager&);
  ConcurrentGCPhaseManager& operator=(const ConcurrentGCPhaseManager&);

  bool wait_when_requested_impl() const;
};

#endif
