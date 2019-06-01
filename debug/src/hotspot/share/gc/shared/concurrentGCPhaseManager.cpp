#include "precompiled.hpp"

#include "gc/shared/concurrentGCPhaseManager.hpp"
#include "runtime/mutexLocker.hpp"
#include "runtime/thread.hpp"

ConcurrentGCPhaseManager::Stack::Stack() :
  _requested_phase(UNCONSTRAINED_PHASE),
  _top(NULL)
{ }

ConcurrentGCPhaseManager::ConcurrentGCPhaseManager(int phase, Stack* stack) :
  _phase(phase),
  _active(true),
  _prev(NULL),
  _stack(stack) {
  MonitorLockerEx ml(CGCPhaseManager_lock, Mutex::_no_safepoint_check_flag);
  if (stack->_top != NULL) {
    _prev = stack->_top;
  }
  stack->_top = this;
  ml.notify_all();
}

ConcurrentGCPhaseManager::~ConcurrentGCPhaseManager() {
  MonitorLockerEx ml(CGCPhaseManager_lock, Mutex::_no_safepoint_check_flag);
  wait_when_requested_impl();
  _stack->_top = _prev;
  ml.notify_all();
}

bool ConcurrentGCPhaseManager::is_requested() const {
  MonitorLockerEx ml(CGCPhaseManager_lock, Mutex::_no_safepoint_check_flag);
  return _active && (_stack->_requested_phase == _phase);
}

bool ConcurrentGCPhaseManager::wait_when_requested_impl() const {
  bool waited = false;
  while (_active && (_stack->_requested_phase == _phase)) {
    waited = true;
    CGCPhaseManager_lock->wait(Mutex::_no_safepoint_check_flag);
  }
  return waited;
}

bool ConcurrentGCPhaseManager::wait_when_requested() const {
  MonitorLockerEx ml(CGCPhaseManager_lock, Mutex::_no_safepoint_check_flag);
  return wait_when_requested_impl();
}

void ConcurrentGCPhaseManager::set_phase(int phase, bool force) {
  MonitorLockerEx ml(CGCPhaseManager_lock, Mutex::_no_safepoint_check_flag);
  if (!force) wait_when_requested_impl();
  _phase = phase;
  ml.notify_all();
}

void ConcurrentGCPhaseManager::deactivate() {
  MonitorLockerEx ml(CGCPhaseManager_lock, Mutex::_no_safepoint_check_flag);
  _active = false;
  ml.notify_all();
}

bool ConcurrentGCPhaseManager::wait_for_phase(int phase, Stack* stack) {
  MonitorLockerEx ml(CGCPhaseManager_lock);
  // Update request and notify service of change.
  if (stack->_requested_phase != phase) {
    stack->_requested_phase = phase;
    ml.notify_all();
  }

  if (phase == UNCONSTRAINED_PHASE) {
    return true;
  }

  // Wait until phase or IDLE is active.
  while (true) {
    bool idle = false;
    for (ConcurrentGCPhaseManager* manager = stack->_top; manager != NULL; manager = manager->_prev) {
      if (manager->_phase == phase) {
        return true;            // phase is active.
      } else if (manager->_phase == IDLE_PHASE) {
        idle = true;            // Note idle active, continue search for phase.
      }
    }
    if (idle) {
      return false;             // idle is active and phase is not.
    } else {
      ml.wait();                // Wait for phase change.
    }
  }
}
