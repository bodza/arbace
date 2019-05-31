#include "precompiled.hpp"
#include "gc/shared/suspendibleThreadSet.hpp"
#include "runtime/mutexLocker.hpp"
#include "runtime/semaphore.hpp"
#include "runtime/thread.inline.hpp"

uint   SuspendibleThreadSet::_nthreads          = 0;
uint   SuspendibleThreadSet::_nthreads_stopped  = 0;
bool   SuspendibleThreadSet::_suspend_all       = false;
double SuspendibleThreadSet::_suspend_all_start = 0.0;

static Semaphore* _synchronize_wakeup = NULL;

void SuspendibleThreadSet_init() {
  _synchronize_wakeup = new Semaphore();
}

bool SuspendibleThreadSet::is_synchronized() {
  return _nthreads_stopped == _nthreads;
}

void SuspendibleThreadSet::join() {
  MonitorLockerEx ml(STS_lock, Mutex::_no_safepoint_check_flag);
  while (_suspend_all) {
    ml.wait(Mutex::_no_safepoint_check_flag);
  }
  _nthreads++;
}

void SuspendibleThreadSet::leave() {
  MonitorLockerEx ml(STS_lock, Mutex::_no_safepoint_check_flag);
  _nthreads--;
  if (_suspend_all && is_synchronized()) {
    // This leave completes a request, so inform the requestor.
    _synchronize_wakeup->signal();
  }
}

void SuspendibleThreadSet::yield() {
  MonitorLockerEx ml(STS_lock, Mutex::_no_safepoint_check_flag);
  if (_suspend_all) {
    _nthreads_stopped++;
    if (is_synchronized()) {
      if (ConcGCYieldTimeout > 0) {
        double now = os::elapsedTime();
        guarantee((now - _suspend_all_start) * 1000.0 < (double)ConcGCYieldTimeout, "Long delay");
      }
      // This yield completes the request, so inform the requestor.
      _synchronize_wakeup->signal();
    }
    while (_suspend_all) {
      ml.wait(Mutex::_no_safepoint_check_flag);
    }
    _nthreads_stopped--;
  }
}

void SuspendibleThreadSet::synchronize() {
  if (ConcGCYieldTimeout > 0) {
    _suspend_all_start = os::elapsedTime();
  }
  {
    MonitorLockerEx ml(STS_lock, Mutex::_no_safepoint_check_flag);
    _suspend_all = true;
    if (is_synchronized()) {
      return;
    }
  }

  // Semaphore initial count is zero.  To reach here, there must be at
  // least one not yielded thread in the set, e.g. is_synchronized()
  // was false before the lock was released.  A thread in the set will
  // signal the semaphore iff it is the last to yield or leave while
  // there is an active suspend request.  So there will be exactly one
  // signal, which will increment the semaphore count to one, which
  // will then be consumed by this wait, returning it to zero.  No
  // thread can exit yield or enter the set until desynchronize is
  // called, so there are no further opportunities for the semaphore
  // being signaled until we get back here again for some later
  // synchronize call.  Hence, there is no need to re-check for
  // is_synchronized after the wait; it will always be true there.
  _synchronize_wakeup->wait();
}

void SuspendibleThreadSet::desynchronize() {
  MonitorLockerEx ml(STS_lock, Mutex::_no_safepoint_check_flag);
  _suspend_all = false;
  ml.notify_all();
}
