#include "precompiled.hpp"
#include "classfile/systemDictionary.hpp"
#include "gc/shared/concurrentGCThread.hpp"
#include "oops/instanceRefKlass.hpp"
#include "oops/oop.inline.hpp"
#include "runtime/init.hpp"
#include "runtime/java.hpp"
#include "runtime/javaCalls.hpp"
#include "runtime/os.hpp"

ConcurrentGCThread::ConcurrentGCThread() :
  _should_terminate(false), _has_terminated(false) {
};

void ConcurrentGCThread::create_and_start(ThreadPriority prio) {
  if (os::create_thread(this, os::cgc_thread)) {
    // XXX: need to set this to low priority
    // unless "aggressive mode" set; priority
    // should be just less than that of VMThread.
    os::set_priority(this, prio);
    if (!_should_terminate) {
      os::start_thread(this);
    }
  }
}

void ConcurrentGCThread::initialize_in_thread() {
  this->initialize_named_thread();
  this->set_active_handles(JNIHandleBlock::allocate_block());
  // From this time Thread::current() should be working.
  assert(this == Thread::current(), "just checking");
}

void ConcurrentGCThread::wait_for_universe_init() {
  MutexLockerEx x(CGC_lock, Mutex::_no_safepoint_check_flag);
  while (!is_init_completed() && !_should_terminate) {
    CGC_lock->wait(Mutex::_no_safepoint_check_flag, 1);
  }
}

void ConcurrentGCThread::terminate() {
  assert(_should_terminate, "Should only be called on terminate request.");
  // Signal that it is terminated
  {
    MutexLockerEx mu(Terminator_lock,
                     Mutex::_no_safepoint_check_flag);
    _has_terminated = true;
    Terminator_lock->notify();
  }
}

void ConcurrentGCThread::run() {
  initialize_in_thread();
  wait_for_universe_init();

  run_service();

  terminate();
}

void ConcurrentGCThread::stop() {
  // it is ok to take late safepoints here, if needed
  {
    MutexLockerEx mu(Terminator_lock);
    assert(!_has_terminated,   "stop should only be called once");
    assert(!_should_terminate, "stop should only be called once");
    _should_terminate = true;
  }

  stop_service();

  {
    MutexLockerEx mu(Terminator_lock);
    while (!_has_terminated) {
      Terminator_lock->wait();
    }
  }
}
