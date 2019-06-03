#include "precompiled.hpp"

#include "gc/shared/collectedHeap.hpp"
#include "gc/shared/gcLocker.hpp"
#include "memory/resourceArea.hpp"
#include "runtime/atomic.hpp"
#include "runtime/safepoint.hpp"
#include "runtime/thread.inline.hpp"
#include "runtime/threadSMR.hpp"

volatile jint GCLocker::_jni_lock_count = 0;
volatile bool GCLocker::_needs_gc       = false;
volatile bool GCLocker::_doing_gc       = false;

void GCLocker::log_debug_jni(const char* msg) { }

bool GCLocker::is_at_safepoint() {
  return SafepointSynchronize::is_at_safepoint();
}

bool GCLocker::check_active_before_gc() {
  if (is_active() && !_needs_gc) {
    verify_critical_count();
    _needs_gc = true;
    log_debug_jni("Setting _needs_gc.");
  }
  return is_active();
}

void GCLocker::stall_until_clear() {
  MutexLocker   ml(JNICritical_lock);

  if (needs_gc()) {
    log_debug_jni("Allocation failed. Thread stalled by JNI critical section.");
  }

  // Wait for _needs_gc  to be cleared
  while (needs_gc()) {
    JNICritical_lock->wait();
  }
}

void GCLocker::jni_lock(JavaThread* thread) {
  MutexLocker mu(JNICritical_lock);
  // Block entering threads if we know at least one thread is in a
  // JNI critical region and we need a GC.
  // We check that at least one thread is in a critical region before
  // blocking because blocked threads are woken up by a thread exiting
  // a JNI critical region.
  while (is_active_and_needs_gc() || _doing_gc) {
    JNICritical_lock->wait();
  }
  thread->enter_critical();
  _jni_lock_count++;
  increment_debug_jni_lock_count();
}

void GCLocker::jni_unlock(JavaThread* thread) {
  MutexLocker mu(JNICritical_lock);
  _jni_lock_count--;
  decrement_debug_jni_lock_count();
  thread->exit_critical();
  if (needs_gc() && !is_active_internal()) {
    // We're the last thread out. Cause a GC to occur.
    _doing_gc = true;
    {
      // Must give up the lock while at a safepoint
      MutexUnlocker munlock(JNICritical_lock);
      log_debug_jni("Performing GC after exiting critical section.");
      Universe::heap()->collect(GCCause::_gc_locker);
    }
    _doing_gc = false;
    _needs_gc = false;
    JNICritical_lock->notify_all();
  }
}
