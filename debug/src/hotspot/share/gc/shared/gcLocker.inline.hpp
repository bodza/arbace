#ifndef SHARE_VM_GC_SHARED_GCLOCKER_INLINE_HPP
#define SHARE_VM_GC_SHARED_GCLOCKER_INLINE_HPP

#include "gc/shared/gcLocker.hpp"
#include "runtime/thread.hpp"

void GCLocker::lock_critical(JavaThread* thread) {
  if (!thread->in_critical()) {
    if (needs_gc()) {
      // jni_lock call calls enter_critical under the lock so that the
      // global lock count and per thread count are in agreement.
      jni_lock(thread);
      return;
    }
    increment_debug_jni_lock_count();
  }
  thread->enter_critical();
}

void GCLocker::unlock_critical(JavaThread* thread) {
  if (thread->in_last_critical()) {
    if (needs_gc()) {
      // jni_unlock call calls exit_critical under the lock so that
      // the global lock count and per thread count are in agreement.
      jni_unlock(thread);
      return;
    }
    decrement_debug_jni_lock_count();
  }
  thread->exit_critical();
}

#endif
