#ifndef SHARE_VM_GC_SHARED_GCLOCKER_HPP
#define SHARE_VM_GC_SHARED_GCLOCKER_HPP

#include "memory/allocation.hpp"
#include "utilities/globalDefinitions.hpp"
#include "utilities/macros.hpp"

class JavaThread;

// The direct lock/unlock calls do not force a collection if an unlock
// decrements the count to zero. Avoid calling these if at all possible.

class GCLocker: public AllStatic {
 private:
  // The _jni_lock_count keeps track of the number of threads that are
  // currently in a critical region.  It's only kept up to date when
  // _needs_gc is true.  The current value is computed during
  // safepointing and decremented during the slow path of GCLocker
  // unlocking.
  static volatile jint _jni_lock_count;  // number of jni active instances.
  static volatile bool _needs_gc;        // heap is filling, we need a GC
                                         // note: bool is typedef'd as jint
  static volatile bool _doing_gc;        // unlock_critical() is doing a GC

  // At a safepoint, visit all threads and count the number of active
  // critical sections.  This is used to ensure that all active
  // critical sections are exited before a new one is started.
  static void verify_critical_count() { };

  static void jni_lock(JavaThread* thread);
  static void jni_unlock(JavaThread* thread);

  static bool is_active_internal() {
    verify_critical_count();
    return _jni_lock_count > 0;
  }

  static void log_debug_jni(const char* msg);

  static bool is_at_safepoint();

 public:
  // Accessors
  static bool is_active() {
    return is_active_internal();
  }
  static bool needs_gc()       { return _needs_gc; }

  // Shorthand
  static bool is_active_and_needs_gc() {
    // Use is_active_internal since _needs_gc can change from true to
    // false outside of a safepoint, triggering the assert in
    // is_active.
    return needs_gc() && is_active_internal();
  }

  // In debug mode track the locking state at all times
  static void increment_debug_jni_lock_count() { };
  static void decrement_debug_jni_lock_count() { };

  // Set the current lock count
  static void set_jni_lock_count(int count) {
    _jni_lock_count = count;
    verify_critical_count();
  }

  // Sets _needs_gc if is_active() is true. Returns is_active().
  static bool check_active_before_gc();

  // Stalls the caller (who should not be in a jni critical section)
  // until needs_gc() clears. Note however that needs_gc() may be
  // set at a subsequent safepoint and/or cleared under the
  // JNICritical_lock, so the caller may not safely assert upon
  // return from this method that "!needs_gc()" since that is
  // not a stable predicate.
  static void stall_until_clear();

  // The following two methods are used for JNI critical regions.
  // If we find that we failed to perform a GC because the GCLocker
  // was active, arrange for one as soon as possible by allowing
  // all threads in critical regions to complete, but not allowing
  // other critical regions to be entered. The reasons for that are:
  // 1) a GC request won't be starved by overlapping JNI critical
  //    region activities, which can cause unnecessary OutOfMemory errors.
  // 2) even if allocation requests can still be satisfied before GC locker
  //    becomes inactive, for example, in tenured generation possibly with
  //    heap expansion, those allocations can trigger lots of safepointing
  //    attempts (ineffective GC attempts) and require Heap_lock which
  //    slow down allocations tremendously.
  //
  // Note that critical regions can be nested in a single thread, so
  // we must allow threads already in critical regions to continue.
  //
  // JNI critical regions are the only participants in this scheme
  // because they are, by spec, well bounded while in a critical region.
  //
  // Each of the following two method is split into a fast path and a
  // slow path. JNICritical_lock is only grabbed in the slow path.
  // _needs_gc is initially false and every java thread will go
  // through the fast path, which simply increments or decrements the
  // current thread's critical count.  When GC happens at a safepoint,
  // GCLocker::is_active() is checked. Since there is no safepoint in
  // the fast path of lock_critical() and unlock_critical(), there is
  // no race condition between the fast path and GC. After _needs_gc
  // is set at a safepoint, every thread will go through the slow path
  // after the safepoint.  Since after a safepoint, each of the
  // following two methods is either entered from the method entry and
  // falls into the slow path, or is resumed from the safepoints in
  // the method, which only exist in the slow path. So when _needs_gc
  // is set, the slow path is always taken, till _needs_gc is cleared.
  inline static void lock_critical(JavaThread* thread);
  inline static void unlock_critical(JavaThread* thread);

  static address needs_gc_address() { return (address) &_needs_gc; }
};

#endif
