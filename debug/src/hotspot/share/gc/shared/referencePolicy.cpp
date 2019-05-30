#include "precompiled.hpp"
#include "classfile/javaClasses.hpp"
#include "gc/shared/referencePolicy.hpp"
#include "memory/universe.hpp"
#include "runtime/arguments.hpp"
#include "runtime/globals.hpp"

LRUCurrentHeapPolicy::LRUCurrentHeapPolicy() {
  setup();
}

// Capture state (of-the-VM) information needed to evaluate the policy
void LRUCurrentHeapPolicy::setup() {
  _max_interval = (Universe::get_heap_free_at_last_gc() / M) * SoftRefLRUPolicyMSPerMB;
  assert(_max_interval >= 0,"Sanity check");
}

// The oop passed in is the SoftReference object, and not
// the object the SoftReference points to.
bool LRUCurrentHeapPolicy::should_clear_reference(oop p,
                                                  jlong timestamp_clock) {
  jlong interval = timestamp_clock - java_lang_ref_SoftReference::timestamp(p);
  assert(interval >= 0, "Sanity check");

  // The interval will be zero if the ref was accessed since the last scavenge/gc.
  if(interval <= _max_interval) {
    return false;
  }

  return true;
}

/////////////////////// MaxHeap //////////////////////

LRUMaxHeapPolicy::LRUMaxHeapPolicy() {
  setup();
}

// Capture state (of-the-VM) information needed to evaluate the policy
void LRUMaxHeapPolicy::setup() {
  size_t max_heap = MaxHeapSize;
  max_heap -= Universe::get_heap_used_at_last_gc();
  max_heap /= M;

  _max_interval = max_heap * SoftRefLRUPolicyMSPerMB;
  assert(_max_interval >= 0,"Sanity check");
}

// The oop passed in is the SoftReference object, and not
// the object the SoftReference points to.
bool LRUMaxHeapPolicy::should_clear_reference(oop p,
                                             jlong timestamp_clock) {
  jlong interval = timestamp_clock - java_lang_ref_SoftReference::timestamp(p);
  assert(interval >= 0, "Sanity check");

  // The interval will be zero if the ref was accessed since the last scavenge/gc.
  if(interval <= _max_interval) {
    return false;
  }

  return true;
}
