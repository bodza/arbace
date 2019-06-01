#ifndef SHARE_VM_GC_SHARED_THREADLOCALALLOCBUFFER_INLINE_HPP
#define SHARE_VM_GC_SHARED_THREADLOCALALLOCBUFFER_INLINE_HPP

#include "gc/shared/collectedHeap.hpp"
#include "gc/shared/threadLocalAllocBuffer.hpp"
#include "logging/log.hpp"
#include "runtime/thread.hpp"
#include "utilities/copy.hpp"

inline HeapWord* ThreadLocalAllocBuffer::allocate(size_t size) {
  invariants();
  HeapWord* obj = top();
  if (pointer_delta(end(), obj) >= size) {
    // successful thread-local allocation
    // This addition is safe because we know that top is
    // at least size below end, so the add can't wrap.
    set_top(obj + size);

    invariants();
    return obj;
  }
  return NULL;
}

inline size_t ThreadLocalAllocBuffer::compute_size(size_t obj_size) {
  // Compute the size for the new TLAB.
  // The "last" tlab may be smaller to reduce fragmentation.
  // unsafe_max_tlab_alloc is just a hint.
  const size_t available_size = Universe::heap()->unsafe_max_tlab_alloc(myThread()) / HeapWordSize;
  size_t new_tlab_size = MIN3(available_size, desired_size() + align_object_size(obj_size), max_size());

  // Make sure there's enough room for object and filler int[].
  if (new_tlab_size < compute_min_size(obj_size)) {
    // If there isn't enough room for the allocation, return failure.
    return 0;
  }
  return new_tlab_size;
}

inline size_t ThreadLocalAllocBuffer::compute_min_size(size_t obj_size) {
  const size_t aligned_obj_size = align_object_size(obj_size);
  const size_t size_with_reserve = aligned_obj_size + alignment_reserve();
  return MAX2(size_with_reserve, heap_word_size(MinTLABSize));
}

void ThreadLocalAllocBuffer::record_slow_allocation(size_t obj_size) {
  // Raise size required to bypass TLAB next time. Why? Else there's
  // a risk that a thread that repeatedly allocates objects of one
  // size will get stuck on this slow path.

  set_refill_waste_limit(refill_waste_limit() + refill_waste_limit_increment());

  _slow_allocations++;
}

#endif
