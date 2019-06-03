#ifndef SHARE_VM_GC_G1_G1ALLOCREGION_INLINE_HPP
#define SHARE_VM_GC_G1_G1ALLOCREGION_INLINE_HPP

#include "gc/g1/g1AllocRegion.hpp"
#include "gc/g1/heapRegion.inline.hpp"

inline void G1AllocRegion::reset_alloc_region() {
  _alloc_region = _dummy_region;
}

inline HeapWord* G1AllocRegion::allocate(HeapRegion* alloc_region, size_t word_size) {
  if (!_bot_updates) {
    return alloc_region->allocate_no_bot_updates(word_size);
  } else {
    return alloc_region->allocate(word_size);
  }
}

inline HeapWord* G1AllocRegion::par_allocate(HeapRegion* alloc_region, size_t word_size) {
  size_t temp;
  return par_allocate(alloc_region, word_size, word_size, &temp);
}

inline HeapWord* G1AllocRegion::par_allocate(HeapRegion* alloc_region, size_t min_word_size, size_t desired_word_size, size_t* actual_word_size) {
  if (!_bot_updates) {
    return alloc_region->par_allocate_no_bot_updates(min_word_size, desired_word_size, actual_word_size);
  } else {
    return alloc_region->par_allocate(min_word_size, desired_word_size, actual_word_size);
  }
}

inline HeapWord* G1AllocRegion::attempt_allocation(size_t word_size) {
  size_t temp;
  return attempt_allocation(word_size, word_size, &temp);
}

inline HeapWord* G1AllocRegion::attempt_allocation(size_t min_word_size,
                                                   size_t desired_word_size,
                                                   size_t* actual_word_size) {
  HeapRegion* alloc_region = _alloc_region;

  HeapWord* result = par_allocate(alloc_region, min_word_size, desired_word_size, actual_word_size);
  if (result != NULL) {
    trace("alloc", min_word_size, desired_word_size, *actual_word_size, result);
    return result;
  }
  trace("alloc failed", min_word_size, desired_word_size);
  return NULL;
}

inline HeapWord* G1AllocRegion::attempt_allocation_locked(size_t word_size) {
  size_t temp;
  return attempt_allocation_locked(word_size, word_size, &temp);
}

inline HeapWord* G1AllocRegion::attempt_allocation_locked(size_t min_word_size,
                                                          size_t desired_word_size,
                                                          size_t* actual_word_size) {
  // First we have to redo the allocation, assuming we're holding the
  // appropriate lock, in case another thread changed the region while
  // we were waiting to get the lock.
  HeapWord* result = attempt_allocation(min_word_size, desired_word_size, actual_word_size);
  if (result != NULL) {
    return result;
  }

  retire(true /* fill_up */);
  result = new_alloc_region_and_allocate(desired_word_size, false /* force */);
  if (result != NULL) {
    *actual_word_size = desired_word_size;
    trace("alloc locked (second attempt)", min_word_size, desired_word_size, *actual_word_size, result);
    return result;
  }
  trace("alloc locked failed", min_word_size, desired_word_size);
  return NULL;
}

inline HeapWord* G1AllocRegion::attempt_allocation_force(size_t word_size) {
  trace("forcing alloc", word_size, word_size);
  HeapWord* result = new_alloc_region_and_allocate(word_size, true /* force */);
  if (result != NULL) {
    trace("alloc forced", word_size, word_size, word_size, result);
    return result;
  }
  trace("alloc forced failed", word_size, word_size);
  return NULL;
}

inline HeapWord* MutatorAllocRegion::attempt_retained_allocation(size_t min_word_size,
                                                                 size_t desired_word_size,
                                                                 size_t* actual_word_size) {
  if (_retained_alloc_region != NULL) {
    HeapWord* result = par_allocate(_retained_alloc_region, min_word_size, desired_word_size, actual_word_size);
    if (result != NULL) {
      trace("alloc retained", min_word_size, desired_word_size, *actual_word_size, result);
      return result;
    }
  }
  return NULL;
}

#endif
