#ifndef SHARE_VM_GC_SHARED_PLAB_INLINE_HPP
#define SHARE_VM_GC_SHARED_PLAB_INLINE_HPP

#include "gc/shared/collectedHeap.inline.hpp"
#include "gc/shared/plab.hpp"
#include "memory/allocation.inline.hpp"
#include "runtime/atomic.hpp"

inline HeapWord* PLAB::allocate_aligned(size_t word_sz, unsigned short alignment_in_bytes) {
  HeapWord* res = CollectedHeap::align_allocation_or_fail(_top, _end, alignment_in_bytes);
  if (res == NULL) {
    return NULL;
  }

  // Set _top so that allocate(), which expects _top to be correctly set,
  // can be used below.
  _top = res;
  return allocate(word_sz);
}

void PLABStats::add_allocated(size_t v) {
  Atomic::add(v, &_allocated);
}

void PLABStats::add_unused(size_t v) {
  Atomic::add(v, &_unused);
}

void PLABStats::add_wasted(size_t v) {
  Atomic::add(v, &_wasted);
}

void PLABStats::add_undo_wasted(size_t v) {
  Atomic::add(v, &_undo_wasted);
}

#endif
