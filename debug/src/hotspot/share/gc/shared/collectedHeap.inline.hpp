#ifndef SHARE_VM_GC_SHARED_COLLECTEDHEAP_INLINE_HPP
#define SHARE_VM_GC_SHARED_COLLECTEDHEAP_INLINE_HPP

#include "gc/shared/collectedHeap.hpp"
#include "oops/oop.inline.hpp"
#include "utilities/align.hpp"

inline HeapWord* CollectedHeap::align_allocation_or_fail(HeapWord* addr,
                                                         HeapWord* end,
                                                         unsigned short alignment_in_bytes) {
  if (alignment_in_bytes <= ObjectAlignmentInBytes) {
    return addr;
  }

  HeapWord* new_addr = align_up(addr, alignment_in_bytes);
  size_t padding = pointer_delta(new_addr, addr);

  if (padding == 0) {
    return addr;
  }

  if (padding < CollectedHeap::min_fill_size()) {
    padding += alignment_in_bytes / HeapWordSize;
    new_addr = addr + padding;
  }

  if (new_addr < end) {
    CollectedHeap::fill_with_object(addr, padding);
    return new_addr;
  } else {
    return NULL;
  }
}

#endif
