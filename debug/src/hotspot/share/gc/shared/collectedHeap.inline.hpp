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

  assert(is_aligned(addr, HeapWordSize), "Address " PTR_FORMAT " is not properly aligned.", p2i(addr));
  assert(is_aligned(alignment_in_bytes, HeapWordSize), "Alignment size %u is incorrect.", alignment_in_bytes);

  HeapWord* new_addr = align_up(addr, alignment_in_bytes);
  size_t padding = pointer_delta(new_addr, addr);

  if (padding == 0) {
    return addr;
  }

  if (padding < CollectedHeap::min_fill_size()) {
    padding += alignment_in_bytes / HeapWordSize;
    assert(padding >= CollectedHeap::min_fill_size(), "alignment_in_bytes %u is expect to be larger than the minimum object size", alignment_in_bytes);
    new_addr = addr + padding;
  }

  assert(new_addr > addr, "Unexpected arithmetic overflow " PTR_FORMAT " not greater than " PTR_FORMAT, p2i(new_addr), p2i(addr));
  if(new_addr < end) {
    CollectedHeap::fill_with_object(addr, padding);
    return new_addr;
  } else {
    return NULL;
  }
}

#endif
