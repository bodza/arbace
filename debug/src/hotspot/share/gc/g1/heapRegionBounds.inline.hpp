#ifndef SHARE_VM_GC_G1_HEAPREGIONBOUNDS_INLINE_HPP
#define SHARE_VM_GC_G1_HEAPREGIONBOUNDS_INLINE_HPP

#include "gc/g1/heapRegionBounds.hpp"

size_t HeapRegionBounds::min_size() {
  return MIN_REGION_SIZE;
}

size_t HeapRegionBounds::max_size() {
  return MAX_REGION_SIZE;
}

size_t HeapRegionBounds::target_number() {
  return TARGET_REGION_NUMBER;
}

#endif
