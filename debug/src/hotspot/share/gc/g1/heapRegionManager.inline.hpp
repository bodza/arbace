#ifndef SHARE_VM_GC_G1_HEAPREGIONMANAGER_INLINE_HPP
#define SHARE_VM_GC_G1_HEAPREGIONMANAGER_INLINE_HPP

#include "gc/g1/heapRegion.hpp"
#include "gc/g1/heapRegionManager.hpp"
#include "gc/g1/heapRegionSet.inline.hpp"

inline HeapRegion* HeapRegionManager::addr_to_region(HeapWord* addr) const {

  HeapRegion* hr = _regions.get_by_address(addr);
  return hr;
}

inline HeapRegion* HeapRegionManager::at(uint index) const {
  HeapRegion* hr = _regions.get_by_index(index);
  return hr;
}

inline HeapRegion* HeapRegionManager::next_region_in_humongous(HeapRegion* hr) const {
  uint index = hr->hrm_index();
  index++;
  if (index < max_length() && is_available(index) && at(index)->is_continues_humongous()) {
    return at(index);
  } else {
    return NULL;
  }
}

inline void HeapRegionManager::insert_into_free_list(HeapRegion* hr) {
  _free_list.add_ordered(hr);
}

inline void HeapRegionManager::allocate_free_regions_starting_at(uint first, uint num_regions) {
  _free_list.remove_starting_at(at(first), num_regions);
}

#endif
