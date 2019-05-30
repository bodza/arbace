#ifndef SHARE_VM_SERVICES_MALLOC_TRACKER_INLINE_HPP
#define SHARE_VM_SERVICES_MALLOC_TRACKER_INLINE_HPP

#include "services/mallocTracker.hpp"
#include "services/memTracker.hpp"

inline void* MallocTracker::get_base(void* memblock){
  return get_base(memblock, MemTracker::tracking_level());
}

#endif
