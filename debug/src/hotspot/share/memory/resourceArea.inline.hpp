#ifndef SHARE_VM_MEMORY_RESOURCEAREA_INLINE_HPP
#define SHARE_VM_MEMORY_RESOURCEAREA_INLINE_HPP

#include "memory/resourceArea.hpp"

inline char* ResourceArea::allocate_bytes(size_t size, AllocFailType alloc_failmode) {
  return (char*)Amalloc(size, alloc_failmode);
}

#endif
