#ifndef SRC_SHARE_VM_GC_SHARED_MEMSETWITHCONCURRENTREADERS_HPP
#define SRC_SHARE_VM_GC_SHARED_MEMSETWITHCONCURRENTREADERS_HPP

#include "utilities/macros.hpp"

#include <stddef.h>
#include <string.h>

// Fill a block of memory with value, like memset, but with the
// understanding that there may be concurrent readers of that memory.
void memset_with_concurrent_readers(void* to, int value, size_t size);

// All others just use memset.

inline void memset_with_concurrent_readers(void* to, int value, size_t size) {
  ::memset(to, value, size);
}

#endif
