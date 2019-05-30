#ifndef SHARE_VM_GC_SHARED_GCUTIL_INLINE_HPP
#define SHARE_VM_GC_SHARED_GCUTIL_INLINE_HPP

#include "gc/shared/gcUtil.hpp"
#include "memory/allocation.inline.hpp"

inline void* AdaptivePaddedAverage::operator new(size_t size) throw() {
  return CHeapObj<mtGC>::operator new(size);
}

#endif
