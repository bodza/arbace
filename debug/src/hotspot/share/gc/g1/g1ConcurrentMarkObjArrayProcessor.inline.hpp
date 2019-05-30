#ifndef SHARE_VM_GC_G1_G1CONCURRENTMARKOBJARRAYPROCESSOR_INLINE_HPP
#define SHARE_VM_GC_G1_G1CONCURRENTMARKOBJARRAYPROCESSOR_INLINE_HPP

#include "oops/oop.inline.hpp"
#include "oops/oopsHierarchy.hpp"
#include "runtime/globals.hpp"

inline bool G1CMObjArrayProcessor::should_be_sliced(oop obj) {
  return obj->is_objArray() && ((size_t)((objArrayOop)obj)->size()) >= 2 * ObjArrayMarkingStride;
}

#endif
