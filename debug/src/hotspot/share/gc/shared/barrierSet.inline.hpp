#ifndef SHARE_VM_GC_SHARED_BARRIERSET_INLINE_HPP
#define SHARE_VM_GC_SHARED_BARRIERSET_INLINE_HPP

#include "gc/shared/barrierSet.hpp"
#include "oops/accessDecorators.hpp"
#include "oops/arrayOop.hpp"
#include "oops/compressedOops.inline.hpp"
#include "oops/objArrayOop.inline.hpp"
#include "oops/oop.hpp"

template <DecoratorSet decorators, typename BarrierSetT>
template <typename T>
inline bool BarrierSet::AccessBarrier<decorators, BarrierSetT>::oop_arraycopy_in_heap(arrayOop src_obj, size_t src_offset_in_bytes, T* src_raw,
                                                                                      arrayOop dst_obj, size_t dst_offset_in_bytes, T* dst_raw,
                                                                                      size_t length) {
  T* src = arrayOopDesc::obj_offset_to_raw(src_obj, src_offset_in_bytes, src_raw);
  T* dst = arrayOopDesc::obj_offset_to_raw(dst_obj, dst_offset_in_bytes, dst_raw);

  if (!HasDecorator<decorators, ARRAYCOPY_CHECKCAST>::value) {
    // Covariant, copy without checks
    return Raw::oop_arraycopy(NULL, 0, src, NULL, 0, dst, length);
  }

  // Copy each element with checking casts
  Klass* const dst_klass = objArrayOop(dst_obj)->element_klass();
  for (const T* const end = src + length; src < end; src++, dst++) {
    const T elem = *src;
    if (!oopDesc::is_instanceof_or_null(CompressedOops::decode(elem), dst_klass)) {
      return false;
    }
    *dst = elem;
  }

  return true;
}

#endif
