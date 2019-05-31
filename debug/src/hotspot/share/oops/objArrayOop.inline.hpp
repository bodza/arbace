#ifndef SHARE_VM_OOPS_OBJARRAYOOP_INLINE_HPP
#define SHARE_VM_OOPS_OBJARRAYOOP_INLINE_HPP

#include "oops/access.inline.hpp"
#include "oops/arrayOop.inline.hpp"
#include "oops/objArrayOop.hpp"
#include "oops/oop.inline.hpp"
#include "runtime/globals.hpp"

inline HeapWord* objArrayOopDesc::base() const { return (HeapWord*) arrayOopDesc::base(T_OBJECT); }
inline HeapWord* objArrayOopDesc::base_raw() const { return (HeapWord*) arrayOopDesc::base_raw(T_OBJECT); }

template <class T> T* objArrayOopDesc::obj_at_addr(int index) const {
  return &((T*)base())[index];
}

template <class T> T* objArrayOopDesc::obj_at_addr_raw(int index) const {
  return &((T*)base_raw())[index];
}

inline oop objArrayOopDesc::obj_at(int index) const {
  ptrdiff_t offset = UseCompressedOops ? obj_at_offset<narrowOop>(index) : obj_at_offset<oop>(index);
  return HeapAccess<IS_ARRAY>::oop_load_at(as_oop(), offset);
}

inline void objArrayOopDesc::obj_at_put(int index, oop value) {
  ptrdiff_t offset = UseCompressedOops ? obj_at_offset<narrowOop>(index) : obj_at_offset<oop>(index);
  HeapAccess<IS_ARRAY>::oop_store_at(as_oop(), offset, value);
}

#endif
