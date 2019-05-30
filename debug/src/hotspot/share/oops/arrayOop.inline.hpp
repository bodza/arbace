#ifndef SHARE_OOPS_ARRAYOOP_INLINE_HPP
#define SHARE_OOPS_ARRAYOOP_INLINE_HPP

#include "oops/access.inline.hpp"
#include "oops/arrayOop.hpp"

void* arrayOopDesc::base(BasicType type) const {
  oop resolved_obj = Access<>::resolve(as_oop());
  return arrayOop(resolved_obj)->base_raw(type);
}

void* arrayOopDesc::base_raw(BasicType type) const {
  return reinterpret_cast<void*>(cast_from_oop<intptr_t>(as_oop()) + base_offset_in_bytes(type));
}

#endif
