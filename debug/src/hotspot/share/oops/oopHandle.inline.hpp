#ifndef SHARE_VM_OOPS_OOPHANDLE_INLINE_HPP
#define SHARE_VM_OOPS_OOPHANDLE_INLINE_HPP

#include "oops/access.inline.hpp"
#include "oops/oopHandle.hpp"

inline oop OopHandle::resolve() const {
  return (_obj == NULL) ? (oop)NULL : NativeAccess<>::oop_load(_obj);
}

#endif
