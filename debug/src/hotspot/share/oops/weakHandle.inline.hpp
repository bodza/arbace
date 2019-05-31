#ifndef SHARE_VM_OOPS_WEAKHANDLE_INLINE_HPP
#define SHARE_VM_OOPS_WEAKHANDLE_INLINE_HPP

#include "oops/weakHandle.hpp"
#include "oops/access.inline.hpp"

template <WeakHandleType T>
oop WeakHandle<T>::resolve() const {
  return NativeAccess<ON_PHANTOM_OOP_REF>::oop_load(_obj);
}

template <WeakHandleType T>
oop WeakHandle<T>::peek() const {
  return NativeAccess<ON_PHANTOM_OOP_REF | AS_NO_KEEPALIVE>::oop_load(_obj);
}

template <WeakHandleType T>
void WeakHandle<T>::replace(oop with_obj) {
  NativeAccess<ON_PHANTOM_OOP_REF>::oop_store(_obj, with_obj);
}

#endif
