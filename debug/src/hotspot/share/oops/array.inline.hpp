#ifndef SHARE_VM_OOPS_ARRAY_INLINE_HPP
#define SHARE_VM_OOPS_ARRAY_INLINE_HPP

#include "oops/array.hpp"
#include "runtime/orderAccess.hpp"

template <typename T>
inline T Array<T>::at_acquire(const int which) { return OrderAccess::load_acquire(adr_at(which)); }

template <typename T>
inline void Array<T>::release_at_put(int which, T contents) { OrderAccess::release_store(adr_at(which), contents); }

#endif
