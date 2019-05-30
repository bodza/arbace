#ifndef SHARE_VM_OOPS_ARRAYKLASS_INLINE_HPP
#define SHARE_VM_OOPS_ARRAYKLASS_INLINE_HPP

#include "runtime/orderAccess.hpp"
#include "oops/arrayKlass.hpp"

inline Klass* ArrayKlass::higher_dimension_acquire() const {
  return OrderAccess::load_acquire(&_higher_dimension);
}

inline void ArrayKlass::release_set_higher_dimension(Klass* k) {
  OrderAccess::release_store(&_higher_dimension, k);
}

#endif
