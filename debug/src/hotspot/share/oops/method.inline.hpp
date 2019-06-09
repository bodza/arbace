#ifndef SHARE_VM_OOPS_METHOD_INLINE_HPP
#define SHARE_VM_OOPS_METHOD_INLINE_HPP

#include "oops/method.hpp"
#include "runtime/orderAccess.hpp"

inline address Method::from_compiled_entry() const {
  return OrderAccess::load_acquire(&_from_compiled_entry);
}

inline void Method::set_method_data(MethodData* data) {
  // The store into method must be released. On platforms without
  // total store order (TSO) the reference may become visible before
  // the initialization of data otherwise.
  OrderAccess::release_store(&_method_data, data);
}

inline CompiledMethod* volatile Method::code() const {
  return OrderAccess::load_acquire(&_code);
}

inline bool Method::has_compiled_code() const { return code() != NULL; }

#endif
