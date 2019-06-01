#ifndef SHARE_VM_OOPS_CPCACHEOOP_INLINE_HPP
#define SHARE_VM_OOPS_CPCACHEOOP_INLINE_HPP

#include "oops/cpCache.hpp"
#include "oops/oopHandle.inline.hpp"
#include "runtime/orderAccess.hpp"

inline int ConstantPoolCacheEntry::indices_ord() const { return OrderAccess::load_acquire(&_indices); }

inline Bytecodes::Code ConstantPoolCacheEntry::bytecode_1() const {
  return Bytecodes::cast((indices_ord() >> bytecode_1_shift) & bytecode_1_mask);
}

inline Bytecodes::Code ConstantPoolCacheEntry::bytecode_2() const {
  return Bytecodes::cast((indices_ord() >> bytecode_2_shift) & bytecode_2_mask);
}

// Has this bytecode been resolved? Only valid for invokes and get/put field/static.
inline bool ConstantPoolCacheEntry::is_resolved(Bytecodes::Code code) const {
  switch (bytecode_number(code)) {
    case 1:  return (bytecode_1() == code);
    case 2:  return (bytecode_2() == code);
  }
  return false;      // default: not resolved
}

inline Method* ConstantPoolCacheEntry::f2_as_interface_method() const {
  return (Method*)_f2;
}

inline Metadata* ConstantPoolCacheEntry::f1_ord() const { return (Metadata *)OrderAccess::load_acquire(&_f1); }

inline Method* ConstantPoolCacheEntry::f1_as_method() const {
  Metadata* f1 = f1_ord();
  return (Method*)f1;
}

inline Klass* ConstantPoolCacheEntry::f1_as_klass() const {
  Metadata* f1 = f1_ord();
  return (Klass*)f1;
}

inline bool ConstantPoolCacheEntry::is_f1_null() const { Metadata* f1 = f1_ord(); return f1 == NULL; }

inline bool ConstantPoolCacheEntry::has_appendix() const {
  return (!is_f1_null()) && (_flags & (1 << has_appendix_shift)) != 0;
}

inline bool ConstantPoolCacheEntry::has_method_type() const {
  return (!is_f1_null()) && (_flags & (1 << has_method_type_shift)) != 0;
}

inline intx ConstantPoolCacheEntry::flags_ord() const { return (intx)OrderAccess::load_acquire(&_flags); }

inline bool ConstantPoolCacheEntry::indy_resolution_failed() const {
  intx flags = flags_ord();
  return (flags & (1 << indy_resolution_failed_shift)) != 0;
}

// Constructor
inline ConstantPoolCache::ConstantPoolCache(int length, const intStack& inverse_index_map, const intStack& invokedynamic_inverse_index_map, const intStack& invokedynamic_references_map) : _length(length), _constant_pool(NULL) {
  initialize(inverse_index_map, invokedynamic_inverse_index_map, invokedynamic_references_map);
}

inline oop ConstantPoolCache::resolved_references() { return _resolved_references.resolve(); }

#endif
