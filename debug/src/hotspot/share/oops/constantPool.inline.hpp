#ifndef SHARE_VM_OOPS_CONSTANTPOOL_INLINE_HPP
#define SHARE_VM_OOPS_CONSTANTPOOL_INLINE_HPP

#include "oops/constantPool.hpp"
#include "oops/cpCache.inline.hpp"
#include "runtime/orderAccess.hpp"

inline CPSlot ConstantPool::slot_at(int which) const {
  // Uses volatile because the klass slot changes without a lock.
  intptr_t adr = OrderAccess::load_acquire(obj_at_addr(which));
  return CPSlot(adr);
}

inline Klass* ConstantPool::resolved_klass_at(int which) const {  // Used by Compiler
  guarantee(tag_at(which).is_klass(), "Corrupted constant pool");
  // Must do an acquire here in case another thread resolved the klass
  // behind our back, lest we later load stale values thru the oop.
  CPKlassSlot kslot = klass_slot_at(which);

  Klass** adr = resolved_klasses()->adr_at(kslot.resolved_klass_index());
  return OrderAccess::load_acquire(adr);
}

inline bool ConstantPool::is_pseudo_string_at(int which) {
  return slot_at(which).is_pseudo_string();
}

inline oop ConstantPool::pseudo_string_at(int which, int obj_index) {
  oop s = resolved_references()->obj_at(obj_index);
  return s;
}

inline oop ConstantPool::pseudo_string_at(int which) {
  int obj_index = cp_to_object_index(which);
  oop s = resolved_references()->obj_at(obj_index);
  return s;
}

#endif
