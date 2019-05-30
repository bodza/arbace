#ifndef SHARE_VM_OOPS_CONSTANTPOOL_INLINE_HPP
#define SHARE_VM_OOPS_CONSTANTPOOL_INLINE_HPP

#include "oops/constantPool.hpp"
#include "oops/cpCache.inline.hpp"
#include "runtime/orderAccess.hpp"

inline CPSlot ConstantPool::slot_at(int which) const {
  assert(is_within_bounds(which), "index out of bounds");
  assert(!tag_at(which).is_unresolved_klass() && !tag_at(which).is_unresolved_klass_in_error(), "Corrupted constant pool");
  // Uses volatile because the klass slot changes without a lock.
  intptr_t adr = OrderAccess::load_acquire(obj_at_addr(which));
  assert(adr != 0 || which == 0, "cp entry for klass should not be zero");
  return CPSlot(adr);
}

inline Klass* ConstantPool::resolved_klass_at(int which) const {  // Used by Compiler
  guarantee(tag_at(which).is_klass(), "Corrupted constant pool");
  // Must do an acquire here in case another thread resolved the klass
  // behind our back, lest we later load stale values thru the oop.
  CPKlassSlot kslot = klass_slot_at(which);
  assert(tag_at(kslot.name_index()).is_symbol(), "sanity");

  Klass** adr = resolved_klasses()->adr_at(kslot.resolved_klass_index());
  return OrderAccess::load_acquire(adr);
}

inline bool ConstantPool::is_pseudo_string_at(int which) {
  assert(tag_at(which).is_string(), "Corrupted constant pool");
  return slot_at(which).is_pseudo_string();
}

inline oop ConstantPool::pseudo_string_at(int which, int obj_index) {
  assert(is_pseudo_string_at(which), "must be a pseudo-string");
  oop s = resolved_references()->obj_at(obj_index);
  return s;
}

inline oop ConstantPool::pseudo_string_at(int which) {
  assert(is_pseudo_string_at(which), "must be a pseudo-string");
  int obj_index = cp_to_object_index(which);
  oop s = resolved_references()->obj_at(obj_index);
  return s;
}

#endif
