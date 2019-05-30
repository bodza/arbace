#ifndef SHARE_VM_C1_C1_VALUESET_INLINE_HPP
#define SHARE_VM_C1_C1_VALUESET_INLINE_HPP

#include "c1/c1_Instruction.hpp"
#include "c1/c1_ValueSet.hpp"
#include "utilities/bitMap.inline.hpp"

inline ValueSet::ValueSet() : _map(Instruction::number_of_instructions()) {
}

inline ValueSet* ValueSet::copy() {
  ValueSet* res = new ValueSet();
  res->_map.set_from(_map);
  return res;
}

inline bool ValueSet::contains(Value x) {
  return _map.at(x->id());
}

inline void ValueSet::put(Value x) {
  _map.set_bit(x->id());
}

inline void ValueSet::remove(Value x) {
  _map.clear_bit(x->id());
}

inline bool ValueSet::set_intersect(ValueSet* other) {
  return _map.set_intersection_with_result(other->_map);
}

inline void ValueSet::set_union(ValueSet* other) {
  _map.set_union(other->_map);
}

inline void ValueSet::clear() {
  _map.clear();
}

inline void ValueSet::set_from(ValueSet* other) {
  _map.set_from(other->_map);
}

inline bool ValueSet::equals(ValueSet* other) {
  return _map.is_same(other->_map);
}

#endif
