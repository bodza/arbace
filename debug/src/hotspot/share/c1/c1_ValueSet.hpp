#ifndef SHARE_VM_C1_C1_VALUESET_HPP
#define SHARE_VM_C1_C1_VALUESET_HPP

#include "c1/c1_Instruction.hpp"
#include "memory/allocation.hpp"
#include "utilities/bitMap.hpp"

// A ValueSet is a simple abstraction on top of a BitMap representing
// a set of Instructions. Currently it assumes that the number of
// instructions is fixed during its lifetime; should make it
// automatically resizable.

class ValueSet: public CompilationResourceObj {
 private:
  ResourceBitMap _map;

 public:
  ValueSet();

  ValueSet* copy();
  bool contains(Value x);
  void put     (Value x);
  void remove  (Value x);
  bool set_intersect(ValueSet* other);
  void set_union(ValueSet* other);
  void clear   ();
  void set_from(ValueSet* other);
  bool equals  (ValueSet* other);
};

#endif
