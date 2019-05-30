#ifndef SHARE_VM_RUNTIME_STACKVALUECOLLECTION_HPP
#define SHARE_VM_RUNTIME_STACKVALUECOLLECTION_HPP

#include "memory/allocation.hpp"
#include "runtime/stackValue.hpp"
#include "utilities/growableArray.hpp"

class StackValueCollection : public ResourceObj {
 private:
  GrowableArray<StackValue*>* _values;

 public:
  StackValueCollection()            { _values = new GrowableArray<StackValue*>(); }
  StackValueCollection(int length)  { _values = new GrowableArray<StackValue*>(length); }

  void add(StackValue *val) const   { _values->push(val); }
  int  size() const                 { return _values->length(); }
  bool is_empty() const             { return (size() == 0); }
  StackValue* at(int i) const       { return _values->at(i); }

  // Get typed locals/expressions
  jint  int_at(int slot) const;
  jlong long_at(int slot) const;
  Handle obj_at(int slot) const;
  jfloat  float_at(int slot) const;
  jdouble double_at(int slot) const;

  // Set typed locals/expressions
  void set_int_at(int slot, jint value);
  void set_long_at(int slot, jlong value);
  void set_obj_at(int slot, Handle value);
  void set_float_at(int slot, jfloat value);
  void set_double_at(int slot, jdouble value);

  void print();
};

#endif
