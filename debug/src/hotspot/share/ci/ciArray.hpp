#ifndef SHARE_VM_CI_CIARRAY_HPP
#define SHARE_VM_CI_CIARRAY_HPP

#include "ci/ciArrayKlass.hpp"
#include "ci/ciConstant.hpp"
#include "ci/ciObject.hpp"
#include "oops/arrayOop.hpp"
#include "oops/objArrayOop.hpp"
#include "oops/typeArrayOop.hpp"

// ciArray
//
// This class represents an arrayOop in the HotSpot virtual
// machine.
class ciArray : public ciObject {
private:
  int _length;

protected:
  ciArray(    arrayHandle h_a) : ciObject(h_a), _length(h_a()->length()) {}
  ciArray( objArrayHandle h_a) : ciObject(h_a), _length(h_a()->length()) {}
  ciArray(typeArrayHandle h_a) : ciObject(h_a), _length(h_a()->length()) {}

  ciArray(ciKlass* klass, int len) : ciObject(klass), _length(len) {}

  arrayOop get_arrayOop() const { return (arrayOop)get_oop(); }

  const char* type_string() { return "ciArray"; }

  void print_impl(outputStream* st);

  ciConstant element_value_impl(BasicType elembt, arrayOop ary, int index);

public:
  int length() { return _length; }

  // Convenience routines.
  ciArrayKlass* array_type()         { return klass()->as_array_klass(); }
  ciType*       element_type()       { return array_type()->element_type(); }
  BasicType     element_basic_type() { return element_type()->basic_type(); }

  // Current value of an element.
  // Returns T_ILLEGAL if there is no element at the given index.
  ciConstant element_value(int index);

  // Current value of an element at the specified offset.
  // Returns T_ILLEGAL if there is no element at the given offset.
  ciConstant element_value_by_offset(intptr_t element_offset);

  // What kind of ciObject is this?
  bool is_array()        { return true; }
  bool is_java_object()  { return true; }
};

#endif
