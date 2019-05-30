#ifndef SHARE_VM_CI_CITYPEARRAY_HPP
#define SHARE_VM_CI_CITYPEARRAY_HPP

#include "ci/ciArray.hpp"
#include "ci/ciClassList.hpp"
#include "oops/typeArrayOop.hpp"

// ciTypeArray
//
// This class represents a typeArrayOop in the HotSpot virtual
// machine.
class ciTypeArray : public ciArray {
  CI_PACKAGE_ACCESS

protected:
  ciTypeArray(typeArrayHandle h_t) : ciArray(h_t) {}

  ciTypeArray(ciKlass* klass, int len) : ciArray(klass, len) {}

  typeArrayOop get_typeArrayOop() {
    return (typeArrayOop)get_oop();
  }

  const char* type_string() { return "ciTypeArray"; }

public:
  // What kind of ciObject is this?
  bool is_type_array() { return true; }

  // Return character at index. This is only useful if the
  // compiler has already proved that the contents of the
  // array will never change.
  jchar char_at(int index);

  // Return byte at index.
  jbyte byte_at(int index);
};

#endif
