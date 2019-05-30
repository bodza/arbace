#ifndef SHARE_VM_CI_CITYPEARRAYKLASS_HPP
#define SHARE_VM_CI_CITYPEARRAYKLASS_HPP

#include "ci/ciArrayKlass.hpp"

// ciTypeArrayKlass
//
// This class represents a Klass* in the HotSpot virtual machine
// whose Klass part in a TypeArrayKlass.
class ciTypeArrayKlass : public ciArrayKlass {
  CI_PACKAGE_ACCESS

protected:
  ciTypeArrayKlass(Klass* k);

  TypeArrayKlass* get_TypeArrayKlass() {
    return (TypeArrayKlass*)get_Klass();
  }

  const char* type_string() { return "ciTypeArrayKlass"; }

  // Helper method for make.
  static ciTypeArrayKlass* make_impl(BasicType type);

public:
  // The type of the array elements.
  BasicType element_type() {
    return Klass::layout_helper_element_type(layout_helper());
  }

  // What kind of ciObject is this?
  bool is_type_array_klass() const { return true; }

  // Make an array klass corresponding to the specified primitive type.
  static ciTypeArrayKlass* make(BasicType type);

  virtual ciKlass* exact_klass() {
    return this;
  }
};

#endif
