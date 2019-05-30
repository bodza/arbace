#ifndef SHARE_VM_CI_CIARRAYKLASS_HPP
#define SHARE_VM_CI_CIARRAYKLASS_HPP

#include "ci/ciKlass.hpp"

// ciArrayKlass
//
// This class, and its subclasses represent Klass*s in the
// HotSpot virtual machine whose Klass part is an ArrayKlass.
class ciArrayKlass : public ciKlass {
  CI_PACKAGE_ACCESS
private:
  jint _dimension;

protected:
  ciArrayKlass(Klass* k);
  ciArrayKlass(ciSymbol* name, int dimension, BasicType bt);

  ArrayKlass* get_ArrayKlass() {
    return (ArrayKlass*)get_Klass();
  }

  const char* type_string() { return "ciArrayKlass"; }

public:
  jint dimension() { return _dimension; }
  ciType* element_type();       // JLS calls this the "component type"
  ciType* base_element_type();  // JLS calls this the "element type"
  bool is_leaf_type();          // No subtypes of this array type.

  // What kind of vmObject is this?
  bool is_array_klass() const { return true; }
  bool is_java_klass() const  { return true; }

  static ciArrayKlass* make(ciType* element_type);
};

#endif
