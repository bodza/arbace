#ifndef SHARE_VM_CI_CIOBJARRAYKLASS_HPP
#define SHARE_VM_CI_CIOBJARRAYKLASS_HPP

#include "ci/ciArrayKlass.hpp"

// ciObjArrayKlass
//
// This class represents a Klass* in the HotSpot virtual machine
// whose Klass part is an ObjArrayKlass.
class ciObjArrayKlass : public ciArrayKlass {
  CI_PACKAGE_ACCESS
  friend class ciEnv;

private:
  ciKlass* _element_klass;
  ciKlass* _base_element_klass;

protected:
  ciObjArrayKlass(Klass* k);
  ciObjArrayKlass(ciSymbol* array_name,
                  ciKlass* base_element_klass,
                  int dimension);

  ObjArrayKlass* get_ObjArrayKlass() {
    return (ObjArrayKlass*)get_Klass();
  }

  static ciObjArrayKlass* make_impl(ciKlass* element_klass);
  static ciSymbol* construct_array_name(ciSymbol* element_name,
                                        int       dimension);

  const char* type_string() { return "ciObjArrayKlass"; }

  oop     loader()        { return _base_element_klass->loader(); }
  jobject loader_handle() { return _base_element_klass->loader_handle(); }

  oop     protection_domain()        { return _base_element_klass->protection_domain(); }
  jobject protection_domain_handle() { return _base_element_klass->protection_domain_handle(); }

public:
  // The one-level type of the array elements.
  ciKlass* element_klass();

  // The innermost type of the array elements.
  ciKlass* base_element_klass() { return _base_element_klass; }

  // What kind of ciObject is this?
  bool is_obj_array_klass() const { return true; }

  static ciObjArrayKlass* make(ciKlass* element_klass);

  virtual ciKlass* exact_klass();
};

#endif
