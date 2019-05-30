#ifndef SHARE_VM_CI_CIOBJARRAY_HPP
#define SHARE_VM_CI_CIOBJARRAY_HPP

#include "ci/ciArray.hpp"
#include "ci/ciClassList.hpp"
#include "oops/objArrayOop.hpp"

// ciObjArray
//
// This class represents a ObjArrayOop in the HotSpot virtual
// machine.
class ciObjArray : public ciArray {
  CI_PACKAGE_ACCESS

protected:
  ciObjArray(objArrayHandle h_o) : ciArray(h_o) {}

  ciObjArray(ciKlass* klass, int len) : ciArray(klass, len) {}

  objArrayOop get_objArrayOop() {
    return (objArrayOop)get_oop();
  }

  const char* type_string() { return "ciObjArray"; }

public:
  // What kind of ciObject is this?
  bool is_obj_array() { return true; }

  ciObject* obj_at(int index);
};

#endif
