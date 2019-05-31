#ifndef SHARE_VM_CI_CIMETHODTYPE_HPP
#define SHARE_VM_CI_CIMETHODTYPE_HPP

#include "ci/ciInstance.hpp"

// ciMethodType
//
// The class represents a java.lang.invoke.MethodType object.
class ciMethodType : public ciInstance {
private:
  ciType* class_to_citype(oop klass_oop) const;

public:
  ciMethodType(instanceHandle h_i) : ciInstance(h_i) { }

  // What kind of ciObject is this?
  bool is_method_type() const { return true; }

  ciType* rtype() const;

  int ptype_count() const;
  int ptype_slot_count() const;

  ciType* ptype_at(int index) const;
};

#endif
