#ifndef SHARE_VM_CI_CIMETHODHANDLE_HPP
#define SHARE_VM_CI_CIMETHODHANDLE_HPP

#include "ci/ciClassList.hpp"
#include "ci/ciInstance.hpp"

// ciMethodHandle
//
// The class represents a java.lang.invoke.MethodHandle object.
class ciMethodHandle : public ciInstance {
public:
  ciMethodHandle(instanceHandle h_i) : ciInstance(h_i) { }

  // What kind of ciObject is this?
  bool is_method_handle() const { return true; }

  ciMethod* get_vmtarget() const;
};

#endif
