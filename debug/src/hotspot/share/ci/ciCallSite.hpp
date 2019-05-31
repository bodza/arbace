#ifndef SHARE_VM_CI_CICALLSITE_HPP
#define SHARE_VM_CI_CICALLSITE_HPP

#include "ci/ciInstance.hpp"

// ciCallSite
//
// The class represents a java.lang.invoke.CallSite object.
class ciCallSite : public ciInstance {
public:
  ciCallSite(instanceHandle h_i) : ciInstance(h_i) { }

  // What kind of ciObject is this?
  bool is_call_site() const { return true; }

  bool is_constant_call_site();
  bool is_mutable_call_site();
  bool is_volatile_call_site();

  // Return the target MethodHandle of this CallSite.
  ciMethodHandle* get_target() const;

  void print();
};

#endif
