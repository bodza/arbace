#ifndef SHARE_VM_CI_CIMEMBERNAME_HPP
#define SHARE_VM_CI_CIMEMBERNAME_HPP

#include "ci/ciCallProfile.hpp"
#include "ci/ciInstance.hpp"

// ciMemberName
//
// The class represents a java.lang.invoke.MemberName object.
class ciMemberName : public ciInstance {
public:
  ciMemberName(instanceHandle h_i) : ciInstance(h_i) { }

  // What kind of ciObject is this?
  bool is_member_name() const { return true; }

  ciMethod* get_vmtarget() const;
};

#endif
