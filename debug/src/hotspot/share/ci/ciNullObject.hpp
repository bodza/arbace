#ifndef SHARE_VM_CI_CINULLOBJECT_HPP
#define SHARE_VM_CI_CINULLOBJECT_HPP

#include "ci/ciClassList.hpp"
#include "ci/ciObject.hpp"

// ciNullObject
//
// This class represents a null reference in the VM.
class ciNullObject : public ciObject {
  CI_PACKAGE_ACCESS

private:
  ciNullObject() : ciObject() { }

  const char* type_string() { return "ciNullObject"; }

  void print_impl(outputStream* st);

public:
  // Is this ciObject a Java Language Object?  That is,
  // is the ciObject an instance or an array
  bool is_java_object() { return true; }

  // What kind of ciObject is this?
  bool is_null_object() const { return true; }
  bool is_classless()   const { return true; }

  // Get the distinguished instance of this klass.
  static ciNullObject* make();
};

#endif
