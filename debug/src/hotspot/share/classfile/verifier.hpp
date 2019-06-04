#ifndef SHARE_VM_CLASSFILE_VERIFIER_HPP
#define SHARE_VM_CLASSFILE_VERIFIER_HPP

#include "classfile/verificationType.hpp"
#include "oops/klass.hpp"
#include "oops/method.hpp"
#include "runtime/handles.hpp"
#include "utilities/exceptions.hpp"
#include "utilities/growableArray.hpp"

// The verifier class
class Verifier : AllStatic {
 public:
  enum {
    STRICTER_ACCESS_CTRL_CHECK_VERSION  = 49,
    STACKMAP_ATTRIBUTE_MAJOR_VERSION    = 50,
    INVOKEDYNAMIC_MAJOR_VERSION         = 51,
    NO_RELAX_ACCESS_CTRL_CHECK_VERSION  = 52,
    DYNAMICCONSTANT_MAJOR_VERSION       = 55
  };
  typedef enum { ThrowException, NoException } Mode;

  static bool verify(InstanceKlass* klass, Mode mode, TRAPS);
};

#endif
