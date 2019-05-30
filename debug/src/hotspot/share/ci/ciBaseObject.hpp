#ifndef SHARE_VM_CI_CIBASEOBJECT_HPP
#define SHARE_VM_CI_CIBASEOBJECT_HPP

#include "ci/ciClassList.hpp"
#include "memory/allocation.hpp"
#include "runtime/jniHandles.hpp"

// ciBaseObject
//
// This class represents an oop in the HotSpot virtual machine.
// Its subclasses are structured in a hierarchy which mirrors
// an aggregate of the VM's oop and klass hierarchies (see
// oopHierarchy.hpp).  Each instance of ciBaseObject holds a handle
// to a corresponding oop on the VM side and provides routines
// for accessing the information in its oop.  By using the ciBaseObject
// hierarchy for accessing oops in the VM, the compiler ensures
// that it is safe with respect to garbage collection; that is,
// GC and compilation can proceed independently without
// interference.
//
// Within the VM, the oop and klass hierarchies are separate.
// The compiler interface does not preserve this separation --
// the distinction between `Klass*' and `Klass' are not
// reflected in the interface and instead the Klass hierarchy
// is directly modeled as the subclasses of ciKlass.
class ciBaseObject : public ResourceObj {
  CI_PACKAGE_ACCESS
  friend class ciEnv;

protected:
  uint     _ident;

  enum { FLAG_BITS   = 1 };
  enum {
         SCAVENGABLE_FLAG = 1
       };
protected:
  ciBaseObject(): _ident(0) {}

  virtual const char* type_string() { return "ciBaseObject"; }

  void set_ident(uint id);

public:
  // A number unique to this object.
  uint ident();

  // What kind of ciBaseObject is this?
  virtual bool is_symbol() const       { return false; }
  virtual bool is_object() const       { return false; }
  virtual bool is_metadata() const     { return false; }

  ciSymbol* as_symbol() {
    assert(is_symbol(), "must be");
    return (ciSymbol*)this;
  }
  ciObject* as_object() {
    assert(is_object(), "must be");
    return (ciObject*)this;
  }
  ciMetadata* as_metadata() {
    assert(is_metadata(), "must be");
    return (ciMetadata*)this;
  }
};
#endif
