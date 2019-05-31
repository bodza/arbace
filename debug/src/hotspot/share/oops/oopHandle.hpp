#ifndef SHARE_VM_OOPS_OOPHANDLE_HPP
#define SHARE_VM_OOPS_OOPHANDLE_HPP

#include "oops/oop.hpp"

// Simple class for encapsulating oop pointers stored in metadata.
// These are different from Handle.  The Handle class stores pointers
// to oops on the stack, and manages the allocation from a thread local
// area in the constructor.
// This assumes that the caller will allocate the handle in the appropriate
// area.  The reason for the encapsulation is to help with naming and to allow
// future uses for read barriers.

class OopHandle {
private:
  oop* _obj;

public:
  OopHandle() : _obj(NULL) { }
  OopHandle(oop* w) : _obj(w) { }

  inline oop resolve() const;

  // Used only for removing handle.
  oop* ptr_raw() const { return _obj; }
};

#endif
