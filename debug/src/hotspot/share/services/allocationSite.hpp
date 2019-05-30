#ifndef SHARE_VM_SERVICES_ALLOCATION_SITE_HPP
#define SHARE_VM_SERVICES_ALLOCATION_SITE_HPP

#include "utilities/nativeCallStack.hpp"

// Allocation site represents a code path that makes a memory
// allocation
template <class E> class AllocationSite {
 private:
  NativeCallStack  _call_stack;
  E                e;
 public:
  AllocationSite(const NativeCallStack& stack) : _call_stack(stack) { }
  int hash() const { return _call_stack.hash(); }
  bool equals(const NativeCallStack& stack) const {
    return _call_stack.equals(stack);
  }

  bool equals(const AllocationSite<E>& other) const {
    return other.equals(_call_stack);
  }

  const NativeCallStack* call_stack() const {
    return &_call_stack;
  }

  // Information regarding this allocation
  E* data()             { return &e; }
  const E* peek() const { return &e; }
};

#endif
