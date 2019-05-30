#ifndef SHARE_VM_GC_SHARED_REFERENCEPOLICY_HPP
#define SHARE_VM_GC_SHARED_REFERENCEPOLICY_HPP

#include "oops/oopsHierarchy.hpp"

// referencePolicy is used to determine when soft reference objects
// should be cleared.

class ReferencePolicy : public CHeapObj<mtGC> {
 public:
  virtual bool should_clear_reference(oop p, jlong timestamp_clock) {
    ShouldNotReachHere();
    return true;
  }

  // Capture state (of-the-VM) information needed to evaluate the policy
  virtual void setup() { /* do nothing */ }
};

class NeverClearPolicy : public ReferencePolicy {
 public:
  virtual bool should_clear_reference(oop p, jlong timestamp_clock) {
    return false;
  }
};

class AlwaysClearPolicy : public ReferencePolicy {
 public:
  virtual bool should_clear_reference(oop p, jlong timestamp_clock) {
    return true;
  }
};

class LRUCurrentHeapPolicy : public ReferencePolicy {
 private:
  jlong _max_interval;

 public:
  LRUCurrentHeapPolicy();

  // Capture state (of-the-VM) information needed to evaluate the policy
  void setup();
  virtual bool should_clear_reference(oop p, jlong timestamp_clock);
};

class LRUMaxHeapPolicy : public ReferencePolicy {
 private:
  jlong _max_interval;

 public:
  LRUMaxHeapPolicy();

  // Capture state (of-the-VM) information needed to evaluate the policy
  void setup();
  virtual bool should_clear_reference(oop p, jlong timestamp_clock);
};

#endif
