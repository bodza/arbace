#ifndef SHARE_VM_RUNTIME_SAFEPOINTVERIFIERS_HPP
#define SHARE_VM_RUNTIME_SAFEPOINTVERIFIERS_HPP

#include "memory/allocation.hpp"
#include "runtime/thread.hpp"

// A NoGCVerifier object can be placed in methods where one assumes that
// no garbage collection will occur. The destructor will verify this property
// unless the constructor is called with argument false (not verifygc).
//
// The check will only be done in debug mode and if verifygc true.

class NoGCVerifier: public StackObj {
 friend class PauseNoGCVerifier;

 protected:
  bool _verifygc;
  unsigned int _old_invocations;

 public:
  NoGCVerifier(bool verifygc = true) { }
  ~NoGCVerifier() { }
};

// A PauseNoGCVerifier is used to temporarily pause the behavior
// of a NoGCVerifier object. If we are not in debug mode or if the
// NoGCVerifier object has a _verifygc value of false, then there
// is nothing to do.

class PauseNoGCVerifier: public StackObj {
 private:
  NoGCVerifier * _ngcv;

 public:
  PauseNoGCVerifier(NoGCVerifier * ngcv) { }
  ~PauseNoGCVerifier() { }
};

// A NoSafepointVerifier object will throw an assertion failure if
// the current thread passes a possible safepoint while this object is
// instantiated. A safepoint, will either be: an oop allocation, blocking
// on a Mutex or JavaLock, or executing a VM operation.
//
// If StrictSafepointChecks is turned off, it degrades into a NoGCVerifier
//
class NoSafepointVerifier : public NoGCVerifier {
 friend class PauseNoSafepointVerifier;

 private:
  bool _activated;
  Thread *_thread;
 public:
  NoSafepointVerifier(bool activated = true, bool verifygc = true) : NoGCVerifier(verifygc) { }
  ~NoSafepointVerifier() { }
};

// A PauseNoSafepointVerifier is used to temporarily pause the
// behavior of a NoSafepointVerifier object. If we are not in debug
// mode then there is nothing to do. If the NoSafepointVerifier
// object has an _activated value of false, then there is nothing to
// do for safepoint and allocation checking, but there may still be
// something to do for the underlying NoGCVerifier object.

class PauseNoSafepointVerifier : public PauseNoGCVerifier {
 private:
  NoSafepointVerifier * _nsv;

 public:
  PauseNoSafepointVerifier(NoSafepointVerifier * nsv)
    : PauseNoGCVerifier(nsv) { }
  ~PauseNoSafepointVerifier() { }
};

// A NoAllocVerifier object can be placed in methods where one assumes that
// no allocation will occur. The destructor will verify this property
// unless the constructor is called with argument false (not activated).
//
// The check will only be done in debug mode and if activated.
// Note: this only makes sense at safepoints (otherwise, other threads may
// allocate concurrently.)

class NoAllocVerifier : public StackObj {
 private:
  bool  _activated;

 public:
  NoAllocVerifier(bool activated = true) { }
  ~NoAllocVerifier() { }
};

#endif
