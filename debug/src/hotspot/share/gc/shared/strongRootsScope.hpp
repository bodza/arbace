#ifndef SHARE_VM_GC_SHARED_STRONGROOTSSCOPE_HPP
#define SHARE_VM_GC_SHARED_STRONGROOTSSCOPE_HPP

#include "memory/allocation.hpp"

class MarkScope : public StackObj {
 protected:
  MarkScope();
  ~MarkScope();
};

// Sets up and tears down the required state for parallel root processing.

class StrongRootsScope : public MarkScope {
  // Number of threads participating in the roots processing.
  const uint _n_threads;

 public:
  StrongRootsScope(uint n_threads);
  ~StrongRootsScope();

  uint n_threads() const { return _n_threads; }
};

#endif
