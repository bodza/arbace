#ifndef SHARE_GC_SHARED_GCARGUMENTS_HPP
#define SHARE_GC_SHARED_GCARGUMENTS_HPP

#include "memory/allocation.hpp"

class CollectedHeap;

class GCArguments {
protected:
  template <class Heap, class Policy>
  CollectedHeap* create_heap_with_policy();

public:
  virtual void initialize();
  virtual size_t conservative_max_heap_alignment() = 0;
  virtual CollectedHeap* create_heap() = 0;
};

#endif
