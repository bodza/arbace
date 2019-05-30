#ifndef SHARE_GC_EPSILON_EPSILONARGUMENTS_HPP
#define SHARE_GC_EPSILON_EPSILONARGUMENTS_HPP

#include "gc/shared/gcArguments.hpp"

class CollectedHeap;

class EpsilonArguments : public GCArguments {
public:
  virtual void initialize();
  virtual size_t conservative_max_heap_alignment();
  virtual CollectedHeap* create_heap();
};

#endif
