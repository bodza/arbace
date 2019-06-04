#ifndef SHARE_GC_G1_G1ARGUMENTS_HPP
#define SHARE_GC_G1_G1ARGUMENTS_HPP

#include "gc/shared/gcArguments.hpp"

class CollectedHeap;

class G1Arguments : public GCArguments {
  friend class G1HeapVerifierTest_parse_Test;

public:
  virtual void initialize();
  virtual size_t conservative_max_heap_alignment();
  virtual CollectedHeap* create_heap();
};

#endif
