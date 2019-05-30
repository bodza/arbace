#ifndef SHARE_VM_GC_G1_G1HEAPTRANSITION_HPP
#define SHARE_VM_GC_G1_G1HEAPTRANSITION_HPP

#include "gc/shared/plab.hpp"

class G1CollectedHeap;

class G1HeapTransition {
  struct Data {
    size_t _eden_length;
    size_t _survivor_length;
    size_t _old_length;
    size_t _humongous_length;
    size_t _metaspace_used_bytes;

    Data(G1CollectedHeap* g1_heap);
  };

  G1CollectedHeap* _g1_heap;
  Data _before;

public:
  G1HeapTransition(G1CollectedHeap* g1_heap);

  void print();
};

#endif
