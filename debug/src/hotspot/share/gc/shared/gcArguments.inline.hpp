#include "gc/shared/gcArguments.hpp"

class CollectedHeap;

template <class Heap, class Policy>
CollectedHeap* GCArguments::create_heap_with_policy() {
  Policy* policy = new Policy();
  policy->initialize_all();
  return new Heap(policy);
}
