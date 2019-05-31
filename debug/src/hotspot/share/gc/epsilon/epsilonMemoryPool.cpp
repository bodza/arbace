#include "precompiled.hpp"
#include "gc/epsilon/epsilonHeap.hpp"
#include "gc/epsilon/epsilonMemoryPool.hpp"

EpsilonMemoryPool::EpsilonMemoryPool(EpsilonHeap* heap) :
        _heap(heap),
        CollectedMemoryPool("Epsilon Heap",
                            heap->capacity(),
                            heap->max_capacity(),
                            false) {
}

MemoryUsage EpsilonMemoryPool::get_memory_usage() {
  size_t initial_sz = initial_size();
  size_t max_sz     = max_size();
  size_t used       = used_in_bytes();
  size_t committed  = committed_in_bytes();

  return MemoryUsage(initial_sz, used, committed, max_sz);
}
