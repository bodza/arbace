#ifndef SHARE_VM_GC_EPSILON_EPSILONMEMORYPOOL_HPP
#define SHARE_VM_GC_EPSILON_EPSILONMEMORYPOOL_HPP

#include "gc/epsilon/epsilonHeap.hpp"
#include "services/memoryPool.hpp"
#include "services/memoryUsage.hpp"
#include "utilities/macros.hpp"

class EpsilonMemoryPool : public CollectedMemoryPool {
private:
  EpsilonHeap* _heap;

public:
  EpsilonMemoryPool(EpsilonHeap* heap);
  size_t committed_in_bytes() { return _heap->capacity(); }
  size_t used_in_bytes()      { return _heap->used(); }
  size_t max_size()     const { return _heap->max_capacity(); }
  MemoryUsage get_memory_usage();
};

#endif
