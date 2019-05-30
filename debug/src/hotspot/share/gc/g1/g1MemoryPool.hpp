#ifndef SHARE_VM_GC_G1_G1MEMORYPOOL_HPP
#define SHARE_VM_GC_G1_G1MEMORYPOOL_HPP

#include "gc/g1/g1MonitoringSupport.hpp"
#include "services/memoryPool.hpp"
#include "services/memoryUsage.hpp"

// This file contains the three classes that represent the memory
// pools of the G1 spaces: G1EdenPool, G1SurvivorPool, and
// G1OldGenPool. In G1, unlike our other GCs, we do not have a
// physical space for each of those spaces. Instead, we allocate
// regions for all three spaces out of a single pool of regions (that
// pool basically covers the entire heap). As a result, the eden,
// survivor, and old gen are considered logical spaces in G1, as each
// is a set of non-contiguous regions. This is also reflected in the
// way we map them to memory pools here. The easiest way to have done
// this would have been to map the entire G1 heap to a single memory
// pool. However, it's helpful to show how large the eden and survivor
// get, as this does affect the performance and behavior of G1. Which
// is why we introduce the three memory pools implemented here.
//
// See comments in g1MonitoringSupport.hpp for additional details
// on this model.
//

class G1CollectedHeap;

// This class is shared by the three G1 memory pool classes
// (G1EdenPool, G1SurvivorPool, G1OldGenPool).
class G1MemoryPoolSuper : public CollectedMemoryPool {
protected:
  const static size_t _undefined_max = (size_t) -1;
  G1MonitoringSupport* _g1mm;

  // Would only be called from subclasses.
  G1MemoryPoolSuper(G1CollectedHeap* g1h,
                    const char* name,
                    size_t init_size,
                    size_t max_size,
                    bool support_usage_threshold);
};

// Memory pool that represents the G1 eden.
class G1EdenPool : public G1MemoryPoolSuper {
public:
  G1EdenPool(G1CollectedHeap* g1h);

  size_t used_in_bytes() {
    return _g1mm->eden_space_used();
  }
  size_t max_size() const {
    return _undefined_max;
  }
  MemoryUsage get_memory_usage();
};

// Memory pool that represents the G1 survivor.
class G1SurvivorPool : public G1MemoryPoolSuper {
public:
  G1SurvivorPool(G1CollectedHeap* g1h);

  size_t used_in_bytes() {
    return _g1mm->survivor_space_used();
  }
  size_t max_size() const {
    return _undefined_max;
  }
  MemoryUsage get_memory_usage();
};

// Memory pool that represents the G1 old gen.
class G1OldGenPool : public G1MemoryPoolSuper {
public:
  G1OldGenPool(G1CollectedHeap* g1h);

  size_t used_in_bytes() {
    return _g1mm->old_space_used();
  }
  size_t max_size() const {
    return _g1mm->old_gen_max();
  }
  MemoryUsage get_memory_usage();
};

#endif
