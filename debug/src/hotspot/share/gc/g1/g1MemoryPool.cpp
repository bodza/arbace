#include "precompiled.hpp"
#include "gc/g1/g1CollectedHeap.hpp"
#include "gc/g1/g1MemoryPool.hpp"
#include "gc/g1/heapRegion.hpp"

G1MemoryPoolSuper::G1MemoryPoolSuper(G1CollectedHeap* g1h,
                                     const char* name,
                                     size_t init_size,
                                     size_t max_size,
                                     bool support_usage_threshold) :
  _g1mm(g1h->g1mm()), CollectedMemoryPool(name,
                                          init_size,
                                          max_size,
                                          support_usage_threshold) {
  assert(UseG1GC, "sanity");
}

G1EdenPool::G1EdenPool(G1CollectedHeap* g1h) :
  G1MemoryPoolSuper(g1h,
                    "G1 Eden Space",
                    g1h->g1mm()->eden_space_committed(), /* init_size */
                    _undefined_max,
                    false /* support_usage_threshold */) { }

MemoryUsage G1EdenPool::get_memory_usage() {
  size_t initial_sz = initial_size();
  size_t max_sz     = max_size();
  size_t used       = used_in_bytes();
  size_t committed  = _g1mm->eden_space_committed();

  return MemoryUsage(initial_sz, used, committed, max_sz);
}

G1SurvivorPool::G1SurvivorPool(G1CollectedHeap* g1h) :
  G1MemoryPoolSuper(g1h,
                    "G1 Survivor Space",
                    g1h->g1mm()->survivor_space_committed(), /* init_size */
                    _undefined_max,
                    false /* support_usage_threshold */) { }

MemoryUsage G1SurvivorPool::get_memory_usage() {
  size_t initial_sz = initial_size();
  size_t max_sz     = max_size();
  size_t used       = used_in_bytes();
  size_t committed  = _g1mm->survivor_space_committed();

  return MemoryUsage(initial_sz, used, committed, max_sz);
}

G1OldGenPool::G1OldGenPool(G1CollectedHeap* g1h) :
  G1MemoryPoolSuper(g1h,
                    "G1 Old Gen",
                    g1h->g1mm()->old_space_committed(), /* init_size */
                    g1h->g1mm()->old_gen_max(),
                    true /* support_usage_threshold */) { }

MemoryUsage G1OldGenPool::get_memory_usage() {
  size_t initial_sz = initial_size();
  size_t max_sz     = max_size();
  size_t used       = used_in_bytes();
  size_t committed  = _g1mm->old_space_committed();

  return MemoryUsage(initial_sz, used, committed, max_sz);
}
