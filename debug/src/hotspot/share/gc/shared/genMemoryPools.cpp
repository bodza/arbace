#include "precompiled.hpp"

#include "gc/shared/generation.hpp"
#include "gc/shared/genMemoryPools.hpp"
#include "gc/shared/space.hpp"

ContiguousSpacePool::ContiguousSpacePool(ContiguousSpace* space, const char* name, size_t max_size, bool support_usage_threshold) :
  CollectedMemoryPool(name, space->capacity(), max_size, support_usage_threshold), _space(space) {
}

size_t ContiguousSpacePool::used_in_bytes() {
  return space()->used();
}

MemoryUsage ContiguousSpacePool::get_memory_usage() {
  size_t maxSize   = (available_for_allocation() ? max_size() : 0);
  size_t used      = used_in_bytes();
  size_t committed = _space->capacity();

  return MemoryUsage(initial_size(), used, committed, maxSize);
}

GenerationPool::GenerationPool(Generation* gen, const char* name, bool support_usage_threshold) :
  CollectedMemoryPool(name, gen->capacity(), gen->max_capacity(), support_usage_threshold), _gen(gen) {
}

size_t GenerationPool::used_in_bytes() {
  return _gen->used();
}

MemoryUsage GenerationPool::get_memory_usage() {
  size_t used      = used_in_bytes();
  size_t committed = _gen->capacity();
  size_t maxSize   = (available_for_allocation() ? max_size() : 0);

  return MemoryUsage(initial_size(), used, committed, maxSize);
}
