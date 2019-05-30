#ifndef SHARE_VM_GC_SHARED_GENMEMORYPOOLS_HPP
#define SHARE_VM_GC_SHARED_GENMEMORYPOOLS_HPP

#include "services/memoryPool.hpp"

class ContiguousSpace;
class DefNewGeneration;
class Generation;

class ContiguousSpacePool : public CollectedMemoryPool {
private:
  ContiguousSpace* _space;

public:
  ContiguousSpacePool(ContiguousSpace* space,
                      const char* name,
                      size_t max_size,
                      bool support_usage_threshold);

  ContiguousSpace* space() { return _space; }
  MemoryUsage get_memory_usage();
  size_t used_in_bytes();
};

class SurvivorContiguousSpacePool : public CollectedMemoryPool {
private:
  DefNewGeneration* _young_gen;

public:
  SurvivorContiguousSpacePool(DefNewGeneration* young_gen,
                              const char* name,
                              size_t max_size,
                              bool support_usage_threshold);

  MemoryUsage get_memory_usage();

  size_t used_in_bytes();
  size_t committed_in_bytes();
};

class GenerationPool : public CollectedMemoryPool {
private:
  Generation* _gen;
public:
  GenerationPool(Generation* gen, const char* name, bool support_usage_threshold);

  MemoryUsage get_memory_usage();
  size_t used_in_bytes();
};

#endif
