#ifndef SHARE_VM_GC_G1_HEAPREGIONBOUNDS_HPP
#define SHARE_VM_GC_G1_HEAPREGIONBOUNDS_HPP

#include "memory/allocation.hpp"

class HeapRegionBounds : public AllStatic {
private:
  // Minimum region size; we won't go lower than that.
  // We might want to decrease this in the future, to deal with small
  // heaps a bit more efficiently.
  static const size_t MIN_REGION_SIZE = 1024 * 1024;

  // Maximum region size; we don't go higher than that. There's a good
  // reason for having an upper bound. We don't want regions to get too
  // large, otherwise cleanup's effectiveness would decrease as there
  // will be fewer opportunities to find totally empty regions after
  // marking.
  static const size_t MAX_REGION_SIZE = 32 * 1024 * 1024;

  // The automatic region size calculation will try to have around this
  // many regions in the heap (based on the min heap size).
  static const size_t TARGET_REGION_NUMBER = 2048;

public:
  static inline size_t min_size();
  static inline size_t max_size();
  static inline size_t target_number();
};

#endif
