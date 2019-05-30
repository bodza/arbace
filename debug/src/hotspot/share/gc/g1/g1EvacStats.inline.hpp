#ifndef SHARE_VM_GC_G1_G1EVACSTATS_INLINE_HPP
#define SHARE_VM_GC_G1_G1EVACSTATS_INLINE_HPP

#include "gc/g1/g1EvacStats.hpp"
#include "runtime/atomic.hpp"

inline void G1EvacStats::add_direct_allocated(size_t value) {
  Atomic::add(value, &_direct_allocated);
}

inline void G1EvacStats::add_region_end_waste(size_t value) {
  Atomic::add(value, &_region_end_waste);
  Atomic::inc(&_regions_filled);
}

inline void G1EvacStats::add_failure_used_and_waste(size_t used, size_t waste) {
  Atomic::add(used, &_failure_used);
  Atomic::add(waste, &_failure_waste);
}

#endif
