#ifndef SHARE_VM_GC_G1_G1REGIONMARKSTATSCACHE_INLINE_HPP
#define SHARE_VM_GC_G1_G1REGIONMARKSTATSCACHE_INLINE_HPP

#include "gc/g1/g1RegionMarkStatsCache.hpp"
#include "runtime/atomic.hpp"

inline G1RegionMarkStatsCache::G1RegionMarkStatsCacheEntry* G1RegionMarkStatsCache::find_for_add(uint region_idx) {
  uint const cache_idx = hash(region_idx);

  G1RegionMarkStatsCacheEntry* cur = &_cache[cache_idx];
  if (cur->_region_idx != region_idx) {
    evict(cache_idx);
    cur->_region_idx = region_idx;
    _cache_misses++;
  } else {
    _cache_hits++;
  }

  return cur;
}

inline void G1RegionMarkStatsCache::evict(uint idx) {
  G1RegionMarkStatsCacheEntry* cur = &_cache[idx];
  if (cur->_stats._live_words != 0) {
    Atomic::add(cur->_stats._live_words, &_target[cur->_region_idx]._live_words);
  }
  cur->clear();
}

#endif
