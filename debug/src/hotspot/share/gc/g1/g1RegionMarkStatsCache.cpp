#include "precompiled.hpp"

#include "gc/g1/g1RegionMarkStatsCache.inline.hpp"
#include "memory/allocation.inline.hpp"

G1RegionMarkStatsCache::G1RegionMarkStatsCache(G1RegionMarkStats* target, uint max_regions, uint num_cache_entries) :
  _num_stats(max_regions),
  _target(target),
  _num_cache_entries(num_cache_entries),
  _cache_hits(0),
  _cache_misses(0) {

  guarantee(is_power_of_2(num_cache_entries), "Number of cache entries must be power of two, but is %u", num_cache_entries);
  _cache = NEW_C_HEAP_ARRAY(G1RegionMarkStatsCacheEntry, _num_cache_entries, mtGC);
  _num_cache_entries_mask = _num_cache_entries - 1;
}

G1RegionMarkStatsCache::~G1RegionMarkStatsCache() {
  FREE_C_HEAP_ARRAY(G1RegionMarkStatsCacheEntry, _cache);
}

// Evict all remaining statistics, returning cache hits and misses.
Pair<size_t, size_t> G1RegionMarkStatsCache::evict_all() {
  for (uint i = 0; i < _num_cache_entries; i++) {
    evict(i);
  }
  return Pair<size_t,size_t>(_cache_hits, _cache_misses);
}

// Reset all cache entries to their default values.
void G1RegionMarkStatsCache::reset() {
  _cache_hits = 0;
  _cache_misses = 0;

  for (uint i = 0; i < _num_cache_entries; i++) {
    _cache[i].clear();
  }
}
