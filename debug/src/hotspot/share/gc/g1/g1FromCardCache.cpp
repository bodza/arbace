#include "precompiled.hpp"
#include "gc/g1/g1FromCardCache.hpp"
#include "gc/g1/g1RemSet.hpp"
#include "memory/padded.inline.hpp"
#include "utilities/debug.hpp"

uintptr_t** G1FromCardCache::_cache = NULL;
uint        G1FromCardCache::_max_regions = 0;
size_t      G1FromCardCache::_static_mem_size = 0;

void G1FromCardCache::initialize(uint num_par_rem_sets, uint max_num_regions) {
  guarantee(max_num_regions > 0, "Heap size must be valid");
  guarantee(_cache == NULL, "Should not call this multiple times");

  _max_regions = max_num_regions;
  _cache = Padded2DArray<uintptr_t, mtGC>::create_unfreeable(_max_regions,
                                                             num_par_rem_sets,
                                                             &_static_mem_size);

  if (AlwaysPreTouch) {
    invalidate(0, _max_regions);
  }
}

void G1FromCardCache::invalidate(uint start_idx, size_t new_num_regions) {
  guarantee((size_t)start_idx + new_num_regions <= max_uintx, "Trying to invalidate beyond maximum region, from %u size " SIZE_FORMAT, start_idx, new_num_regions);
  uint end_idx = (start_idx + (uint)new_num_regions);

  for (uint i = 0; i < G1RemSet::num_par_rem_sets(); i++) {
    for (uint j = start_idx; j < end_idx; j++) {
      set(i, j, InvalidCard);
    }
  }
}

void G1FromCardCache::clear(uint region_idx) {
  uint num_par_remsets = G1RemSet::num_par_rem_sets();
  for (uint i = 0; i < num_par_remsets; i++) {
    set(i, region_idx, InvalidCard);
  }
}
