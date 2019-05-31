#include "precompiled.hpp"
#include "gc/g1/dirtyCardQueue.hpp"
#include "gc/g1/g1CollectedHeap.inline.hpp"
#include "gc/g1/g1HotCardCache.hpp"
#include "runtime/atomic.hpp"

G1HotCardCache::G1HotCardCache(G1CollectedHeap *g1h):
  _g1h(g1h), _hot_cache(NULL), _use_cache(false), _card_counts(g1h) { }

void G1HotCardCache::initialize(G1RegionToSpaceMapper* card_counts_storage) {
  if (default_use_cache()) {
    _use_cache = true;

    _hot_cache_size = (size_t)1 << G1ConcRSLogCacheSize;
    _hot_cache = ArrayAllocator<jbyte*>::allocate(_hot_cache_size, mtGC);

    reset_hot_cache_internal();

    // For refining the cards in the hot cache in parallel
    _hot_cache_par_chunk_size = ClaimChunkSize;
    _hot_cache_par_claimed_idx = 0;

    _card_counts.initialize(card_counts_storage);
  }
}

G1HotCardCache::~G1HotCardCache() {
  if (default_use_cache()) {
    ArrayAllocator<jbyte*>::free(_hot_cache, _hot_cache_size);
    _hot_cache = NULL;
  }
}

jbyte* G1HotCardCache::insert(jbyte* card_ptr) {
  uint count = _card_counts.add_card_count(card_ptr);
  if (!_card_counts.is_hot(count)) {
    // The card is not hot so do not store it in the cache;
    // return it for immediate refining.
    return card_ptr;
  }
  // Otherwise, the card is hot.
  size_t index = Atomic::add(1u, &_hot_cache_idx) - 1;
  size_t masked_index = index & (_hot_cache_size - 1);
  jbyte* current_ptr = _hot_cache[masked_index];

  // Try to store the new card pointer into the cache. Compare-and-swap to guard
  // against the unlikely event of a race resulting in another card pointer to
  // have already been written to the cache. In this case we will return
  // card_ptr in favor of the other option, which would be starting over. This
  // should be OK since card_ptr will likely be the older card already when/if
  // this ever happens.
  jbyte* previous_ptr = Atomic::cmpxchg(card_ptr,
                                        &_hot_cache[masked_index],
                                        current_ptr);
  return (previous_ptr == current_ptr) ? previous_ptr : card_ptr;
}

void G1HotCardCache::drain(CardTableEntryClosure* cl, uint worker_i) {

  while (_hot_cache_par_claimed_idx < _hot_cache_size) {
    size_t end_idx = Atomic::add(_hot_cache_par_chunk_size,
                                 &_hot_cache_par_claimed_idx);
    size_t start_idx = end_idx - _hot_cache_par_chunk_size;
    // The current worker has successfully claimed the chunk [start_idx..end_idx)
    end_idx = MIN2(end_idx, _hot_cache_size);
    for (size_t i = start_idx; i < end_idx; i++) {
      jbyte* card_ptr = _hot_cache[i];
      if (card_ptr != NULL) {
        bool result = cl->do_card_ptr(card_ptr, worker_i);
      } else {
        break;
      }
    }
  }

  // The existing entries in the hot card cache, which were just refined
  // above, are discarded prior to re-enabling the cache near the end of the GC.
}

void G1HotCardCache::reset_card_counts(HeapRegion* hr) {
  _card_counts.clear_region(hr);
}
