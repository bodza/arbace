#ifndef SHARE_VM_GC_G1_G1COLLECTEDHEAP_INLINE_HPP
#define SHARE_VM_GC_G1_G1COLLECTEDHEAP_INLINE_HPP

#include "gc/g1/g1BarrierSet.hpp"
#include "gc/g1/g1CollectedHeap.hpp"
#include "gc/g1/g1CollectorState.hpp"
#include "gc/g1/heapRegionManager.inline.hpp"
#include "gc/g1/heapRegionSet.inline.hpp"
#include "gc/shared/taskqueue.inline.hpp"
#include "runtime/orderAccess.hpp"

G1EvacStats* G1CollectedHeap::alloc_buffer_stats(InCSetState dest) {
  switch (dest.value()) {
    case InCSetState::Young:
      return &_survivor_evac_stats;
    case InCSetState::Old:
      return &_old_evac_stats;
    default:
      ShouldNotReachHere();
      return NULL; // Keep some compilers happy
  }
}

size_t G1CollectedHeap::desired_plab_sz(InCSetState dest) {
  size_t gclab_word_size = alloc_buffer_stats(dest)->desired_plab_sz(workers()->active_workers());
  // Prevent humongous PLAB sizes for two reasons:
  // * PLABs are allocated using a similar paths as oops, but should
  //   never be in a humongous region
  // * Allowing humongous PLABs needlessly churns the region free lists
  return MIN2(_humongous_object_threshold_in_words, gclab_word_size);
}

// Inline functions for G1CollectedHeap

// Return the region with the given index. It assumes the index is valid.
inline HeapRegion* G1CollectedHeap::region_at(uint index) const { return _hrm.at(index); }

inline HeapRegion* G1CollectedHeap::next_region_in_humongous(HeapRegion* hr) const {
  return _hrm.next_region_in_humongous(hr);
}

inline uint G1CollectedHeap::addr_to_region(HeapWord* addr) const {
  return (uint)(pointer_delta(addr, reserved_region().start(), sizeof(uint8_t)) >> HeapRegion::LogOfHRGrainBytes);
}

inline HeapWord* G1CollectedHeap::bottom_addr_for_region(uint index) const {
  return _hrm.reserved().start() + index * HeapRegion::GrainWords;
}

template <class T>
inline HeapRegion* G1CollectedHeap::heap_region_containing(const T addr) const {
  return _hrm.addr_to_region((HeapWord*) addr);
}

inline void G1CollectedHeap::old_set_add(HeapRegion* hr) {
  _old_set.add(hr);
}

inline void G1CollectedHeap::old_set_remove(HeapRegion* hr) {
  _old_set.remove(hr);
}

// It dirties the cards that cover the block so that the post
// write barrier never queues anything when updating objects on this
// block. It is assumed (and in fact we assert) that the block
// belongs to a young region.
inline void
G1CollectedHeap::dirty_young_block(HeapWord* start, size_t word_size) {
  HeapWord* end = start + word_size;

  MemRegion mr(start, end);
  card_table()->g1_mark_as_young(mr);
}

inline RefToScanQueue* G1CollectedHeap::task_queue(uint i) const {
  return _task_queues->queue(i);
}

inline bool G1CollectedHeap::is_marked_next(oop obj) const {
  return _cm->next_mark_bitmap()->is_marked((HeapWord*)obj);
}

inline bool G1CollectedHeap::is_in_cset(oop obj) {
  return is_in_cset((HeapWord*)obj);
}

inline bool G1CollectedHeap::is_in_cset(HeapWord* addr) {
  return _in_cset_fast_test.is_in_cset(addr);
}

bool G1CollectedHeap::is_in_cset(const HeapRegion* hr) {
  return _in_cset_fast_test.is_in_cset(hr);
}

bool G1CollectedHeap::is_in_cset_or_humongous(const oop obj) {
  return _in_cset_fast_test.is_in_cset_or_humongous((HeapWord*)obj);
}

InCSetState G1CollectedHeap::in_cset_state(const oop obj) {
  return _in_cset_fast_test.at((HeapWord*)obj);
}

void G1CollectedHeap::register_humongous_region_with_cset(uint index) {
  _in_cset_fast_test.set_humongous(index);
}

inline bool G1CollectedHeap::is_in_young(const oop obj) {
  if (obj == NULL) {
    return false;
  }
  return heap_region_containing(obj)->is_young();
}

inline bool G1CollectedHeap::is_obj_dead(const oop obj) const {
  if (obj == NULL) {
    return false;
  }
  return is_obj_dead(obj, heap_region_containing(obj));
}

inline bool G1CollectedHeap::is_obj_ill(const oop obj) const {
  if (obj == NULL) {
    return false;
  }
  return is_obj_ill(obj, heap_region_containing(obj));
}

inline bool G1CollectedHeap::is_obj_dead_full(const oop obj, const HeapRegion* hr) const {
   return !is_marked_next(obj) && !hr->is_archive();
}

inline bool G1CollectedHeap::is_obj_dead_full(const oop obj) const {
    return is_obj_dead_full(obj, heap_region_containing(obj));
}

inline void G1CollectedHeap::set_humongous_reclaim_candidate(uint region, bool value) {
  _humongous_reclaim_candidates.set_candidate(region, value);
}

inline bool G1CollectedHeap::is_humongous_reclaim_candidate(uint region) {
  return _humongous_reclaim_candidates.is_candidate(region);
}

inline void G1CollectedHeap::set_humongous_is_live(oop obj) {
  uint region = addr_to_region((HeapWord*)obj);
  // Clear the flag in the humongous_reclaim_candidates table.  Also
  // reset the entry in the _in_cset_fast_test table so that subsequent references
  // to the same humongous object do not go into the slow path again.
  // This is racy, as multiple threads may at the same time enter here, but this
  // is benign.
  // During collection we only ever clear the "candidate" flag, and only ever clear the
  // entry in the in_cset_fast_table.
  // We only ever evaluate the contents of these tables (in the VM thread) after
  // having synchronized the worker threads with the VM thread, or in the same
  // thread (i.e. within the VM thread).
  if (is_humongous_reclaim_candidate(region)) {
    set_humongous_reclaim_candidate(region, false);
    _in_cset_fast_test.clear_humongous(region);
  }
}

#endif
