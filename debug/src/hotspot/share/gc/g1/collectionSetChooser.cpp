#include "precompiled.hpp"

#include "gc/g1/collectionSetChooser.hpp"
#include "gc/g1/g1CollectedHeap.inline.hpp"
#include "gc/g1/heapRegionRemSet.hpp"
#include "gc/shared/space.inline.hpp"
#include "runtime/atomic.hpp"

// Even though we don't use the GC efficiency in our heuristics as
// much as we used to, we still order according to GC efficiency. This
// will cause regions with a lot of live objects and large RSets to
// end up at the end of the array. Given that we might skip collecting
// the last few old regions, if after a few mixed GCs the remaining
// have reclaimable bytes under a certain threshold, the hope is that
// the ones we'll skip are ones with both large RSets and a lot of
// live objects, not the ones with just a lot of live objects if we
// ordered according to the amount of reclaimable bytes per region.
static int order_regions(HeapRegion* hr1, HeapRegion* hr2) {
  if (hr1 == NULL) {
    if (hr2 == NULL) {
      return 0;
    } else {
      return 1;
    }
  } else if (hr2 == NULL) {
    return -1;
  }

  double gc_eff1 = hr1->gc_efficiency();
  double gc_eff2 = hr2->gc_efficiency();
  if (gc_eff1 > gc_eff2) {
    return -1;
  } if (gc_eff1 < gc_eff2) {
    return 1;
  } else {
    return 0;
  }
}

static int order_regions(HeapRegion** hr1p, HeapRegion** hr2p) {
  return order_regions(*hr1p, *hr2p);
}

CollectionSetChooser::CollectionSetChooser() :
  // The line below is the worst bit of C++ hackery I've ever written
  // (Detlefs, 11/23).  You should think of it as equivalent to
  // "_regions(100, true)": initialize the growable array and inform it
  // that it should allocate its elem array(s) on the C heap.
  //
  // The first argument, however, is actually a comma expression
  // (set_allocation_type(this, C_HEAP), 100). The purpose of the
  // set_allocation_type() call is to replace the default allocation
  // type for embedded objects STACK_OR_EMBEDDED with C_HEAP. It will
  // allow to pass the assert in GenericGrowableArray() which checks
  // that a growable array object must be on C heap if elements are.
  //
  // Note: containing object is allocated on C heap since it is CHeapObj.
  //
  _regions((ResourceObj::set_allocation_type((address) &_regions,
                                             ResourceObj::C_HEAP),
                  100), true /* C_Heap */),
    _front(0), _end(0), _first_par_unreserved_idx(0),
    _region_live_threshold_bytes(0), _remaining_reclaimable_bytes(0) {
  _region_live_threshold_bytes = mixed_gc_live_threshold_bytes();
}

void CollectionSetChooser::sort_regions() {
  // First trim any unused portion of the top in the parallel case.
  if (_first_par_unreserved_idx > 0) {
    regions_trunc_to(_first_par_unreserved_idx);
  }
  _regions.sort(order_regions);
}

void CollectionSetChooser::add_region(HeapRegion* hr) {
  _regions.append(hr);
  _end++;
  _remaining_reclaimable_bytes += hr->reclaimable_bytes();
  hr->calc_gc_efficiency();
}

void CollectionSetChooser::push(HeapRegion* hr) {
  _front--;
  regions_at_put(_front, hr);
  _remaining_reclaimable_bytes += hr->reclaimable_bytes();
}

void CollectionSetChooser::prepare_for_par_region_addition(uint n_threads, uint n_regions, uint chunk_size) {
  _first_par_unreserved_idx = 0;
  uint max_waste = n_threads * chunk_size;
  // it should be aligned with respect to chunk_size
  uint aligned_n_regions = (n_regions + chunk_size - 1) / chunk_size * chunk_size;
  regions_at_put_grow(aligned_n_regions + max_waste - 1, NULL);
}

uint CollectionSetChooser::claim_array_chunk(uint chunk_size) {
  uint res = (uint) Atomic::add((jint) chunk_size, (volatile jint*) &_first_par_unreserved_idx);
  return res - chunk_size;
}

void CollectionSetChooser::set_region(uint index, HeapRegion* hr) {
  regions_at_put(index, hr);
  hr->calc_gc_efficiency();
}

void CollectionSetChooser::update_totals(uint region_num, size_t reclaimable_bytes) {
  // Only take the lock if we actually need to update the totals.
  if (region_num > 0) {
    // We could have just used atomics instead of taking the
    // lock. However, we currently don't have an atomic add for size_t.
    MutexLockerEx x(ParGCRareEvent_lock, Mutex::_no_safepoint_check_flag);
    _end += region_num;
    _remaining_reclaimable_bytes += reclaimable_bytes;
  }
}

void CollectionSetChooser::iterate(HeapRegionClosure* cl) {
  for (uint i = _front; i < _end; i++) {
    HeapRegion* r = regions_at(i);
    if (cl->do_heap_region(r)) {
      cl->set_incomplete();
      break;
    }
  }
}

void CollectionSetChooser::clear() {
  _regions.clear();
  _front = 0;
  _end = 0;
  _remaining_reclaimable_bytes = 0;
}

class ParKnownGarbageHRClosure: public HeapRegionClosure {
  G1CollectedHeap* _g1h;
  CSetChooserParUpdater _cset_updater;

public:
  ParKnownGarbageHRClosure(CollectionSetChooser* hrSorted, uint chunk_size) :
    _g1h(G1CollectedHeap::heap()),
    _cset_updater(hrSorted, true /* parallel */, chunk_size) { }

  bool do_heap_region(HeapRegion* r) {
    // We will skip any region that's currently used as an old GC
    // alloc region (we should not consider those for collection
    // before we fill them up).
    if (_cset_updater.should_add(r) && !_g1h->is_old_gc_alloc_region(r)) {
      _cset_updater.add_region(r);
    } else if (r->is_old()) {
      // Keep remembered sets for humongous regions, otherwise clean out remembered
      // sets for old regions.
      r->rem_set()->clear(true /* only_cardset */);
    }
    return false;
  }
};

class ParKnownGarbageTask: public AbstractGangTask {
  CollectionSetChooser* _hrSorted;
  uint _chunk_size;
  G1CollectedHeap* _g1h;
  HeapRegionClaimer _hrclaimer;

public:
  ParKnownGarbageTask(CollectionSetChooser* hrSorted, uint chunk_size, uint n_workers) :
      AbstractGangTask("ParKnownGarbageTask"),
      _hrSorted(hrSorted), _chunk_size(chunk_size),
      _g1h(G1CollectedHeap::heap()), _hrclaimer(n_workers) { }

  void work(uint worker_id) {
    ParKnownGarbageHRClosure par_known_garbage_cl(_hrSorted, _chunk_size);
    _g1h->heap_region_par_iterate_from_worker_offset(&par_known_garbage_cl, &_hrclaimer, worker_id);
  }
};

uint CollectionSetChooser::calculate_parallel_work_chunk_size(uint n_workers, uint n_regions) const {
  const uint overpartition_factor = 4;
  const uint min_chunk_size = MAX2(n_regions / n_workers, 1U);
  return MAX2(n_regions / (n_workers * overpartition_factor), min_chunk_size);
}

bool CollectionSetChooser::region_occupancy_low_enough_for_evac(size_t live_bytes) {
  return live_bytes < mixed_gc_live_threshold_bytes();
}

bool CollectionSetChooser::should_add(HeapRegion* hr) const {
  return !hr->is_young() && !hr->is_pinned() && region_occupancy_low_enough_for_evac(hr->live_bytes()) && hr->rem_set()->is_complete();
}

void CollectionSetChooser::rebuild(WorkGang* workers, uint n_regions) {
  clear();

  uint n_workers = workers->active_workers();

  uint chunk_size = calculate_parallel_work_chunk_size(n_workers, n_regions);
  prepare_for_par_region_addition(n_workers, n_regions, chunk_size);

  ParKnownGarbageTask par_known_garbage_task(this, chunk_size, n_workers);
  workers->run_task(&par_known_garbage_task);

  sort_regions();
}
