#include "precompiled.hpp"

#include "gc/g1/g1CollectedHeap.hpp"
#include "gc/g1/g1CollectionSet.hpp"
#include "gc/g1/g1CollectorState.hpp"
#include "gc/g1/g1Policy.hpp"
#include "gc/g1/heapRegion.inline.hpp"
#include "gc/g1/heapRegionRemSet.hpp"
#include "gc/g1/heapRegionSet.hpp"
#include "utilities/debug.hpp"
#include "utilities/quickSort.hpp"

G1CollectorState* G1CollectionSet::collector_state() {
  return _g1h->collector_state();
}

G1GCPhaseTimes* G1CollectionSet::phase_times() {
  return _policy->phase_times();
}

CollectionSetChooser* G1CollectionSet::cset_chooser() {
  return _cset_chooser;
}

double G1CollectionSet::predict_region_elapsed_time_ms(HeapRegion* hr) {
  return _policy->predict_region_elapsed_time_ms(hr, collector_state()->in_young_only_phase());
}

G1CollectionSet::G1CollectionSet(G1CollectedHeap* g1h, G1Policy* policy) :
  _g1h(g1h),
  _policy(policy),
  _cset_chooser(new CollectionSetChooser()),
  _eden_region_length(0),
  _survivor_region_length(0),
  _old_region_length(0),
  _bytes_used_before(0),
  _recorded_rs_lengths(0),
  _collection_set_regions(NULL),
  _collection_set_cur_length(0),
  _collection_set_max_length(0),
  // Incremental CSet attributes
  _inc_build_state(Inactive),
  _inc_bytes_used_before(0),
  _inc_recorded_rs_lengths(0),
  _inc_recorded_rs_lengths_diffs(0),
  _inc_predicted_elapsed_time_ms(0.0),
  _inc_predicted_elapsed_time_ms_diffs(0.0) {
}

G1CollectionSet::~G1CollectionSet() {
  if (_collection_set_regions != NULL) {
    FREE_C_HEAP_ARRAY(uint, _collection_set_regions);
  }
  delete _cset_chooser;
}

void G1CollectionSet::init_region_lengths(uint eden_cset_region_length, uint survivor_cset_region_length) {
  _eden_region_length     = eden_cset_region_length;
  _survivor_region_length = survivor_cset_region_length;

  _old_region_length      = 0;
}

void G1CollectionSet::initialize(uint max_region_length) {
  guarantee(_collection_set_regions == NULL, "Must only initialize once.");
  _collection_set_max_length = max_region_length;
  _collection_set_regions = NEW_C_HEAP_ARRAY(uint, max_region_length, mtGC);
}

void G1CollectionSet::set_recorded_rs_lengths(size_t rs_lengths) {
  _recorded_rs_lengths = rs_lengths;
}

// Add the heap region at the head of the non-incremental collection set
void G1CollectionSet::add_old_region(HeapRegion* hr) {
  _g1h->register_old_region_with_cset(hr);

  _collection_set_regions[_collection_set_cur_length++] = hr->hrm_index();

  _bytes_used_before += hr->used();
  size_t rs_length = hr->rem_set()->occupied();
  _recorded_rs_lengths += rs_length;
  _old_region_length += 1;
}

// Initialize the per-collection-set information
void G1CollectionSet::start_incremental_building() {
  _inc_bytes_used_before = 0;

  _inc_recorded_rs_lengths = 0;
  _inc_recorded_rs_lengths_diffs = 0;
  _inc_predicted_elapsed_time_ms = 0.0;
  _inc_predicted_elapsed_time_ms_diffs = 0.0;
  _inc_build_state = Active;
}

void G1CollectionSet::finalize_incremental_building() {
  // The two "main" fields, _inc_recorded_rs_lengths and
  // _inc_predicted_elapsed_time_ms, are updated by the thread
  // that adds a new region to the CSet. Further updates by the
  // concurrent refinement thread that samples the young RSet lengths
  // are accumulated in the *_diffs fields. Here we add the diffs to
  // the "main" fields.

  if (_inc_recorded_rs_lengths_diffs >= 0) {
    _inc_recorded_rs_lengths += _inc_recorded_rs_lengths_diffs;
  } else {
    // This is defensive. The diff should in theory be always positive
    // as RSets can only grow between GCs. However, given that we
    // sample their size concurrently with other threads updating them
    // it's possible that we might get the wrong size back, which
    // could make the calculations somewhat inaccurate.
    size_t diffs = (size_t) (-_inc_recorded_rs_lengths_diffs);
    if (_inc_recorded_rs_lengths >= diffs) {
      _inc_recorded_rs_lengths -= diffs;
    } else {
      _inc_recorded_rs_lengths = 0;
    }
  }
  _inc_predicted_elapsed_time_ms += _inc_predicted_elapsed_time_ms_diffs;

  _inc_recorded_rs_lengths_diffs = 0;
  _inc_predicted_elapsed_time_ms_diffs = 0.0;
}

void G1CollectionSet::clear() {
  _collection_set_cur_length = 0;
}

void G1CollectionSet::iterate(HeapRegionClosure* cl) const {
  iterate_from(cl, 0, 1);
}

void G1CollectionSet::iterate_from(HeapRegionClosure* cl, uint worker_id, uint total_workers) const {
  size_t len = _collection_set_cur_length;
  OrderAccess::loadload();
  if (len == 0) {
    return;
  }
  size_t start_pos = (worker_id * len) / total_workers;
  size_t cur_pos = start_pos;

  do {
    HeapRegion* r = _g1h->region_at(_collection_set_regions[cur_pos]);
    bool result = cl->do_heap_region(r);
    if (result) {
      cl->set_incomplete();
      return;
    }
    cur_pos++;
    if (cur_pos == len) {
      cur_pos = 0;
    }
  } while (cur_pos != start_pos);
}

void G1CollectionSet::update_young_region_prediction(HeapRegion* hr, size_t new_rs_length) {
  // Update the CSet information that is dependent on the new RS length

  // We could have updated _inc_recorded_rs_lengths and
  // _inc_predicted_elapsed_time_ms directly but we'd need to do
  // that atomically, as this code is executed by a concurrent
  // refinement thread, potentially concurrently with a mutator thread
  // allocating a new region and also updating the same fields. To
  // avoid the atomic operations we accumulate these updates on two
  // separate fields (*_diffs) and we'll just add them to the "main"
  // fields at the start of a GC.

  ssize_t old_rs_length = (ssize_t) hr->recorded_rs_length();
  ssize_t rs_lengths_diff = (ssize_t) new_rs_length - old_rs_length;
  _inc_recorded_rs_lengths_diffs += rs_lengths_diff;

  double old_elapsed_time_ms = hr->predicted_elapsed_time_ms();
  double new_region_elapsed_time_ms = predict_region_elapsed_time_ms(hr);
  double elapsed_ms_diff = new_region_elapsed_time_ms - old_elapsed_time_ms;
  _inc_predicted_elapsed_time_ms_diffs += elapsed_ms_diff;

  hr->set_recorded_rs_length(new_rs_length);
  hr->set_predicted_elapsed_time_ms(new_region_elapsed_time_ms);
}

void G1CollectionSet::add_young_region_common(HeapRegion* hr) {
  size_t collection_set_length = _collection_set_cur_length;
  hr->set_young_index_in_cset((int)collection_set_length);

  _collection_set_regions[collection_set_length] = hr->hrm_index();
  // Concurrent readers must observe the store of the value in the array before an
  // update to the length field.
  OrderAccess::storestore();
  _collection_set_cur_length++;

  // This routine is used when:
  // * adding survivor regions to the incremental cset at the end of an
  //   evacuation pause or
  // * adding the current allocation region to the incremental cset
  //   when it is retired.
  // Therefore this routine may be called at a safepoint by the
  // VM thread, or in-between safepoints by mutator threads (when
  // retiring the current allocation region)
  // We need to clear and set the cached recorded/cached collection set
  // information in the heap region here (before the region gets added
  // to the collection set). An individual heap region's cached values
  // are calculated, aggregated with the policy collection set info,
  // and cached in the heap region here (initially) and (subsequently)
  // by the Young List sampling code.
  // Ignore calls to this due to retirement during full gc.

  if (!_g1h->collector_state()->in_full_gc()) {
    size_t rs_length = hr->rem_set()->occupied();
    double region_elapsed_time_ms = predict_region_elapsed_time_ms(hr);

    // Cache the values we have added to the aggregated information
    // in the heap region in case we have to remove this region from
    // the incremental collection set, or it is updated by the
    // rset sampling code
    hr->set_recorded_rs_length(rs_length);
    hr->set_predicted_elapsed_time_ms(region_elapsed_time_ms);

    _inc_recorded_rs_lengths += rs_length;
    _inc_predicted_elapsed_time_ms += region_elapsed_time_ms;
    _inc_bytes_used_before += hr->used();
  }

  _g1h->register_young_region_with_cset(hr);
}

void G1CollectionSet::add_survivor_regions(HeapRegion* hr) {
  add_young_region_common(hr);
}

void G1CollectionSet::add_eden_region(HeapRegion* hr) {
  add_young_region_common(hr);
}

double G1CollectionSet::finalize_young_part(double target_pause_time_ms, G1SurvivorRegions* survivors) {
  double young_start_time_sec = os::elapsedTime();

  finalize_incremental_building();

  guarantee(target_pause_time_ms > 0.0, "target_pause_time_ms = %1.6lf should be positive", target_pause_time_ms);

  size_t pending_cards = _policy->pending_cards();
  double base_time_ms = _policy->predict_base_elapsed_time_ms(pending_cards);
  double time_remaining_ms = MAX2(target_pause_time_ms - base_time_ms, 0.0);

  // The young list is laid with the survivor regions from the previous
  // pause are appended to the RHS of the young list, i.e.
  //   [Newly Young Regions ++ Survivors from last pause].

  uint survivor_region_length = survivors->length();
  uint eden_region_length = _g1h->eden_regions_count();
  init_region_lengths(eden_region_length, survivor_region_length);

  verify_young_cset_indices();

  // Clear the fields that point to the survivor list - they are all young now.
  survivors->convert_to_eden();

  _bytes_used_before = _inc_bytes_used_before;
  time_remaining_ms = MAX2(time_remaining_ms - _inc_predicted_elapsed_time_ms, 0.0);

  // The number of recorded young regions is the incremental
  // collection set's current size
  set_recorded_rs_lengths(_inc_recorded_rs_lengths);

  double young_end_time_sec = os::elapsedTime();
  phase_times()->record_young_cset_choice_time_ms((young_end_time_sec - young_start_time_sec) * 1000.0);

  return time_remaining_ms;
}

static int compare_region_idx(const uint a, const uint b) {
  if (a > b) {
    return 1;
  } else if (a == b) {
    return 0;
  } else {
    return -1;
  }
}

void G1CollectionSet::finalize_old_part(double time_remaining_ms) {
  double non_young_start_time_sec = os::elapsedTime();
  double predicted_old_time_ms = 0.0;

  if (collector_state()->in_mixed_phase()) {
    const uint min_old_cset_length = _policy->calc_min_old_cset_length();
    const uint max_old_cset_length = _policy->calc_max_old_cset_length();

    uint expensive_region_num = 0;
    bool check_time_remaining = _policy->adaptive_young_list_length();

    HeapRegion* hr = cset_chooser()->peek();
    while (hr != NULL) {
      if (old_region_length() >= max_old_cset_length) {
        // Added maximum number of old regions to the CSet.
        break;
      }

      // Stop adding regions if the remaining reclaimable space is
      // not above G1HeapWastePercent.
      size_t reclaimable_bytes = cset_chooser()->remaining_reclaimable_bytes();
      double reclaimable_percent = _policy->reclaimable_bytes_percent(reclaimable_bytes);
      double threshold = (double) G1HeapWastePercent;
      if (reclaimable_percent <= threshold) {
        // We've added enough old regions that the amount of uncollected
        // reclaimable space is at or below the waste threshold. Stop
        // adding old regions to the CSet.
        break;
      }

      double predicted_time_ms = predict_region_elapsed_time_ms(hr);
      if (check_time_remaining) {
        if (predicted_time_ms > time_remaining_ms) {
          // Too expensive for the current CSet.

          if (old_region_length() >= min_old_cset_length) {
            // We have added the minimum number of old regions to the CSet,
            // we are done with this CSet.
            break;
          }

          // We'll add it anyway given that we haven't reached the
          // minimum number of old regions.
          expensive_region_num += 1;
        }
      } else {
        if (old_region_length() >= min_old_cset_length) {
          // In the non-auto-tuning case, we'll finish adding regions
          // to the CSet if we reach the minimum.

          break;
        }
      }

      // We will add this region to the CSet.
      time_remaining_ms = MAX2(time_remaining_ms - predicted_time_ms, 0.0);
      predicted_old_time_ms += predicted_time_ms;
      cset_chooser()->pop(); // already have region via peek()
      _g1h->old_set_remove(hr);
      add_old_region(hr);

      hr = cset_chooser()->peek();
    }
  }

  stop_incremental_building();

  double non_young_end_time_sec = os::elapsedTime();
  phase_times()->record_non_young_cset_choice_time_ms((non_young_end_time_sec - non_young_start_time_sec) * 1000.0);

  QuickSort::sort(_collection_set_regions, _collection_set_cur_length, compare_region_idx, true);
}
