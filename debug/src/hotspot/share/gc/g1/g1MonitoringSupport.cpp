#include "precompiled.hpp"

#include "gc/g1/g1CollectedHeap.inline.hpp"
#include "gc/g1/g1MonitoringSupport.hpp"
#include "gc/g1/g1Policy.hpp"
#include "memory/metaspaceCounters.hpp"

G1MonitoringSupport::G1MonitoringSupport(G1CollectedHeap* g1h) :
  _g1h(g1h),
  _overall_reserved(0),
  _overall_committed(0),    _overall_used(0),
  _young_region_num(0),
  _young_gen_committed(0),
  _eden_committed(0),       _eden_used(0),
  _survivor_committed(0),   _survivor_used(0),
  _old_committed(0),        _old_used(0) {
  _overall_reserved = g1h->max_capacity();
  recalculate_sizes();

  // timer sampling for all counters supporting sampling only update the
  // used value.  See the take_sample() method.  G1 requires both used and
  // capacity updated so sampling is not currently used.  It might
  // be sufficient to update all counters in take_sample() even though
  // take_sample() only returns "used".  When sampling was used, there
  // were some anomolous values emitted which may have been the consequence
  // of not updating all values simultaneously (i.e., see the calculation done
  // in eden_space_used(), is it possible that the values used to
  // calculate either eden_used or survivor_used are being updated by
  // the collector when the sample is being done?).
  const bool sampled = false;
}

void G1MonitoringSupport::recalculate_sizes() {
  // Recalculate all the sizes from scratch. We assume that this is
  // called at a point where no concurrent updates to the various
  // values we read here are possible (i.e., at a STW phase at the end
  // of a GC).

  uint young_list_length = _g1h->young_regions_count();
  uint survivor_list_length = _g1h->survivor_regions_count();
  uint eden_list_length = young_list_length - survivor_list_length;
  // Max length includes any potential extensions to the young gen
  // we'll do when the GC locker is active.
  uint young_list_max_length = _g1h->g1_policy()->young_list_max_length();
  uint eden_list_max_length = young_list_max_length - survivor_list_length;

  _overall_used = _g1h->used_unlocked();
  _eden_used = (size_t) eden_list_length * HeapRegion::GrainBytes;
  _survivor_used = (size_t) survivor_list_length * HeapRegion::GrainBytes;
  _young_region_num = young_list_length;
  _old_used = subtract_up_to_zero(_overall_used, _eden_used + _survivor_used);

  // First calculate the committed sizes that can be calculated independently.
  _survivor_committed = _survivor_used;
  _old_committed = HeapRegion::align_up_to_region_byte_size(_old_used);

  // Next, start with the overall committed size.
  _overall_committed = _g1h->capacity();
  size_t committed = _overall_committed;

  // Remove the committed size we have calculated so far (for the
  // survivor and old space).
  committed -= _survivor_committed + _old_committed;

  // Next, calculate and remove the committed size for the eden.
  _eden_committed = (size_t) eden_list_max_length * HeapRegion::GrainBytes;
  // Somewhat defensive: be robust in case there are inaccuracies in
  // the calculations
  _eden_committed = MIN2(_eden_committed, committed);
  committed -= _eden_committed;

  // Finally, give the rest to the old space...
  _old_committed += committed;
  // ..and calculate the young gen committed.
  _young_gen_committed = _eden_committed + _survivor_committed;

  // Somewhat defensive: cap the eden used size to make sure it
  // never exceeds the committed size.
  _eden_used = MIN2(_eden_used, _eden_committed);
  // _survivor_committed and _old_committed are calculated in terms of
  // the corresponding _*_used value, so the next two conditions
  // should hold.
}

void G1MonitoringSupport::recalculate_eden_size() {
  // When a new eden region is allocated, only the eden_used size is
  // affected (since we have recalculated everything else at the last GC).

  uint young_region_num = _g1h->young_regions_count();
  if (young_region_num > _young_region_num) {
    uint diff = young_region_num - _young_region_num;
    _eden_used += (size_t) diff * HeapRegion::GrainBytes;
    // Somewhat defensive: cap the eden used size to make sure it
    // never exceeds the committed size.
    _eden_used = MIN2(_eden_used, _eden_committed);
    _young_region_num = young_region_num;
  }
}

void G1MonitoringSupport::update_sizes() {
  recalculate_sizes();
}

void G1MonitoringSupport::update_eden_size() {
  recalculate_eden_size();
}
