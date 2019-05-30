#include "precompiled.hpp"
#include "gc/shared/copyFailedInfo.hpp"
#include "gc/shared/gcHeapSummary.hpp"
#include "gc/shared/gcId.hpp"
#include "gc/shared/gcTimer.hpp"
#include "gc/shared/gcTrace.hpp"
#include "gc/shared/objectCountEventSender.hpp"
#include "gc/shared/referenceProcessorStats.hpp"
#include "memory/heapInspection.hpp"
#include "memory/resourceArea.hpp"
#include "runtime/os.hpp"
#include "utilities/globalDefinitions.hpp"
#include "utilities/macros.hpp"
#include "utilities/ticks.hpp"
#include "gc/g1/evacuationInfo.hpp"

void GCTracer::report_gc_start_impl(GCCause::Cause cause, const Ticks& timestamp) {
  _shared_gc_info.set_cause(cause);
  _shared_gc_info.set_start_timestamp(timestamp);
}

void GCTracer::report_gc_start(GCCause::Cause cause, const Ticks& timestamp) {
  report_gc_start_impl(cause, timestamp);
}

void GCTracer::report_gc_end_impl(const Ticks& timestamp, TimePartitions* time_partitions) {
  _shared_gc_info.set_sum_of_pauses(time_partitions->sum_of_pauses());
  _shared_gc_info.set_longest_pause(time_partitions->longest_pause());
  _shared_gc_info.set_end_timestamp(timestamp);

  send_phase_events(time_partitions);
  send_garbage_collection_event();
}

void GCTracer::report_gc_end(const Ticks& timestamp, TimePartitions* time_partitions) {
  report_gc_end_impl(timestamp, time_partitions);
}

void GCTracer::report_gc_reference_stats(const ReferenceProcessorStats& rps) const {
  send_reference_stats_event(REF_SOFT, rps.soft_count());
  send_reference_stats_event(REF_WEAK, rps.weak_count());
  send_reference_stats_event(REF_FINAL, rps.final_count());
  send_reference_stats_event(REF_PHANTOM, rps.phantom_count());
}

void GCTracer::report_gc_heap_summary(GCWhen::Type when, const GCHeapSummary& heap_summary) const {
  send_gc_heap_summary_event(when, heap_summary);
}

void GCTracer::report_metaspace_summary(GCWhen::Type when, const MetaspaceSummary& summary) const {
  send_meta_space_summary_event(when, summary);

  send_metaspace_chunk_free_list_summary(when, Metaspace::NonClassType, summary.metaspace_chunk_free_list_summary());
  if (UseCompressedClassPointers) {
    send_metaspace_chunk_free_list_summary(when, Metaspace::ClassType, summary.class_chunk_free_list_summary());
  }
}

void YoungGCTracer::report_gc_end_impl(const Ticks& timestamp, TimePartitions* time_partitions) {
  assert(_tenuring_threshold != UNSET_TENURING_THRESHOLD, "Tenuring threshold has not been reported");

  GCTracer::report_gc_end_impl(timestamp, time_partitions);
  send_young_gc_event();

  _tenuring_threshold = UNSET_TENURING_THRESHOLD;
}

void YoungGCTracer::report_promotion_failed(const PromotionFailedInfo& pf_info) const {
  send_promotion_failed_event(pf_info);
}

void YoungGCTracer::report_tenuring_threshold(const uint tenuring_threshold) {
  _tenuring_threshold = tenuring_threshold;
}

bool YoungGCTracer::should_report_promotion_events() const {
  return should_report_promotion_in_new_plab_event() ||
          should_report_promotion_outside_plab_event();
}

bool YoungGCTracer::should_report_promotion_in_new_plab_event() const {
  return should_send_promotion_in_new_plab_event();
}

bool YoungGCTracer::should_report_promotion_outside_plab_event() const {
  return should_send_promotion_outside_plab_event();
}

void YoungGCTracer::report_promotion_in_new_plab_event(Klass* klass, size_t obj_size,
                                                       uint age, bool tenured,
                                                       size_t plab_size) const {
  send_promotion_in_new_plab_event(klass, obj_size, age, tenured, plab_size);
}

void YoungGCTracer::report_promotion_outside_plab_event(Klass* klass, size_t obj_size,
                                                        uint age, bool tenured) const {
  send_promotion_outside_plab_event(klass, obj_size, age, tenured);
}

void OldGCTracer::report_gc_end_impl(const Ticks& timestamp, TimePartitions* time_partitions) {
  GCTracer::report_gc_end_impl(timestamp, time_partitions);
  send_old_gc_event();
}

void ParallelOldTracer::report_gc_end_impl(const Ticks& timestamp, TimePartitions* time_partitions) {
  OldGCTracer::report_gc_end_impl(timestamp, time_partitions);
  send_parallel_old_event();
}

void ParallelOldTracer::report_dense_prefix(void* dense_prefix) {
  _parallel_old_gc_info.report_dense_prefix(dense_prefix);
}

void OldGCTracer::report_concurrent_mode_failure() {
  send_concurrent_mode_failure_event();
}

void G1MMUTracer::report_mmu(double time_slice_sec, double gc_time_sec, double max_time_sec) {
  send_g1_mmu_event(time_slice_sec * MILLIUNITS,
                    gc_time_sec * MILLIUNITS,
                    max_time_sec * MILLIUNITS);
}

void G1NewTracer::report_yc_type(G1YCType type) {
  _g1_young_gc_info.set_type(type);
}

void G1NewTracer::report_gc_end_impl(const Ticks& timestamp, TimePartitions* time_partitions) {
  YoungGCTracer::report_gc_end_impl(timestamp, time_partitions);
  send_g1_young_gc_event();
}

void G1NewTracer::report_evacuation_info(EvacuationInfo* info) {
  send_evacuation_info_event(info);
}

void G1NewTracer::report_evacuation_failed(EvacuationFailedInfo& ef_info) {
  send_evacuation_failed_event(ef_info);
  ef_info.reset();
}

void G1NewTracer::report_evacuation_statistics(const G1EvacSummary& young_summary, const G1EvacSummary& old_summary) const {
  send_young_evacuation_statistics(young_summary);
  send_old_evacuation_statistics(old_summary);
}

void G1NewTracer::report_basic_ihop_statistics(size_t threshold,
                                               size_t target_ccupancy,
                                               size_t current_occupancy,
                                               size_t last_allocation_size,
                                               double last_allocation_duration,
                                               double last_marking_length) {
  send_basic_ihop_statistics(threshold,
                             target_ccupancy,
                             current_occupancy,
                             last_allocation_size,
                             last_allocation_duration,
                             last_marking_length);
}

void G1NewTracer::report_adaptive_ihop_statistics(size_t threshold,
                                                  size_t internal_target_occupancy,
                                                  size_t current_occupancy,
                                                  size_t additional_buffer_size,
                                                  double predicted_allocation_rate,
                                                  double predicted_marking_length,
                                                  bool prediction_active) {
  send_adaptive_ihop_statistics(threshold,
                                internal_target_occupancy,
                                additional_buffer_size,
                                current_occupancy,
                                predicted_allocation_rate,
                                predicted_marking_length,
                                prediction_active);
}

void G1OldTracer::report_gc_start_impl(GCCause::Cause cause, const Ticks& timestamp) {
  _shared_gc_info.set_start_timestamp(timestamp);
}

void G1OldTracer::set_gc_cause(GCCause::Cause cause) {
  _shared_gc_info.set_cause(cause);
}
