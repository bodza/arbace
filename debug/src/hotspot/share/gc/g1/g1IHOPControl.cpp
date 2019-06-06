#include "precompiled.hpp"

#include "gc/g1/g1CollectedHeap.inline.hpp"
#include "gc/g1/g1IHOPControl.hpp"
#include "gc/g1/g1Predictions.hpp"

G1IHOPControl::G1IHOPControl(double initial_ihop_percent) :
  _initial_ihop_percent(initial_ihop_percent),
  _target_occupancy(0),
  _last_allocated_bytes(0),
  _last_allocation_time_s(0.0)
{ }

void G1IHOPControl::update_target_occupancy(size_t new_target_occupancy) {
  _target_occupancy = new_target_occupancy;
}

void G1IHOPControl::update_allocation_info(double allocation_time_s, size_t allocated_bytes, size_t additional_buffer_size) {
  _last_allocation_time_s = allocation_time_s;
  _last_allocated_bytes = allocated_bytes;
}

void G1IHOPControl::print() { }

G1StaticIHOPControl::G1StaticIHOPControl(double ihop_percent) :
  G1IHOPControl(ihop_percent),
  _last_marking_length_s(0.0) {
}

G1AdaptiveIHOPControl::G1AdaptiveIHOPControl(double ihop_percent, G1Predictions const* predictor, size_t heap_reserve_percent, size_t heap_waste_percent) :
  G1IHOPControl(ihop_percent),
  _predictor(predictor),
  _marking_times_s(10, 0.95),
  _allocation_rate_s(10, 0.95),
  _last_unrestrained_young_size(0),
  _heap_reserve_percent(heap_reserve_percent),
  _heap_waste_percent(heap_waste_percent)
{ }

size_t G1AdaptiveIHOPControl::actual_target_threshold() const {
  guarantee(_target_occupancy > 0, "Target occupancy still not updated yet.");
  // The actual target threshold takes the heap reserve and the expected waste in
  // free space  into account.
  // _heap_reserve is that part of the total heap capacity that is reserved for
  // eventual promotion failure.
  // _heap_waste is the amount of space will never be reclaimed in any
  // heap, so can not be used for allocation during marking and must always be
  // considered.

  double safe_total_heap_percentage = MIN2((double)(_heap_reserve_percent + _heap_waste_percent), 100.0);

  return (size_t)MIN2(G1CollectedHeap::heap()->max_capacity() * (100.0 - safe_total_heap_percentage) / 100.0, _target_occupancy * (100.0 - _heap_waste_percent) / 100.0);
}

bool G1AdaptiveIHOPControl::have_enough_data_for_prediction() const {
  return ((size_t)_marking_times_s.num() >= G1AdaptiveIHOPNumInitialSamples) && ((size_t)_allocation_rate_s.num() >= G1AdaptiveIHOPNumInitialSamples);
}

size_t G1AdaptiveIHOPControl::get_conc_mark_start_threshold() {
  if (have_enough_data_for_prediction()) {
    double pred_marking_time = _predictor->get_new_prediction(&_marking_times_s);
    double pred_promotion_rate = _predictor->get_new_prediction(&_allocation_rate_s);
    size_t pred_promotion_size = (size_t)(pred_marking_time * pred_promotion_rate);

    size_t predicted_needed_bytes_during_marking = pred_promotion_size +
      // In reality we would need the maximum size of the young gen during
      // marking. This is a conservative estimate.
      _last_unrestrained_young_size;

    size_t internal_threshold = actual_target_threshold();
    size_t predicted_initiating_threshold = predicted_needed_bytes_during_marking < internal_threshold ? internal_threshold - predicted_needed_bytes_during_marking : 0;
    return predicted_initiating_threshold;
  } else {
    // Use the initial value.
    return (size_t)(_initial_ihop_percent * _target_occupancy / 100.0);
  }
}

void G1AdaptiveIHOPControl::update_allocation_info(double allocation_time_s, size_t allocated_bytes, size_t additional_buffer_size) {
  G1IHOPControl::update_allocation_info(allocation_time_s, allocated_bytes, additional_buffer_size);

  double allocation_rate = (double) allocated_bytes / allocation_time_s;
  _allocation_rate_s.add(allocation_rate);

  _last_unrestrained_young_size = additional_buffer_size;
}

void G1AdaptiveIHOPControl::update_marking_length(double marking_length_s) {
  _marking_times_s.add(marking_length_s);
}

void G1AdaptiveIHOPControl::print() {
  G1IHOPControl::print();
}
