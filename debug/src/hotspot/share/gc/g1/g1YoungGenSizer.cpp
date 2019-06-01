#include "precompiled.hpp"

#include "gc/g1/g1YoungGenSizer.hpp"
#include "gc/g1/heapRegion.hpp"
#include "logging/log.hpp"

G1YoungGenSizer::G1YoungGenSizer() : _sizer_kind(SizerDefaults), _adaptive_size(true), _min_desired_young_length(0), _max_desired_young_length(0) {
  if (FLAG_IS_CMDLINE(NewRatio)) {
    if (FLAG_IS_CMDLINE(NewSize) || FLAG_IS_CMDLINE(MaxNewSize)) {
    } else {
      _sizer_kind = SizerNewRatio;
      _adaptive_size = false;
      return;
    }
  }

  if (NewSize > MaxNewSize) {
    FLAG_SET_ERGO(size_t, MaxNewSize, NewSize);
  }

  if (FLAG_IS_CMDLINE(NewSize)) {
    _min_desired_young_length = MAX2((uint) (NewSize / HeapRegion::GrainBytes), 1U);
    if (FLAG_IS_CMDLINE(MaxNewSize)) {
      _max_desired_young_length = MAX2((uint) (MaxNewSize / HeapRegion::GrainBytes), 1U);
      _sizer_kind = SizerMaxAndNewSize;
      _adaptive_size = _min_desired_young_length != _max_desired_young_length;
    } else {
      _sizer_kind = SizerNewSizeOnly;
    }
  } else if (FLAG_IS_CMDLINE(MaxNewSize)) {
    _max_desired_young_length = MAX2((uint) (MaxNewSize / HeapRegion::GrainBytes), 1U);
    _sizer_kind = SizerMaxNewSizeOnly;
  }
}

uint G1YoungGenSizer::calculate_default_min_length(uint new_number_of_heap_regions) {
  uint default_value = (new_number_of_heap_regions * G1NewSizePercent) / 100;
  return MAX2(1U, default_value);
}

uint G1YoungGenSizer::calculate_default_max_length(uint new_number_of_heap_regions) {
  uint default_value = (new_number_of_heap_regions * G1MaxNewSizePercent) / 100;
  return MAX2(1U, default_value);
}

void G1YoungGenSizer::recalculate_min_max_young_length(uint number_of_heap_regions, uint* min_young_length, uint* max_young_length) {

  switch (_sizer_kind) {
    case SizerDefaults:
      *min_young_length = calculate_default_min_length(number_of_heap_regions);
      *max_young_length = calculate_default_max_length(number_of_heap_regions);
      break;
    case SizerNewSizeOnly:
      *max_young_length = calculate_default_max_length(number_of_heap_regions);
      *max_young_length = MAX2(*min_young_length, *max_young_length);
      break;
    case SizerMaxNewSizeOnly:
      *min_young_length = calculate_default_min_length(number_of_heap_regions);
      *min_young_length = MIN2(*min_young_length, *max_young_length);
      break;
    case SizerMaxAndNewSize:
      // Do nothing. Values set on the command line, don't update them at runtime.
      break;
    case SizerNewRatio:
      *min_young_length = number_of_heap_regions / (NewRatio + 1);
      *max_young_length = *min_young_length;
      break;
    default:
      ShouldNotReachHere();
  }
}

void G1YoungGenSizer::adjust_max_new_size(uint number_of_heap_regions) {

  // We need to pass the desired values because recalculation may not update these
  // values in some cases.
  uint temp = _min_desired_young_length;
  uint result = _max_desired_young_length;
  recalculate_min_max_young_length(number_of_heap_regions, &temp, &result);

  size_t max_young_size = result * HeapRegion::GrainBytes;
  if (max_young_size != MaxNewSize) {
    FLAG_SET_ERGO(size_t, MaxNewSize, max_young_size);
  }
}

void G1YoungGenSizer::heap_size_changed(uint new_number_of_heap_regions) {
  recalculate_min_max_young_length(new_number_of_heap_regions, &_min_desired_young_length,
          &_max_desired_young_length);
}
