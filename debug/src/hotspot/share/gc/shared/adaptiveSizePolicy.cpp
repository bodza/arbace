#include "precompiled.hpp"

#include "gc/shared/adaptiveSizePolicy.hpp"
#include "gc/shared/collectorPolicy.hpp"
#include "gc/shared/gcCause.hpp"
#include "gc/shared/gcUtil.inline.hpp"
#include "gc/shared/softRefPolicy.hpp"
#include "gc/shared/workgroup.hpp"
#include "runtime/timer.hpp"
#include "utilities/ostream.hpp"

elapsedTimer AdaptiveSizePolicy::_minor_timer;
elapsedTimer AdaptiveSizePolicy::_major_timer;
bool AdaptiveSizePolicy::_debug_perturbation = false;

// The throughput goal is implemented as
//      _throughput_goal = 1 - ( 1 / (1 + gc_cost_ratio))
// gc_cost_ratio is the ratio
//      application cost / gc cost
// For example a gc_cost_ratio of 4 translates into a
// throughput goal of .80

AdaptiveSizePolicy::AdaptiveSizePolicy(size_t init_eden_size, size_t init_promo_size, size_t init_survivor_size, double gc_pause_goal_sec, uint gc_cost_ratio) :
    _eden_size(init_eden_size),
    _promo_size(init_promo_size),
    _survivor_size(init_survivor_size),
    _gc_pause_goal_sec(gc_pause_goal_sec),
    _throughput_goal(1.0 - double(1.0 / (1.0 + (double) gc_cost_ratio))),
    _gc_overhead_limit_exceeded(false),
    _print_gc_overhead_limit_would_be_exceeded(false),
    _gc_overhead_limit_count(0),
    _latest_minor_mutator_interval_seconds(0),
    _threshold_tolerance_percent(1.0 + ThresholdTolerance/100.0),
    _young_gen_change_for_minor_throughput(0),
    _old_gen_change_for_major_throughput(0) {
  _avg_minor_pause    = new AdaptivePaddedAverage(AdaptiveTimeWeight, PausePadding);
  _avg_minor_interval = new AdaptiveWeightedAverage(AdaptiveTimeWeight);
  _avg_minor_gc_cost  = new AdaptiveWeightedAverage(AdaptiveTimeWeight);
  _avg_major_gc_cost  = new AdaptiveWeightedAverage(AdaptiveTimeWeight);

  _avg_young_live     = new AdaptiveWeightedAverage(AdaptiveSizePolicyWeight);
  _avg_old_live       = new AdaptiveWeightedAverage(AdaptiveSizePolicyWeight);
  _avg_eden_live      = new AdaptiveWeightedAverage(AdaptiveSizePolicyWeight);

  _avg_survived       = new AdaptivePaddedAverage(AdaptiveSizePolicyWeight, SurvivorPadding);
  _avg_pretenured     = new AdaptivePaddedNoZeroDevAverage(AdaptiveSizePolicyWeight, SurvivorPadding);

  _minor_pause_old_estimator = new LinearLeastSquareFit(AdaptiveSizePolicyWeight);
  _minor_pause_young_estimator = new LinearLeastSquareFit(AdaptiveSizePolicyWeight);
  _minor_collection_estimator = new LinearLeastSquareFit(AdaptiveSizePolicyWeight);
  _major_collection_estimator = new LinearLeastSquareFit(AdaptiveSizePolicyWeight);

  // Start the timers
  _minor_timer.start();

  _young_gen_policy_is_ready = false;
}

//  If the number of GC threads was set on the command line,
// use it.
//  Else
//    Calculate the number of GC threads based on the number of Java threads.
//    Calculate the number of GC threads based on the size of the heap.
//    Use the larger.

uint AdaptiveSizePolicy::calc_default_active_workers(uintx total_workers,
                                                     const uintx min_workers,
                                                     uintx active_workers,
                                                     uintx application_workers) {
  // If the user has specifically set the number of
  // GC threads, use them.

  // If the user has turned off using a dynamic number of GC threads
  // or the users has requested a specific number, set the active
  // number of workers to all the workers.

  uintx new_active_workers = total_workers;
  uintx prev_active_workers = active_workers;
  uintx active_workers_by_JT = 0;
  uintx active_workers_by_heap_size = 0;

  // Always use at least min_workers but use up to
  // GCThreadsPerJavaThreads * application threads.
  active_workers_by_JT = MAX2((uintx) GCWorkersPerJavaThread * application_workers, min_workers);

  // Choose a number of GC threads based on the current size
  // of the heap.  This may be complicated because the size of
  // the heap depends on factors such as the throughput goal.
  // Still a large heap should be collected by more GC threads.
  active_workers_by_heap_size = MAX2((size_t) 2U, Universe::heap()->capacity() / HeapSizePerGCThread);

  uintx max_active_workers = MAX2(active_workers_by_JT, active_workers_by_heap_size);

  new_active_workers = MIN2(max_active_workers, (uintx) total_workers);

  // Increase GC workers instantly but decrease them more
  // slowly.
  if (new_active_workers < prev_active_workers) {
    new_active_workers = MAX2(min_workers, (prev_active_workers + new_active_workers) / 2);
  }

  if (ForceDynamicNumberOfGCThreads) {
    // Assume this is debugging and jiggle the number of GC threads.
    if (new_active_workers == prev_active_workers) {
      if (new_active_workers < total_workers) {
        new_active_workers++;
      } else if (new_active_workers > min_workers) {
        new_active_workers--;
      }
    }
    if (new_active_workers == total_workers) {
      if (_debug_perturbation) {
        new_active_workers =  min_workers;
      }
      _debug_perturbation = !_debug_perturbation;
    }
  }

  return new_active_workers;
}

uint AdaptiveSizePolicy::calc_active_workers(uintx total_workers, uintx active_workers, uintx application_workers) {
  // If the user has specifically set the number of
  // GC threads, use them.

  // If the user has turned off using a dynamic number of GC threads
  // or the users has requested a specific number, set the active
  // number of workers to all the workers.

  uint new_active_workers;
  if (!UseDynamicNumberOfGCThreads || (!FLAG_IS_DEFAULT(ParallelGCThreads) && !ForceDynamicNumberOfGCThreads)) {
    new_active_workers = total_workers;
  } else {
    uintx min_workers = (total_workers == 1) ? 1 : 2;
    new_active_workers = calc_default_active_workers(total_workers, min_workers, active_workers, application_workers);
  }
  return new_active_workers;
}

uint AdaptiveSizePolicy::calc_active_conc_workers(uintx total_workers, uintx active_workers, uintx application_workers) {
  if (!UseDynamicNumberOfGCThreads ||
     (!FLAG_IS_DEFAULT(ConcGCThreads) && !ForceDynamicNumberOfGCThreads)) {
    return ConcGCThreads;
  } else {
    uint no_of_gc_threads = calc_default_active_workers(total_workers, 1, /* Minimum number of workers */ active_workers, application_workers);
    return no_of_gc_threads;
  }
}

bool AdaptiveSizePolicy::tenuring_threshold_change() const {
  return decrement_tenuring_threshold_for_gc_cost() || increment_tenuring_threshold_for_gc_cost() || decrement_tenuring_threshold_for_survivor_limit();
}

void AdaptiveSizePolicy::minor_collection_begin() {
  // Update the interval time
  _minor_timer.stop();
  // Save most recent collection time
  _latest_minor_mutator_interval_seconds = _minor_timer.seconds();
  _minor_timer.reset();
  _minor_timer.start();
}

void AdaptiveSizePolicy::update_minor_pause_young_estimator(double minor_pause_in_ms) {
  double eden_size_in_mbytes = ((double)_eden_size)/((double)M);
  _minor_pause_young_estimator->update(eden_size_in_mbytes,
    minor_pause_in_ms);
}

void AdaptiveSizePolicy::minor_collection_end(GCCause::Cause gc_cause) {
  // Update the pause time.
  _minor_timer.stop();

  if (!GCCause::is_user_requested_gc(gc_cause) || UseAdaptiveSizePolicyWithSystemGC) {
    double minor_pause_in_seconds = _minor_timer.seconds();
    double minor_pause_in_ms = minor_pause_in_seconds * MILLIUNITS;

    // Sample for performance counter
    _avg_minor_pause->sample(minor_pause_in_seconds);

    // Cost of collection (unit-less)
    double collection_cost = 0.0;
    if ((_latest_minor_mutator_interval_seconds > 0.0) && (minor_pause_in_seconds > 0.0)) {
      double interval_in_seconds = _latest_minor_mutator_interval_seconds + minor_pause_in_seconds;
      collection_cost = minor_pause_in_seconds / interval_in_seconds;
      _avg_minor_gc_cost->sample(collection_cost);
      // Sample for performance counter
      _avg_minor_interval->sample(interval_in_seconds);
    }

    // The policy does not have enough data until at least some
    // young collections have been done.
    _young_gen_policy_is_ready = (_avg_minor_gc_cost->count() >= AdaptiveSizePolicyReadyThreshold);

    // Calculate variables used to estimate pause time vs. gen sizes
    double eden_size_in_mbytes = ((double)_eden_size) / ((double)M);
    update_minor_pause_young_estimator(minor_pause_in_ms);
    update_minor_pause_old_estimator(minor_pause_in_ms);

    // Calculate variable used to estimate collection cost vs. gen sizes
    _minor_collection_estimator->update(eden_size_in_mbytes, collection_cost);
  }

  // Interval times use this timer to measure the mutator time.
  // Reset the timer after the GC pause.
  _minor_timer.reset();
  _minor_timer.start();
}

size_t AdaptiveSizePolicy::eden_increment(size_t cur_eden, uint percent_change) {
  size_t eden_heap_delta;
  eden_heap_delta = cur_eden / 100 * percent_change;
  return eden_heap_delta;
}

size_t AdaptiveSizePolicy::eden_increment(size_t cur_eden) {
  return eden_increment(cur_eden, YoungGenerationSizeIncrement);
}

size_t AdaptiveSizePolicy::eden_decrement(size_t cur_eden) {
  size_t eden_heap_delta = eden_increment(cur_eden) / AdaptiveSizeDecrementScaleFactor;
  return eden_heap_delta;
}

size_t AdaptiveSizePolicy::promo_increment(size_t cur_promo, uint percent_change) {
  size_t promo_heap_delta;
  promo_heap_delta = cur_promo / 100 * percent_change;
  return promo_heap_delta;
}

size_t AdaptiveSizePolicy::promo_increment(size_t cur_promo) {
  return promo_increment(cur_promo, TenuredGenerationSizeIncrement);
}

size_t AdaptiveSizePolicy::promo_decrement(size_t cur_promo) {
  size_t promo_heap_delta = promo_increment(cur_promo);
  promo_heap_delta = promo_heap_delta / AdaptiveSizeDecrementScaleFactor;
  return promo_heap_delta;
}

double AdaptiveSizePolicy::time_since_major_gc() const {
  _major_timer.stop();
  double result = _major_timer.seconds();
  _major_timer.start();
  return result;
}

// Linear decay of major gc cost
double AdaptiveSizePolicy::decaying_major_gc_cost() const {
  double major_interval = major_gc_interval_average_for_decay();
  double major_gc_cost_average = major_gc_cost();
  double decayed_major_gc_cost = major_gc_cost_average;
  if (time_since_major_gc() > 0.0) {
    decayed_major_gc_cost = major_gc_cost() *
      (((double) AdaptiveSizeMajorGCDecayTimeScale) * major_interval)
      / time_since_major_gc();
  }

  // The decayed cost should always be smaller than the
  // average cost but the vagaries of finite arithmetic could
  // produce a larger value in decayed_major_gc_cost so protect
  // against that.
  return MIN2(major_gc_cost_average, decayed_major_gc_cost);
}

// Use a value of the major gc cost that has been decayed
// by the factor
//
//      average-interval-between-major-gc * AdaptiveSizeMajorGCDecayTimeScale /
//        time-since-last-major-gc
//
// if the average-interval-between-major-gc * AdaptiveSizeMajorGCDecayTimeScale
// is less than time-since-last-major-gc.
//
// In cases where there are initial major gc's that
// are of a relatively high cost but no later major
// gc's, the total gc cost can remain high because
// the major gc cost remains unchanged (since there are no major
// gc's).  In such a situation the value of the unchanging
// major gc cost can keep the mutator throughput below
// the goal when in fact the major gc cost is becoming diminishingly
// small.  Use the decaying gc cost only to decide whether to
// adjust for throughput.  Using it also to determine the adjustment
// to be made for throughput also seems reasonable but there is
// no test case to use to decide if it is the right thing to do
// don't do it yet.

double AdaptiveSizePolicy::decaying_gc_cost() const {
  double decayed_major_gc_cost = major_gc_cost();
  double avg_major_interval = major_gc_interval_average_for_decay();
  if (UseAdaptiveSizeDecayMajorGCCost && (AdaptiveSizeMajorGCDecayTimeScale > 0) && (avg_major_interval > 0.00)) {
    double time_since_last_major_gc = time_since_major_gc();

    // Decay the major gc cost?
    if (time_since_last_major_gc >
        ((double) AdaptiveSizeMajorGCDecayTimeScale) * avg_major_interval) {
      // Decay using the time-since-last-major-gc
      decayed_major_gc_cost = decaying_major_gc_cost();
    }
  }
  double result = MIN2(1.0, decayed_major_gc_cost + minor_gc_cost());
  return result;
}

void AdaptiveSizePolicy::clear_generation_free_space_flags() {
  set_change_young_gen_for_min_pauses(0);
  set_change_old_gen_for_maj_pauses(0);

  set_change_old_gen_for_throughput(0);
  set_change_young_gen_for_throughput(0);
  set_decrease_for_footprint(0);
  set_decide_at_full_gc(0);
}

void AdaptiveSizePolicy::check_gc_overhead_limit(size_t young_live, size_t eden_live, size_t max_old_gen_size, size_t max_eden_size, bool is_full_gc, GCCause::Cause gc_cause, SoftRefPolicy* soft_ref_policy) {
  // Ignore explicit GC's.  Exiting here does not set the flag and
  // does not reset the count.  Updating of the averages for system
  // GC's is still controlled by UseAdaptiveSizePolicyWithSystemGC.
  if (GCCause::is_user_requested_gc(gc_cause) || GCCause::is_serviceability_requested_gc(gc_cause)) {
    return;
  }
  // eden_limit is the upper limit on the size of eden based on
  // the maximum size of the young generation and the sizes
  // of the survivor space.
  // The question being asked is whether the gc costs are high
  // and the space being recovered by a collection is low.
  // free_in_young_gen is the free space in the young generation
  // after a collection and promo_live is the free space in the old
  // generation after a collection.
  //
  // Use the minimum of the current value of the live in the
  // young gen or the average of the live in the young gen.
  // If the current value drops quickly, that should be taken
  // into account (i.e., don't trigger if the amount of free
  // space has suddenly jumped up).  If the current is much
  // higher than the average, use the average since it represents
  // the longer term behavior.
  const size_t live_in_eden = MIN2(eden_live, (size_t) avg_eden_live()->average());
  const size_t free_in_eden = max_eden_size > live_in_eden ? max_eden_size - live_in_eden : 0;
  const size_t free_in_old_gen = (size_t)(max_old_gen_size - avg_old_live()->average());
  const size_t total_free_limit = free_in_old_gen + free_in_eden;
  const size_t total_mem = max_old_gen_size + max_eden_size;
  const double mem_free_limit = total_mem * (GCHeapFreeLimit/100.0);
  const double mem_free_old_limit = max_old_gen_size * (GCHeapFreeLimit/100.0);
  const double mem_free_eden_limit = max_eden_size * (GCHeapFreeLimit/100.0);
  const double gc_cost_limit = GCTimeLimit/100.0;
  size_t promo_limit = (size_t)(max_old_gen_size - avg_old_live()->average());
  // But don't force a promo size below the current promo size. Otherwise,
  // the promo size will shrink for no good reason.
  promo_limit = MAX2(promo_limit, _promo_size);

  if (is_full_gc) {
    if (gc_cost() > gc_cost_limit && free_in_old_gen < (size_t) mem_free_old_limit && free_in_eden < (size_t) mem_free_eden_limit) {
      // Collections, on average, are taking too much time, and
      //      gc_cost() > gc_cost_limit
      // we have too little space available after a full gc.
      //      total_free_limit < mem_free_limit
      // where
      //   total_free_limit is the free space available in
      //     both generations
      //   total_mem is the total space available for allocation
      //     in both generations (survivor spaces are not included
      //     just as they are not included in eden_limit).
      //   mem_free_limit is a fraction of total_mem judged to be an
      //     acceptable amount that is still unused.
      // The heap can ask for the value of this variable when deciding
      // whether to thrown an OutOfMemory error.
      // Note that the gc time limit test only works for the collections
      // of the young gen + tenured gen and not for collections of the
      // permanent gen.  That is because the calculation of the space
      // freed by the collection is the free space in the young gen +
      // tenured gen.
      // At this point the GC overhead limit is being exceeded.
      inc_gc_overhead_limit_count();
      if (UseGCOverheadLimit) {
        if (gc_overhead_limit_count() >=
            AdaptiveSizePolicyGCTimeLimitThreshold) {
          // All conditions have been met for throwing an out-of-memory
          set_gc_overhead_limit_exceeded(true);
          // Avoid consecutive OOM due to the gc time limit by resetting
          // the counter.
          reset_gc_overhead_limit_count();
        } else {
          // The required consecutive collections which exceed the
          // GC time limit may or may not have been reached. We
          // are approaching that condition and so as not to
          // throw an out-of-memory before all SoftRef's have been
          // cleared, set _should_clear_all_soft_refs in CollectorPolicy.
          // The clearing will be done on the next GC.
          bool near_limit = gc_overhead_limit_near();
          if (near_limit) {
            soft_ref_policy->set_should_clear_all_soft_refs(true);
          }
        }
      }
    } else {
      // Did not exceed overhead limits
      reset_gc_overhead_limit_count();
    }
  }

  if (UseGCOverheadLimit) {
    if (gc_overhead_limit_exceeded()) {
      reset_gc_overhead_limit_count();
    }
  }
}

bool AdaptiveSizePolicy::print() const {
    return false;
}
