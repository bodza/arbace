#include "precompiled.hpp"

#include "gc/g1/g1FullGCTask.hpp"
#include "utilities/ticks.hpp"

void G1FullGCTask::log_task(const char* name, uint worker_id, const Ticks& start, const Ticks& stop) {
  Tickspan duration = stop - start;
  double duration_ms = TimeHelper::counter_to_millis(duration.value());
}
