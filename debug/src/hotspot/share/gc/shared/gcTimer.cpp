#include "precompiled.hpp"

#include "gc/shared/gcTimer.hpp"
#include "utilities/growableArray.hpp"

// the "time" parameter for most functions
// has a default value set by Ticks::now()

void GCTimer::register_gc_start(const Ticks& time) {
  _time_partitions.clear();
  _gc_start = time;
}

void GCTimer::register_gc_end(const Ticks& time) {

  _gc_end = time;
}

void GCTimer::register_gc_pause_start(const char* name, const Ticks& time) {
  _time_partitions.report_gc_phase_start(name, time);
}

void GCTimer::register_gc_pause_end(const Ticks& time) {
  _time_partitions.report_gc_phase_end(time);
}

void GCTimer::register_gc_phase_start(const char* name, const Ticks& time) {
  _time_partitions.report_gc_phase_start(name, time);
}

void GCTimer::register_gc_phase_end(const Ticks& time) {
  _time_partitions.report_gc_phase_end(time);
}

void STWGCTimer::register_gc_start(const Ticks& time) {
  GCTimer::register_gc_start(time);
  register_gc_pause_start("GC Pause", time);
}

void STWGCTimer::register_gc_end(const Ticks& time) {
  register_gc_pause_end(time);
  GCTimer::register_gc_end(time);
}

void ConcurrentGCTimer::register_gc_pause_start(const char* name, const Ticks& time) {
  GCTimer::register_gc_pause_start(name, time);
}

void ConcurrentGCTimer::register_gc_pause_end(const Ticks& time) {
  GCTimer::register_gc_pause_end(time);
}

void ConcurrentGCTimer::register_gc_concurrent_start(const char* name, const Ticks& time) {
  _time_partitions.report_gc_phase_start(name, time, GCPhase::ConcurrentPhaseType);
  _is_concurrent_phase_active = true;
}

void ConcurrentGCTimer::register_gc_concurrent_end(const Ticks& time) {
  _time_partitions.report_gc_phase_end(time, GCPhase::ConcurrentPhaseType);
  _is_concurrent_phase_active = false;
}

void PhasesStack::clear() {
  _next_phase_level = 0;
}

void PhasesStack::push(int phase_index) {

  _phase_indices[_next_phase_level] = phase_index;
  _next_phase_level++;
}

int PhasesStack::pop() {

  _next_phase_level--;
  return _phase_indices[_next_phase_level];
}

int PhasesStack::count() const {
  return _next_phase_level;
}

TimePartitions::TimePartitions() {
  _phases = new (ResourceObj::C_HEAP, mtGC) GrowableArray<GCPhase>(INITIAL_CAPACITY, true, mtGC);
  clear();
}

TimePartitions::~TimePartitions() {
  delete _phases;
  _phases = NULL;
}

void TimePartitions::clear() {
  _phases->clear();
  _active_phases.clear();
  _sum_of_pauses = Tickspan();
  _longest_pause = Tickspan();
}

void TimePartitions::report_gc_phase_start(const char* name, const Ticks& time, GCPhase::PhaseType type) {

  int level = _active_phases.count();

  GCPhase phase;
  phase.set_type(type);
  phase.set_level(level);
  phase.set_name(name);
  phase.set_start(time);

  int index = _phases->append(phase);

  _active_phases.push(index);
}

void TimePartitions::update_statistics(GCPhase* phase) {
  if ((phase->type() == GCPhase::PausePhaseType) && (phase->level() == 0)) {
    const Tickspan pause = phase->end() - phase->start();
    _sum_of_pauses += pause;
    _longest_pause = MAX2(pause, _longest_pause);
  }
}

void TimePartitions::report_gc_phase_end(const Ticks& time, GCPhase::PhaseType type) {
  int phase_index = _active_phases.pop();
  GCPhase* phase = _phases->adr_at(phase_index);
  phase->set_end(time);
  update_statistics(phase);
}

int TimePartitions::num_phases() const {
  return _phases->length();
}

GCPhase* TimePartitions::phase_at(int index) const {

  return _phases->adr_at(index);
}

bool TimePartitions::has_active_phases() {
  return _active_phases.count() > 0;
}

bool TimePartitionPhasesIterator::has_next() {
  return _next < _time_partitions->num_phases();
}

GCPhase* TimePartitionPhasesIterator::next() {
  return _time_partitions->phase_at(_next++);
}
