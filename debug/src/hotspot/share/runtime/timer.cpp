#include "precompiled.hpp"

#include "logging/log.hpp"
#include "oops/oop.inline.hpp"
#include "runtime/timer.hpp"
#include "utilities/ostream.hpp"

double TimeHelper::counter_to_seconds(jlong counter) {
  double freq  = (double) os::elapsed_frequency();
  return counter / freq;
}

double TimeHelper::counter_to_millis(jlong counter) {
  return counter_to_seconds(counter) * 1000.0;
}

jlong TimeHelper::millis_to_counter(jlong millis) {
  jlong freq = os::elapsed_frequency() / MILLIUNITS;
  return millis * freq;
}

elapsedTimer::elapsedTimer(jlong time, jlong timeUnitsPerSecond) {
  _active = false;
  jlong osTimeUnitsPerSecond = os::elapsed_frequency();
  while (osTimeUnitsPerSecond < timeUnitsPerSecond) {
    timeUnitsPerSecond /= 1000;
    time *= 1000;
  }
  while (osTimeUnitsPerSecond > timeUnitsPerSecond) {
    timeUnitsPerSecond *= 1000;
    time /= 1000;
  }
  _counter = time;
}

void elapsedTimer::add(elapsedTimer t) {
  _counter += t._counter;
}

void elapsedTimer::start() {
  if (!_active) {
    _active = true;
    _start_counter = os::elapsed_counter();
  }
}

void elapsedTimer::stop() {
  if (_active) {
    _counter += os::elapsed_counter() - _start_counter;
    _active = false;
  }
}

double elapsedTimer::seconds() const {
 return TimeHelper::counter_to_seconds(_counter);
}

jlong elapsedTimer::milliseconds() const {
  return (jlong)TimeHelper::counter_to_millis(_counter);
}

jlong elapsedTimer::active_ticks() const {
  if (!_active) {
    return ticks();
  }
  jlong counter = _counter + os::elapsed_counter() - _start_counter;
  return counter;
}

void TimeStamp::update_to(jlong ticks) {
  _counter = ticks;
  if (_counter == 0)  _counter = 1;
}

void TimeStamp::update() {
  update_to(os::elapsed_counter());
}

double TimeStamp::seconds() const {
  jlong new_count = os::elapsed_counter();
  return TimeHelper::counter_to_seconds(new_count - _counter);
}

jlong TimeStamp::milliseconds() const {
  jlong new_count = os::elapsed_counter();
  return (jlong)TimeHelper::counter_to_millis(new_count - _counter);
}

jlong TimeStamp::ticks_since_update() const {
  return os::elapsed_counter() - _counter;
}

TraceCPUTime::TraceCPUTime(bool doit, bool print_cr, outputStream *logfile) :
  _active(doit),
  _print_cr(print_cr),
  _starting_user_time(0.0),
  _starting_system_time(0.0),
  _starting_real_time(0.0),
  _logfile(logfile),
  _error(false) {
  if (_active) {
    if (logfile != NULL) {
      _logfile = logfile;
    } else {
      _logfile = tty;
    }

    _error = !os::getTimesSecs(&_starting_real_time,
                               &_starting_user_time,
                               &_starting_system_time);
  }
}

TraceCPUTime::~TraceCPUTime() {
  if (_active) {
    bool valid = false;
    if (!_error) {
      double real_secs;                 // walk clock time
      double system_secs;               // system time
      double user_secs;                 // user time for all threads

      double real_time, user_time, system_time;
      valid = os::getTimesSecs(&real_time, &user_time, &system_time);
      if (valid) {

        user_secs = user_time - _starting_user_time;
        system_secs = system_time - _starting_system_time;
        real_secs = real_time - _starting_real_time;

        _logfile->print(" [Times: user=%3.2f sys=%3.2f real=%3.2f secs] ",
          user_secs, system_secs, real_secs);

      } else {
        _logfile->print("[Invalid result in TraceCPUTime]");
      }
    } else {
      _logfile->print("[Error in TraceCPUTime]");
    }
    if (_print_cr) {
      _logfile->cr();
    }
    _logfile->flush();
  }
}
