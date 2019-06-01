#include "precompiled.hpp"

#include "gc/shared/gcTraceTime.inline.hpp"
#include "logging/log.hpp"
#include "runtime/os.hpp"

GCTraceCPUTime::GCTraceCPUTime() :
  _active(false),
  _starting_user_time(0.0),
  _starting_system_time(0.0),
  _starting_real_time(0.0) {
  if (_active) {
    bool valid = os::getTimesSecs(&_starting_real_time, &_starting_user_time, &_starting_system_time);
    if (!valid) {
      _active = false;
    }
  }
}

GCTraceCPUTime::~GCTraceCPUTime() {
  if (_active) {
    double real_time, user_time, system_time;
    bool valid = os::getTimesSecs(&real_time, &user_time, &system_time);
  }
}
