#include "precompiled.hpp"
#include "gc/shared/gcTraceTime.inline.hpp"
#include "logging/log.hpp"
#include "runtime/os.hpp"

GCTraceCPUTime::GCTraceCPUTime() :
  _active(log_is_enabled(Info, gc, cpu)),
  _starting_user_time(0.0),
  _starting_system_time(0.0),
  _starting_real_time(0.0) {
  if (_active) {
    bool valid = os::getTimesSecs(&_starting_real_time, &_starting_user_time, &_starting_system_time);
    if (!valid) {
      log_warning(gc, cpu)("TraceCPUTime: os::getTimesSecs() returned invalid result");
      _active = false;
    }
  }
}

GCTraceCPUTime::~GCTraceCPUTime() {
  if (_active) {
    double real_time, user_time, system_time;
    bool valid = os::getTimesSecs(&real_time, &user_time, &system_time);
    if (valid) {
      log_info(gc, cpu)("User=%3.2fs Sys=%3.2fs Real=%3.2fs",
                        user_time - _starting_user_time,
                        system_time - _starting_system_time,
                        real_time - _starting_real_time);
    } else {
      log_warning(gc, cpu)("TraceCPUTime: os::getTimesSecs() returned invalid result");
    }
  }
}
