#ifndef SHARE_VM_LOGGING_LOGDECORATIONS_HPP
#define SHARE_VM_LOGGING_LOGDECORATIONS_HPP

#include "logging/logDecorators.hpp"
#include "logging/logTagSet.hpp"

// Temporary object containing the necessary data for a log call's decorations (timestamps, etc).
class LogDecorations {
 public:
  static const int DecorationsBufferSize = 256;
 private:
  char _decorations_buffer[DecorationsBufferSize];
  char* _decoration_offset[LogDecorators::Count];
  LogLevelType _level;
  const LogTagSet& _tagset;
  jlong _millis;
  static jlong _vm_start_time_millis;
  static const char* _host_name;

  jlong java_millis();
  void create_decorations(const LogDecorators& decorators);

#define DECORATOR(name, abbr) char* create_##name##_decoration(char* pos);
  DECORATOR_LIST
#undef DECORATOR

 public:
  static void initialize(jlong vm_start_time);

  LogDecorations(LogLevelType level, const LogTagSet& tagset, const LogDecorators& decorators);

  void set_level(LogLevelType level) {
    _level = level;
  }

  const char* decoration(LogDecorators::Decorator decorator) const {
    if (decorator == LogDecorators::level_decorator) {
      return LogLevel::name(_level);
    }
    return _decoration_offset[decorator];
  }
};

#endif
