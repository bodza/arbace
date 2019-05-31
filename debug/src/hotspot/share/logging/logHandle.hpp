#ifndef SHARE_VM_LOGGING_LOGHANDLE_HPP
#define SHARE_VM_LOGGING_LOGHANDLE_HPP

#include "logging/log.hpp"

// Wraps a Log instance and throws away the template information.
//
// This can be used to pass a Log instance as a parameter without
// polluting the surrounding API with template functions.
class LogHandle {
private:
  LogTagSet* _tagset;

public:
  template <LogTagType T0, LogTagType T1, LogTagType T2, LogTagType T3, LogTagType T4, LogTagType GuardTag>
  LogHandle(const LogImpl<T0, T1, T2, T3, T4, GuardTag>& type_carrier) :
      _tagset(&LogTagSetMapping<T0, T1, T2, T3, T4>::tagset()) { }

  bool is_level(LogLevelType level) {
    return _tagset->is_level(level);
  }

  LogTagSet* tagset() const {
    return _tagset;
  }

#define LOG_LEVEL(level, name) ATTRIBUTE_PRINTF(2, 0) \
  LogHandle& v##name(const char* fmt, va_list args) { \
    _tagset->vwrite(LogLevel::level, fmt, args); \
    return *this; \
  } \
  LogHandle& name(const char* fmt, ...) ATTRIBUTE_PRINTF(2, 3) { \
    va_list args; \
    va_start(args, fmt); \
    _tagset->vwrite(LogLevel::level, fmt, args); \
    va_end(args); \
    return *this; \
  } \
  bool is_##name() { \
    return _tagset->is_level(LogLevel::level); \
  }
  LOG_LEVEL_LIST
#undef LOG_LEVEL
};

// Wraps a LogTarget instance and throws away the template information.
//
// This can be used to pass a Log instance as a parameter without
// polluting the surrounding API with template functions.
class LogTargetHandle {
private:
  const LogLevelType _level;
  LogTagSet*         _tagset;

public:
  LogTargetHandle(LogLevelType level, LogTagSet* tagset) : _level(level), _tagset(tagset) { }

  template <LogLevelType level, LogTagType T0, LogTagType T1, LogTagType T2, LogTagType T3, LogTagType T4, LogTagType GuardTag>
  LogTargetHandle(const LogTargetImpl<level, T0, T1, T2, T3, T4, GuardTag>& type_carrier) :
      _level(level),
      _tagset(&LogTagSetMapping<T0, T1, T2, T3, T4>::tagset()) { }

  template <LogLevelType level, LogTagType T0, LogTagType T1, LogTagType T2, LogTagType T3, LogTagType T4, LogTagType GuardTag>
  static LogTargetHandle create() {
    return LogTargetHandle(LogTargetImpl<level, T0, T1, T2, T3, T4, GuardTag>());
  }

  void print(const char* fmt, ...) ATTRIBUTE_PRINTF(2, 3) {
    va_list args;
    va_start(args, fmt);
    _tagset->vwrite(_level, fmt, args);
    va_end(args);
  }

  bool is_enabled() const {
    return _tagset->is_level(_level);
  }
};

#endif
