#ifndef SHARE_VM_LOGGING_LOGLEVEL_HPP
#define SHARE_VM_LOGGING_LOGLEVEL_HPP

#include "memory/allocation.hpp"
#include "utilities/macros.hpp"

// The list of log levels:
//
//  trace   - Finest level of logging. Use for extensive/noisy
//            logging that can give slow-down when enabled.
//
//  debug   - A finer level of logging. Use for semi-noisy
//            logging that is does not fit the info level.
//
//  info    - General level of logging. Use for significant
//            events and/or informative summaries.
//
//  warning - Important messages that are not strictly errors.
//
//  error   - Critical messages caused by errors.
//
#define LOG_LEVEL_LIST \
  LOG_LEVEL(Trace, trace) \
  LOG_LEVEL(Debug, debug) \
  LOG_LEVEL(Info, info) \
  LOG_LEVEL(Warning, warning) \
  LOG_LEVEL(Error, error)

class LogLevel : public AllStatic {
 public:
  enum type {
    Off,
#define LOG_LEVEL(name, printname) name,
    LOG_LEVEL_LIST
#undef LOG_LEVEL
    Count,
    Invalid,
    NotMentioned,
    First = Off + 1,
    Last = Error,
    Default = Warning,
    Unspecified = Info
  };

  static const char *name(LogLevel::type level) {
    return _name[level];
  }

  static LogLevel::type from_string(const char* str);
  static LogLevel::type fuzzy_match(const char *level);

 private:
  static const char* _name[];
};

typedef LogLevel::type LogLevelType;

#endif
