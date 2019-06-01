#include "precompiled.hpp"

#include "logging/logLevel.hpp"
#include "utilities/globalDefinitions.hpp"
#include "utilities/stringUtils.hpp"

const char* LogLevel::_name[] = {
  "off",
#define LOG_LEVEL(name, printname) #printname,
  LOG_LEVEL_LIST
#undef LOG_LEVEL
};

LogLevelType LogLevel::from_string(const char* str) {
  for (uint i = 0; i < Count; i++) {
    if (strcasecmp(str, _name[i]) == 0) {
      return static_cast<LogLevelType>(i);
    }
  }
  return Invalid;
}

LogLevelType LogLevel::fuzzy_match(const char *level) {
  size_t len = strlen(level);
  LogLevelType match = LogLevel::Invalid;
  double best = 0.4; // required similarity to be considered a match
  for (uint i = 1; i < Count; i++) {
    LogLevelType cur = static_cast<LogLevelType>(i);
    const char* levelname = LogLevel::name(cur);
    double score = StringUtils::similarity(level, len, levelname, strlen(levelname));
    if (score >= best) {
      match = cur;
      best= score;
    }
  }
  return match;
}
