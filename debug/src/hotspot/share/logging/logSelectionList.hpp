#ifndef SHARE_VM_LOGGING_LOGSELECTIONLIST_HPP
#define SHARE_VM_LOGGING_LOGSELECTIONLIST_HPP

#include "logging/logConfiguration.hpp"
#include "logging/logSelection.hpp"
#include "logging/logTag.hpp"
#include "memory/allocation.hpp"
#include "utilities/debug.hpp"
#include "utilities/globalDefinitions.hpp"

class LogTagSet;

// Class used to temporary encode a series of log selections during log configuration.
// Consists of ordered LogSelections, i.e. "tag1+tag2=level1,tag3*=level2".
class LogSelectionList : public StackObj {
 public:
  static const size_t MaxSelections = 256;

 private:
  friend void LogConfiguration::configure_stdout(LogLevelType, int, ...);

  size_t _nselections;
  LogSelection _selections[MaxSelections];

 public:
  LogSelectionList() : _nselections(0) { }

  LogSelectionList(const LogSelection& selection) : _nselections(1) {
    _selections[0] = selection;
  }

  bool parse(const char* str, outputStream* errstream = NULL);
  LogLevelType level_for(const LogTagSet& ts) const;

  // Verify that each selection actually selects something.
  // Returns false if some invalid selection was found. If given an outputstream,
  // this function will list all the invalid selections on the stream.
  bool verify_selections(outputStream* out = NULL) const;
};

#endif
