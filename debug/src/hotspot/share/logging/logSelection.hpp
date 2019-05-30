#ifndef SHARE_VM_LOGGING_LOGSELECTION_HPP
#define SHARE_VM_LOGGING_LOGSELECTION_HPP

#include "logging/logLevel.hpp"
#include "logging/logTag.hpp"
#include "memory/allocation.hpp"

class LogTagSet;

// Class representing a selection of tags with for a given level.
// Consists of a set of tags, an optional wildcard flag, and a level, e.g. "tag1+tag2*=level".
class LogSelection : public StackObj {
  friend class LogSelectionList;

 private:
  size_t _ntags;
  LogTagType _tags[LogTag::MaxTags];
  bool _wildcard;
  LogLevelType _level;
  size_t _tag_sets_selected;

  LogSelection();

 public:
  static const LogSelection Invalid;

  static LogSelection parse(const char* str, outputStream* error_stream = NULL);

  LogSelection(const LogTagType tags[LogTag::MaxTags], bool wildcard, LogLevelType level);

  bool operator==(const LogSelection& ref) const;
  bool operator!=(const LogSelection& ref) const;

  size_t ntags() const;
  LogLevelType level() const;
  size_t tag_sets_selected() const;

  bool selects(const LogTagSet& ts) const;
  bool consists_of(const LogTagType tags[LogTag::MaxTags]) const;

  int describe_tags(char* buf, size_t bufsize) const;
  int describe(char* buf, size_t bufsize) const;

  // List similar selections that matches existing tag sets on the given outputstream
  void suggest_similar_matching(outputStream* out) const;

  // Compute a similarity measure in the range [0, 1], where higher means more similar
  double similarity(const LogSelection& other) const;
};

#endif
