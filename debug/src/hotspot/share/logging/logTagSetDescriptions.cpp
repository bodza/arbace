#include "precompiled.hpp"

#include "logging/logTag.hpp"
#include "logging/logTagSet.hpp"
#include "logging/logTagSetDescriptions.hpp"

// List of described tag sets. Tags should be specified using the LOG_TAGS()
// macro. Described tag sets can be listed from command line (or DCMD) using
// -Xlog:help (or "VM.log list")
#define LOG_TAG_SET_DESCRIPTION_LIST \
  LOG_TAG_SET_DESCRIPTION(LOG_TAGS(logging), \
                          "Logging for the log framework itself")

#define LOG_TAG_SET_DESCRIPTION(tags, descr) \
  { &LogTagSetMapping<tags>::tagset(), descr },

struct LogTagSetDescription tagset_descriptions[] = {
  LOG_TAG_SET_DESCRIPTION_LIST
  { NULL, NULL }
};
