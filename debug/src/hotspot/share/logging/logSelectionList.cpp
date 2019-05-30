#include "precompiled.hpp"
#include "logging/logSelectionList.hpp"
#include "logging/logTagSet.hpp"
#include "runtime/arguments.hpp"
#include "runtime/os.inline.hpp"

static const char* DefaultExpressionString = "all";

bool LogSelectionList::verify_selections(outputStream* out) const {
  bool valid = true;

  for (size_t i = 0; i < _nselections; i++) {
    if (_selections[i].tag_sets_selected() == 0) {
      // Return immediately unless all invalid selections should be listed
      if (out == NULL) {
        return false;
      }

      out->print("No tag set matches selection:");
      valid = false;

      char buf[256];
      _selections[i].describe_tags(buf, sizeof(buf));
      out->print(" %s. ", buf);

      _selections[i].suggest_similar_matching(out);
      out->cr();
    }
  }
  return valid;
}

bool LogSelectionList::parse(const char* str, outputStream* errstream) {
  bool success = true;
  if (str == NULL || strcmp(str, "") == 0) {
    str = DefaultExpressionString;
  }
  char* copy = os::strdup_check_oom(str, mtLogging);
  // Split string on commas
  for (char *comma_pos = copy, *cur = copy; success && comma_pos != NULL; cur = comma_pos + 1) {
    if (_nselections == MaxSelections) {
      if (errstream != NULL) {
        errstream->print_cr("Can not have more than " SIZE_FORMAT " log selections in a single configuration.",
                            MaxSelections);
      }
      success = false;
      break;
    }

    comma_pos = strchr(cur, ',');
    if (comma_pos != NULL) {
      *comma_pos = '\0';
    }

    LogSelection selection = LogSelection::parse(cur, errstream);
    if (selection == LogSelection::Invalid) {
      success = false;
      break;
    }
    _selections[_nselections++] = selection;
  }

  os::free(copy);
  return success;
}

LogLevelType LogSelectionList::level_for(const LogTagSet& ts) const {
  // Return NotMentioned if the given tagset isn't covered by this expression.
  LogLevelType level = LogLevel::NotMentioned;
  for (size_t i= 0; i < _nselections; i++) {
    if (_selections[i].selects(ts)) {
      level = _selections[i].level();
    }
  }
  return level;
}
