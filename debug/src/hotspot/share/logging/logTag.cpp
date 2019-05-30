#include "precompiled.hpp"
#include "logging/logTag.hpp"
#include "utilities/stringUtils.hpp"
#include "utilities/globalDefinitions.hpp"
#include "utilities/ostream.hpp"
#include "utilities/quickSort.hpp"

const char* LogTag::_name[] = {
  "", // __NO_TAG
#define LOG_TAG(name) #name,
  LOG_TAG_LIST
#undef LOG_TAG
};

LogTagType LogTag::from_string(const char* str) {
  for (uint i = 0; i < LogTag::Count; i++) {
    if (strcasecmp(str, _name[i]) == 0) {
      return static_cast<LogTagType>(i);
    }
  }
  return __NO_TAG;
}

LogTagType LogTag::fuzzy_match(const char *str) {
  size_t len = strlen(str);
  LogTagType match = LogTag::__NO_TAG;
  double best = 0.5; // required similarity to be considered a match
  for (size_t i = 1; i < LogTag::Count; i++) {
    LogTagType tag = static_cast<LogTagType>(i);
    const char* tagname = LogTag::name(tag);
    double score = StringUtils::similarity(tagname, strlen(tagname), str, len);
    if (score >= best) {
      match = tag;
      best = score;
    }
  }
  return match;
}

static int cmp_logtag(LogTagType a, LogTagType b) {
  return strcmp(LogTag::name(a), LogTag::name(b));
}

static const size_t sorted_tagcount = LogTag::Count - 1; // Not counting _NO_TAG
static LogTagType sorted_tags[sorted_tagcount];

class TagSorter {
 public:
  TagSorter() {
    for (size_t i = 1; i < LogTag::Count; i++) {
      sorted_tags[i - 1] = static_cast<LogTagType>(i);
    }
    QuickSort::sort(sorted_tags, sorted_tagcount, cmp_logtag, true);
  }
};

static TagSorter tagsorter; // Sorts tags during static initialization

void LogTag::list_tags(outputStream* out) {
  for (size_t i = 0; i < sorted_tagcount; i++) {
    out->print("%s %s", (i == 0 ? "" : ","), _name[sorted_tags[i]]);
  }
  out->cr();
}
