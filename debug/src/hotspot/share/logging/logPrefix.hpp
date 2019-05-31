#ifndef SHARE_VM_LOGGING_LOGPREFIX_HPP
#define SHARE_VM_LOGGING_LOGPREFIX_HPP

#include "gc/shared/gcId.hpp"
#include "logging/logTag.hpp"

// Prefixes prepend each log message for a specified tagset with a given prefix.
// These prefixes are written before the log message but after the log decorations.
//
// A prefix is defined as a function that takes a buffer (with some size) as argument.
// This function will be called for each log message, and should write the prefix
// to the given buffer. The function should return how many characters it wrote,
// which should never exceed the given size.
//
// List of prefixes for specific tags and/or tagsets.
// Syntax: LOG_PREFIX(<name of prefixer function>, LOG_TAGS(<chosen log tags>))
// Where the prefixer function matches the following signature: size_t (*)(char*, size_t)

#define LOG_PREFIX_LIST \
  LOG_PREFIX(GCId::print_prefix, LOG_TAGS(gc)) \
  LOG_PREFIX(GCId::print_prefix, LOG_TAGS(gc, age)) \
  LOG_PREFIX(GCId::print_prefix, LOG_TAGS(gc, alloc)) \
  LOG_PREFIX(GCId::print_prefix, LOG_TAGS(gc, alloc, region)) \
  LOG_PREFIX(GCId::print_prefix, LOG_TAGS(gc, barrier)) \
  LOG_PREFIX(GCId::print_prefix, LOG_TAGS(gc, classhisto)) \
  LOG_PREFIX(GCId::print_prefix, LOG_TAGS(gc, compaction)) \
  LOG_PREFIX(GCId::print_prefix, LOG_TAGS(gc, cpu)) \
  LOG_PREFIX(GCId::print_prefix, LOG_TAGS(gc, ergo)) \
  LOG_PREFIX(GCId::print_prefix, LOG_TAGS(gc, ergo, cset)) \
  LOG_PREFIX(GCId::print_prefix, LOG_TAGS(gc, ergo, heap)) \
  LOG_PREFIX(GCId::print_prefix, LOG_TAGS(gc, ergo, ihop)) \
  LOG_PREFIX(GCId::print_prefix, LOG_TAGS(gc, ergo, refine)) \
  LOG_PREFIX(GCId::print_prefix, LOG_TAGS(gc, heap)) \
  LOG_PREFIX(GCId::print_prefix, LOG_TAGS(gc, heap, region)) \
  LOG_PREFIX(GCId::print_prefix, LOG_TAGS(gc, freelist)) \
  LOG_PREFIX(GCId::print_prefix, LOG_TAGS(gc, humongous)) \
  LOG_PREFIX(GCId::print_prefix, LOG_TAGS(gc, ihop)) \
  LOG_PREFIX(GCId::print_prefix, LOG_TAGS(gc, liveness)) \
  LOG_PREFIX(GCId::print_prefix, LOG_TAGS(gc, load)) \
  LOG_PREFIX(GCId::print_prefix, LOG_TAGS(gc, marking)) \
  LOG_PREFIX(GCId::print_prefix, LOG_TAGS(gc, metaspace)) \
  LOG_PREFIX(GCId::print_prefix, LOG_TAGS(gc, mmu)) \
  LOG_PREFIX(GCId::print_prefix, LOG_TAGS(gc, nmethod)) \
  LOG_PREFIX(GCId::print_prefix, LOG_TAGS(gc, phases)) \
  LOG_PREFIX(GCId::print_prefix, LOG_TAGS(gc, phases, ref)) \
  LOG_PREFIX(GCId::print_prefix, LOG_TAGS(gc, phases, start)) \
  LOG_PREFIX(GCId::print_prefix, LOG_TAGS(gc, phases, task)) \
  LOG_PREFIX(GCId::print_prefix, LOG_TAGS(gc, plab)) \
  LOG_PREFIX(GCId::print_prefix, LOG_TAGS(gc, region)) \
  LOG_PREFIX(GCId::print_prefix, LOG_TAGS(gc, remset)) \
  LOG_PREFIX(GCId::print_prefix, LOG_TAGS(gc, remset, tracking)) \
  LOG_PREFIX(GCId::print_prefix, LOG_TAGS(gc, ref)) \
  LOG_PREFIX(GCId::print_prefix, LOG_TAGS(gc, ref, start)) \
  LOG_PREFIX(GCId::print_prefix, LOG_TAGS(gc, reloc)) \
  LOG_PREFIX(GCId::print_prefix, LOG_TAGS(gc, start)) \
  LOG_PREFIX(GCId::print_prefix, LOG_TAGS(gc, stringtable)) \
  LOG_PREFIX(GCId::print_prefix, LOG_TAGS(gc, sweep)) \
  LOG_PREFIX(GCId::print_prefix, LOG_TAGS(gc, task)) \
  LOG_PREFIX(GCId::print_prefix, LOG_TAGS(gc, task, start)) \
  LOG_PREFIX(GCId::print_prefix, LOG_TAGS(gc, task, stats)) \
  LOG_PREFIX(GCId::print_prefix, LOG_TAGS(gc, task, time)) \
  LOG_PREFIX(GCId::print_prefix, LOG_TAGS(gc, tlab)) \
  LOG_PREFIX(GCId::print_prefix, LOG_TAGS(gc, workgang))

// The empty prefix, used when there's no prefix defined.
template <LogTagType T0, LogTagType T1, LogTagType T2, LogTagType T3, LogTagType T4, LogTagType GuardTag = LogTag::__NO_TAG>
struct LogPrefix : public AllStatic {
  STATIC_ASSERT(GuardTag == LogTag::__NO_TAG);
  static size_t prefix(char* buf, size_t len) {
    return 0;
  }
};

#define LOG_PREFIX(fn, ...) \
template <> struct LogPrefix<__VA_ARGS__> { \
  static size_t prefix(char* buf, size_t len) { \
    size_t ret = fn(buf, len); \
    return ret; \
  } \
};
LOG_PREFIX_LIST
#undef LOG_PREFIX

#endif
