#ifndef SHARE_VM_GC_SHARED_PRESERVEDMARKS_INLINE_HPP
#define SHARE_VM_GC_SHARED_PRESERVEDMARKS_INLINE_HPP

#include "gc/shared/preservedMarks.hpp"
#include "logging/log.hpp"
#include "oops/oop.inline.hpp"
#include "utilities/stack.inline.hpp"

inline bool PreservedMarks::should_preserve_mark(oop obj, markOop m) const {
  return m->must_be_preserved_for_promotion_failure(obj);
}

inline void PreservedMarks::push(oop obj, markOop m) {
  OopAndMarkOop elem(obj, m);
  _stack.push(elem);
}

inline void PreservedMarks::push_if_necessary(oop obj, markOop m) {
  if (should_preserve_mark(obj, m)) {
    push(obj, m);
  }
}

inline void PreservedMarks::init_forwarded_mark(oop obj) {
  obj->init_mark_raw();
}

inline void PreservedMarksSet::restore(RestorePreservedMarksTaskExecutor* executor) {
  volatile size_t total_size = 0;

  executor->restore(this, &total_size);
  assert_empty();

  log_trace(gc)("Restored " SIZE_FORMAT " marks", total_size);
}

inline PreservedMarks::PreservedMarks()
    : _stack(OopAndMarkOopStack::default_segment_size(),
             // This stack should be used very infrequently so there's
             // no point in caching stack segments (there will be a
             // waste of space most of the time). So we set the max
             // cache size to 0.
             0 /* max_cache_size */) { }

void PreservedMarks::OopAndMarkOop::set_mark() const {
  _o->set_mark_raw(_m);
}

#endif
