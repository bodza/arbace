#ifndef SHARE_VM_GC_SHARED_STRINGDEDUP_STRINGDEDUPQUEUE_INLINE_HPP
#define SHARE_VM_GC_SHARED_STRINGDEDUP_STRINGDEDUPQUEUE_INLINE_HPP

#include "gc/shared/stringdedup/stringDedup.hpp"
#include "gc/shared/stringdedup/stringDedupQueue.hpp"

template <typename Q>
void StringDedupQueue::create() {
  assert(StringDedup::is_enabled(), "Must be enabled");
  assert(_queue == NULL, "Can have only one queue");
  _queue = new Q;
}

void StringDedupQueue::wait() {
  queue()->wait_impl();
}

void StringDedupQueue::cancel_wait() {
  queue()->cancel_wait_impl();
}

void StringDedupQueue::push(uint worker_id, oop java_string) {
  queue()->push_impl(worker_id, java_string);
}

oop StringDedupQueue::pop() {
  return queue()->pop_impl();
}

#endif
