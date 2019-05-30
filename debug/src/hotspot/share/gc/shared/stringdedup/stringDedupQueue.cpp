#include "precompiled.hpp"

#include "gc/shared/stringdedup/stringDedup.hpp"
#include "gc/shared/stringdedup/stringDedupQueue.hpp"
#include "runtime/atomic.hpp"

StringDedupQueue* StringDedupQueue::_queue = NULL;
volatile size_t   StringDedupQueue::_claimed_index = 0;

size_t StringDedupQueue::claim() {
  return Atomic::add(size_t(1), &_claimed_index) - 1;
}

void StringDedupQueue::unlink_or_oops_do(StringDedupUnlinkOrOopsDoClosure* cl) {
  size_t claimed_queue = claim();
  while (claimed_queue < queue()->num_queues()) {
    queue()->unlink_or_oops_do_impl(cl, claimed_queue);
    claimed_queue = claim();
  }
}

void StringDedupQueue::print_statistics() {
  queue()->print_statistics_impl();
}

void StringDedupQueue::verify() {
  queue()->verify_impl();
}

StringDedupQueue* const StringDedupQueue::queue() {
  assert(_queue != NULL, "Not yet initialized");
  return _queue;
}

void StringDedupQueue::gc_prologue() {
  _claimed_index = 0;
}

void StringDedupQueue::gc_epilogue() {
  assert(_claimed_index >= queue()->num_queues() || _claimed_index == 0, "All or nothing");
}
