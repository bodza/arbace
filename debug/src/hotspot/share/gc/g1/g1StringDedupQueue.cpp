#include "precompiled.hpp"

#include "classfile/javaClasses.inline.hpp"
#include "gc/g1/g1CollectedHeap.hpp"
#include "gc/g1/g1StringDedup.hpp"
#include "gc/g1/g1StringDedupQueue.hpp"
#include "logging/log.hpp"
#include "oops/oop.inline.hpp"
#include "runtime/atomic.hpp"
#include "runtime/mutexLocker.hpp"
#include "runtime/safepointVerifiers.hpp"
#include "utilities/stack.inline.hpp"

const size_t        G1StringDedupQueue::_max_size = 1000000; // Max number of elements per queue
const size_t        G1StringDedupQueue::_max_cache_size = 0; // Max cache size per queue

G1StringDedupQueue::G1StringDedupQueue() :
  _cursor(0),
  _cancel(false),
  _empty(true),
  _dropped(0) {
  _nqueues = ParallelGCThreads;
  _queues = NEW_C_HEAP_ARRAY(G1StringDedupWorkerQueue, _nqueues, mtGC);
  for (size_t i = 0; i < _nqueues; i++) {
    new (_queues + i) G1StringDedupWorkerQueue(G1StringDedupWorkerQueue::default_segment_size(), _max_cache_size, _max_size);
  }
}

G1StringDedupQueue::~G1StringDedupQueue() {
  ShouldNotReachHere();
}

void G1StringDedupQueue::wait_impl() {
  MonitorLockerEx ml(StringDedupQueue_lock, Mutex::_no_safepoint_check_flag);
  while (_empty && !_cancel) {
    ml.wait(Mutex::_no_safepoint_check_flag);
  }
}

void G1StringDedupQueue::cancel_wait_impl() {
  MonitorLockerEx ml(StringDedupQueue_lock, Mutex::_no_safepoint_check_flag);
  _cancel = true;
  ml.notify();
}

void G1StringDedupQueue::push_impl(uint worker_id, oop java_string) {

  // Push and notify waiter
  G1StringDedupWorkerQueue& worker_queue = _queues[worker_id];
  if (!worker_queue.is_full()) {
    worker_queue.push(java_string);
    if (_empty) {
      MonitorLockerEx ml(StringDedupQueue_lock, Mutex::_no_safepoint_check_flag);
      if (_empty) {
        // Mark non-empty and notify waiter
        _empty = false;
        ml.notify();
      }
    }
  } else {
    // Queue is full, drop the string and update the statistics
    Atomic::inc(&_dropped);
  }
}

oop G1StringDedupQueue::pop_impl() {
  NoSafepointVerifier nsv;

  // Try all queues before giving up
  for (size_t tries = 0; tries < _nqueues; tries++) {
    // The cursor indicates where we left of last time
    G1StringDedupWorkerQueue* queue = &_queues[_cursor];
    while (!queue->is_empty()) {
      oop obj = queue->pop();
      // The oop we pop can be NULL if it was marked
      // dead. Just ignore those and pop the next oop.
      if (obj != NULL) {
        return obj;
      }
    }

    // Try next queue
    _cursor = (_cursor + 1) % _nqueues;
  }

  // Mark empty
  _empty = true;

  return NULL;
}

void G1StringDedupQueue::unlink_or_oops_do_impl(StringDedupUnlinkOrOopsDoClosure* cl, size_t queue) {
  StackIterator<oop, mtGC> iter(_queues[queue]);
  while (!iter.is_empty()) {
    oop* p = iter.next_addr();
    if (*p != NULL) {
      if (cl->is_alive(*p)) {
        cl->keep_alive(p);
      } else {
        // Clear dead reference
        *p = NULL;
      }
    }
  }
}
