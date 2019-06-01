#ifndef SHARE_VM_GC_G1_G1STRINGDEDUPQUEUE_HPP
#define SHARE_VM_GC_G1_G1STRINGDEDUPQUEUE_HPP

#include "gc/shared/stringdedup/stringDedupQueue.hpp"
#include "memory/allocation.hpp"
#include "oops/oop.hpp"
#include "utilities/stack.hpp"

class StringDedupUnlinkOrOopsDoClosure;

//
// G1 enqueues candidates during the stop-the-world mark/evacuation phase.
//

class G1StringDedupQueue : public StringDedupQueue {
private:
  typedef Stack<oop, mtGC> G1StringDedupWorkerQueue;

  static const size_t        _max_size;
  static const size_t        _max_cache_size;

  G1StringDedupWorkerQueue*  _queues;
  size_t                     _nqueues;
  size_t                     _cursor;
  bool                       _cancel;
  volatile bool              _empty;

  // Statistics counter, only used for logging.
  uintx                      _dropped;

  ~G1StringDedupQueue();

  void unlink_or_oops_do(StringDedupUnlinkOrOopsDoClosure* cl, size_t queue);

public:
  G1StringDedupQueue();

protected:

  // Blocks and waits for the queue to become non-empty.
  void wait_impl();

  // Wakes up any thread blocked waiting for the queue to become non-empty.
  void cancel_wait_impl();

  // Pushes a deduplication candidate onto a specific GC worker queue.
  void push_impl(uint worker_id, oop java_string);

  // Pops a deduplication candidate from any queue, returns NULL if
  // all queues are empty.
  oop pop_impl();

  size_t num_queues() const {
    return _nqueues;
  }

  void unlink_or_oops_do_impl(StringDedupUnlinkOrOopsDoClosure* cl, size_t queue);
};

#endif
