#ifndef SHARE_VM_GC_SHARED_STRINGDEDUP_STRINGDEDUPTHREAD_INLINE_HPP
#define SHARE_VM_GC_SHARED_STRINGDEDUP_STRINGDEDUPTHREAD_INLINE_HPP

#include "gc/shared/suspendibleThreadSet.hpp"
#include "gc/shared/stringdedup/stringDedupQueue.inline.hpp"
#include "gc/shared/stringdedup/stringDedupThread.hpp"

template <typename S>
void StringDedupThreadImpl<S>::do_deduplication() {
  S total_stat;

  deduplicate_shared_strings(&total_stat);

  // Main loop
  for (;;) {
    S stat;

    stat.mark_idle();

    // Wait for the queue to become non-empty
    StringDedupQueue::wait();
    if (this->should_terminate()) {
      break;
    }

    {
      // Include thread in safepoints
      SuspendibleThreadSetJoiner sts_join;

      stat.mark_exec();
      StringDedupStat::print_start(&stat);

      // Process the queue
      for (;;) {
        oop java_string = StringDedupQueue::pop();
        if (java_string == NULL) {
          break;
        }

        StringDedupTable::deduplicate(java_string, &stat);

        // Safepoint this thread if needed
        if (sts_join.should_yield()) {
          stat.mark_block();
          sts_join.yield();
          stat.mark_unblock();
        }
      }

      stat.mark_done();

      total_stat.add(&stat);
      print_end(&stat, &total_stat);
      stat.reset();
    }

    StringDedupTable::clean_entry_cache();
  }
}

template <typename S>
void StringDedupThreadImpl<S>::create() {
  assert(_thread == NULL, "One string deduplication thread allowed");
  _thread = new StringDedupThreadImpl<S>();
}

#endif
