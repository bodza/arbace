#include "precompiled.hpp"

#include "classfile/javaClasses.inline.hpp"
#include "gc/g1/g1CollectedHeap.inline.hpp"
#include "gc/g1/g1GCPhaseTimes.hpp"
#include "gc/g1/g1StringDedup.hpp"
#include "gc/g1/g1StringDedupQueue.hpp"
#include "gc/g1/g1StringDedupStat.hpp"
#include "gc/shared/stringdedup/stringDedup.inline.hpp"
#include "gc/shared/stringdedup/stringDedupQueue.hpp"
#include "gc/shared/stringdedup/stringDedupTable.hpp"
#include "gc/shared/stringdedup/stringDedupThread.inline.hpp"
#include "oops/oop.inline.hpp"
#include "runtime/atomic.hpp"

void G1StringDedup::initialize() {
  StringDedup::initialize_impl<G1StringDedupQueue, G1StringDedupStat>();
}

bool G1StringDedup::is_candidate_from_mark(oop obj) {
  if (java_lang_String::is_instance_inlined(obj)) {
    bool from_young = G1CollectedHeap::heap()->heap_region_containing(obj)->is_young();
    if (from_young && obj->age() < StringDeduplicationAgeThreshold) {
      // Candidate found. String is being evacuated from young to old but has not
      // reached the deduplication age threshold, i.e. has not previously been a
      // candidate during its life in the young generation.
      return true;
    }
  }

  // Not a candidate
  return false;
}

void G1StringDedup::enqueue_from_mark(oop java_string, uint worker_id) {
  if (is_candidate_from_mark(java_string)) {
    G1StringDedupQueue::push(worker_id, java_string);
  }
}

bool G1StringDedup::is_candidate_from_evacuation(bool from_young, bool to_young, oop obj) {
  if (from_young && java_lang_String::is_instance_inlined(obj)) {
    if (to_young && obj->age() == StringDeduplicationAgeThreshold) {
      // Candidate found. String is being evacuated from young to young and just
      // reached the deduplication age threshold.
      return true;
    }
    if (!to_young && obj->age() < StringDeduplicationAgeThreshold) {
      // Candidate found. String is being evacuated from young to old but has not
      // reached the deduplication age threshold, i.e. has not previously been a
      // candidate during its life in the young generation.
      return true;
    }
  }

  // Not a candidate
  return false;
}

void G1StringDedup::enqueue_from_evacuation(bool from_young, bool to_young, uint worker_id, oop java_string) {
  if (is_candidate_from_evacuation(from_young, to_young, java_string)) {
    G1StringDedupQueue::push(worker_id, java_string);
  }
}

void G1StringDedup::oops_do(OopClosure* keep_alive) {
  unlink_or_oops_do(NULL, keep_alive, true /* allow_resize_and_rehash */);
}

void G1StringDedup::parallel_unlink(G1StringDedupUnlinkOrOopsDoClosure* unlink, uint worker_id) {
  StringDedupQueue::unlink_or_oops_do(unlink);
  StringDedupTable::unlink_or_oops_do(unlink, worker_id);
}

//
// Task for parallel unlink_or_oops_do() operation on the deduplication queue
// and table.
//
class G1StringDedupUnlinkOrOopsDoTask : public AbstractGangTask {
private:
  G1StringDedupUnlinkOrOopsDoClosure _cl;
  G1GCPhaseTimes* _phase_times;

public:
  G1StringDedupUnlinkOrOopsDoTask(BoolObjectClosure* is_alive, OopClosure* keep_alive, bool allow_resize_and_rehash, G1GCPhaseTimes* phase_times) :
    AbstractGangTask("G1StringDedupUnlinkOrOopsDoTask"),
    _cl(is_alive, keep_alive, allow_resize_and_rehash), _phase_times(phase_times) { }

  virtual void work(uint worker_id) {
    {
      G1GCParPhaseTimesTracker x(_phase_times, G1GCPhaseTimes::StringDedupQueueFixup, worker_id);
      StringDedupQueue::unlink_or_oops_do(&_cl);
    }
    {
      G1GCParPhaseTimesTracker x(_phase_times, G1GCPhaseTimes::StringDedupTableFixup, worker_id);
      StringDedupTable::unlink_or_oops_do(&_cl, worker_id);
    }
  }
};

void G1StringDedup::unlink_or_oops_do(BoolObjectClosure* is_alive, OopClosure* keep_alive, bool allow_resize_and_rehash, G1GCPhaseTimes* phase_times) {
  G1StringDedupUnlinkOrOopsDoTask task(is_alive, keep_alive, allow_resize_and_rehash, phase_times);
  G1CollectedHeap* g1h = G1CollectedHeap::heap();
  g1h->workers()->run_task(&task);
}
