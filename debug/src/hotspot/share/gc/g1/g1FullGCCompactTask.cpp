#include "precompiled.hpp"

#include "gc/g1/g1CollectedHeap.hpp"
#include "gc/g1/g1ConcurrentMarkBitMap.inline.hpp"
#include "gc/g1/g1FullCollector.hpp"
#include "gc/g1/g1FullGCCompactionPoint.hpp"
#include "gc/g1/g1FullGCCompactTask.hpp"
#include "gc/g1/heapRegion.inline.hpp"
#include "oops/oop.inline.hpp"
#include "utilities/ticks.hpp"

class G1ResetHumongousClosure : public HeapRegionClosure {
  G1CMBitMap* _bitmap;

public:
  G1ResetHumongousClosure(G1CMBitMap* bitmap) :
      _bitmap(bitmap) { }

  bool do_heap_region(HeapRegion* current) {
    if (current->is_humongous()) {
      if (current->is_starts_humongous()) {
        oop obj = oop(current->bottom());
        if (_bitmap->is_marked(obj)) {
          // Clear bitmap and fix mark word.
          _bitmap->clear(obj);
          obj->init_mark_raw();
        }
      }
      current->reset_during_compaction();
    }
    return false;
  }
};

size_t G1FullGCCompactTask::G1CompactRegionClosure::apply(oop obj) {
  size_t size = obj->size();
  HeapWord* destination = (HeapWord*)obj->forwardee();
  if (destination == NULL) {
    // Object not moving
    return size;
  }

  // copy object and reinit its mark
  HeapWord* obj_addr = (HeapWord*) obj;
  Copy::aligned_conjoint_words(obj_addr, destination, size);
  oop(destination)->init_mark_raw();

  return size;
}

void G1FullGCCompactTask::compact_region(HeapRegion* hr) {
  G1CompactRegionClosure compact(collector()->mark_bitmap());
  hr->apply_to_marked_objects(collector()->mark_bitmap(), &compact);
  // Once all objects have been moved the liveness information
  // needs be cleared.
  collector()->mark_bitmap()->clear_region(hr);
  hr->complete_compaction();
}

void G1FullGCCompactTask::work(uint worker_id) {
  Ticks start = Ticks::now();
  GrowableArray<HeapRegion*>* compaction_queue = collector()->compaction_point(worker_id)->regions();
  for (GrowableArrayIterator<HeapRegion*> it = compaction_queue->begin(); it != compaction_queue->end(); ++it) {
    compact_region(*it);
  }

  G1ResetHumongousClosure hc(collector()->mark_bitmap());
  G1CollectedHeap::heap()->heap_region_par_iterate_from_worker_offset(&hc, &_claimer, worker_id);
  log_task("Compaction task", worker_id, start);
}

void G1FullGCCompactTask::serial_compaction() {
  GrowableArray<HeapRegion*>* compaction_queue = collector()->serial_compaction_point()->regions();
  for (GrowableArrayIterator<HeapRegion*> it = compaction_queue->begin(); it != compaction_queue->end(); ++it) {
    compact_region(*it);
  }
}
