#ifndef SHARE_GC_G1_G1FULLGCCOMPACTTASK_HPP
#define SHARE_GC_G1_G1FULLGCCOMPACTTASK_HPP

#include "gc/g1/g1FullGCCompactionPoint.hpp"
#include "gc/g1/g1FullGCScope.hpp"
#include "gc/g1/g1FullGCTask.hpp"
#include "gc/g1/g1StringDedup.hpp"
#include "gc/g1/heapRegionManager.hpp"
#include "gc/shared/referenceProcessor.hpp"

class G1CollectedHeap;
class G1CMBitMap;

class G1FullGCCompactTask : public G1FullGCTask {
protected:
  HeapRegionClaimer _claimer;

private:
  void compact_region(HeapRegion* hr);

public:
  G1FullGCCompactTask(G1FullCollector* collector) :
    G1FullGCTask("G1 Compact Task", collector),
    _claimer(collector->workers()) { }
  void work(uint worker_id);
  void serial_compaction();

  class G1CompactRegionClosure : public StackObj {
    G1CMBitMap* _bitmap;

  public:
    G1CompactRegionClosure(G1CMBitMap* bitmap) : _bitmap(bitmap) { }
    size_t apply(oop object);
  };
};

#endif
