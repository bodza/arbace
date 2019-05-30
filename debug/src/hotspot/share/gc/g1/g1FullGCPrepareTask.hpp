#ifndef SHARE_GC_G1_G1FULLGCPREPARETASK_HPP
#define SHARE_GC_G1_G1FULLGCPREPARETASK_HPP

#include "gc/g1/g1FullGCCompactionPoint.hpp"
#include "gc/g1/g1FullGCScope.hpp"
#include "gc/g1/g1FullGCTask.hpp"
#include "gc/g1/g1RootProcessor.hpp"
#include "gc/g1/g1StringDedup.hpp"
#include "gc/g1/heapRegionManager.hpp"
#include "gc/shared/referenceProcessor.hpp"

class G1CMBitMap;

class G1FullGCPrepareTask : public G1FullGCTask {
protected:
  volatile bool     _freed_regions;
  HeapRegionClaimer _hrclaimer;

  void set_freed_regions();

public:
  G1FullGCPrepareTask(G1FullCollector* collector);
  void work(uint worker_id);
  void prepare_serial_compaction();
  bool has_freed_regions();

protected:
  class G1CalculatePointersClosure : public HeapRegionClosure {
  protected:
    G1CollectedHeap* _g1h;
    G1CMBitMap* _bitmap;
    G1FullGCCompactionPoint* _cp;
    uint _humongous_regions_removed;

    virtual void prepare_for_compaction(HeapRegion* hr);
    void prepare_for_compaction_work(G1FullGCCompactionPoint* cp, HeapRegion* hr);
    void free_humongous_region(HeapRegion* hr);
    void reset_region_metadata(HeapRegion* hr);

  public:
    G1CalculatePointersClosure(G1CMBitMap* bitmap,
                               G1FullGCCompactionPoint* cp);

    void update_sets();
    bool do_heap_region(HeapRegion* hr);
    bool freed_regions();
  };

  class G1PrepareCompactLiveClosure : public StackObj {
    G1FullGCCompactionPoint* _cp;

  public:
    G1PrepareCompactLiveClosure(G1FullGCCompactionPoint* cp);
    size_t apply(oop object);
  };

  class G1RePrepareClosure : public StackObj {
    G1FullGCCompactionPoint* _cp;
    HeapRegion* _current;

  public:
    G1RePrepareClosure(G1FullGCCompactionPoint* hrcp,
                       HeapRegion* hr) :
        _cp(hrcp),
        _current(hr) { }

    size_t apply(oop object);
  };
};

#endif
