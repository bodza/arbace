#ifndef SHARE_GC_G1_G1FULLGCADJUSTTASK_HPP
#define SHARE_GC_G1_G1FULLGCADJUSTTASK_HPP

#include "gc/g1/g1FullGCOopClosures.hpp"
#include "gc/g1/g1FullGCTask.hpp"
#include "gc/g1/g1RootProcessor.hpp"
#include "gc/g1/g1StringDedup.hpp"
#include "gc/g1/heapRegionManager.hpp"
#include "utilities/ticks.hpp"

class G1CollectedHeap;

class G1FullGCAdjustTask : public G1FullGCTask {
  G1RootProcessor          _root_processor;
  HeapRegionClaimer        _hrclaimer;
  G1AdjustClosure          _adjust;
  G1StringDedupUnlinkOrOopsDoClosure _adjust_string_dedup;

public:
  G1FullGCAdjustTask(G1FullCollector* collector);
  void work(uint worker_id);
};

#endif
