#ifndef SHARE_GC_G1_G1FULLGCMARKTASK_HPP
#define SHARE_GC_G1_G1FULLGCMARKTASK_HPP

#include "gc/g1/g1FullGCCompactionPoint.hpp"
#include "gc/g1/g1FullGCScope.hpp"
#include "gc/g1/g1FullGCTask.hpp"
#include "gc/g1/g1RootProcessor.hpp"
#include "gc/g1/g1StringDedup.hpp"
#include "gc/g1/heapRegionManager.hpp"
#include "gc/shared/referenceProcessor.hpp"
#include "utilities/ticks.hpp"

class G1FullGCMarkTask : public G1FullGCTask {
  G1RootProcessor          _root_processor;
  ParallelTaskTerminator   _terminator;

public:
  G1FullGCMarkTask(G1FullCollector* collector);
  void work(uint worker_id);
};

#endif
