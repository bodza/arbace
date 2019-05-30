#ifndef SHARE_VM_GC_EPSILON_EPSILONMONITORINGSUPPORT_HPP
#define SHARE_VM_GC_EPSILON_EPSILONMONITORINGSUPPORT_HPP

#include "memory/allocation.hpp"

class GenerationCounters;
class EpsilonSpaceCounters;
class EpsilonHeap;

class EpsilonMonitoringSupport : public CHeapObj<mtGC> {
private:
  GenerationCounters*   _heap_counters;
  EpsilonSpaceCounters* _space_counters;

public:
  EpsilonMonitoringSupport(EpsilonHeap* heap);
  void update_counters();
};

#endif
