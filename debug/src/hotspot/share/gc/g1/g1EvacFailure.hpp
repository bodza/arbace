#ifndef SHARE_VM_GC_G1_G1EVACFAILURE_HPP
#define SHARE_VM_GC_G1_G1EVACFAILURE_HPP

#include "gc/g1/g1OopClosures.hpp"
#include "gc/g1/heapRegionManager.hpp"
#include "gc/shared/workgroup.hpp"
#include "utilities/globalDefinitions.hpp"

class G1CollectedHeap;

// Task to fixup self-forwarding pointers
// installed as a result of an evacuation failure.
class G1ParRemoveSelfForwardPtrsTask: public AbstractGangTask {
protected:
  G1CollectedHeap* _g1h;
  HeapRegionClaimer _hrclaimer;

public:
  G1ParRemoveSelfForwardPtrsTask();

  void work(uint worker_id);
};

#endif
