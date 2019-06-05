#include "precompiled.hpp"

#include "gc/g1/g1CollectedHeap.hpp"
#include "gc/g1/heapRegion.hpp"
#include "g1HeapRegionEventSender.hpp"

class DumpEventInfoClosure : public HeapRegionClosure {
public:
  bool do_heap_region(HeapRegion* r) {
    return false;
  }
};

void G1HeapRegionEventSender::send_events() {
  DumpEventInfoClosure c;

  G1CollectedHeap::heap()->heap_region_iterate(&c);
}
