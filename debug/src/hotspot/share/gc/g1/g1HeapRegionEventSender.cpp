#include "precompiled.hpp"

#include "gc/g1/g1CollectedHeap.hpp"
#include "gc/g1/heapRegion.hpp"
#include "g1HeapRegionEventSender.hpp"

class DumpEventInfoClosure : public HeapRegionClosure {
public:
  bool do_heap_region(HeapRegion* r) {
    EventG1HeapRegionInformation evt;
    evt.set_index(r->hrm_index());
    evt.set_type(r->get_trace_type());
    evt.set_start((uintptr_t)r->bottom());
    evt.set_used(r->used());
    evt.commit();
    return false;
  }
};

void G1HeapRegionEventSender::send_events() {
  DumpEventInfoClosure c;

  G1CollectedHeap::heap()->heap_region_iterate(&c);
}
