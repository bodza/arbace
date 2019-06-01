#include "precompiled.hpp"

#include "gc/g1/heapRegionTracer.hpp"

void HeapRegionTracer::send_region_type_change(uint index, G1HeapRegionTraceType::Type from, G1HeapRegionTraceType::Type to, uintptr_t start, size_t used) {
  EventG1HeapRegionTypeChange e;
  if (e.should_commit()) {
    e.set_index(index);
    e.set_from(from);
    e.set_to(to);
    e.set_start(start);
    e.set_used(used);
    e.commit();
  }
}
