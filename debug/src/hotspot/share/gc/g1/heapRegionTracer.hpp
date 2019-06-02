#ifndef SHARE_VM_GC_G1_HEAPREGIONTRACER_HPP
#define SHARE_VM_GC_G1_HEAPREGIONTRACER_HPP

#include "gc/g1/g1HeapRegionTraceType.hpp"
#include "memory/allocation.hpp"

class HeapRegionTracer : AllStatic {
  public:
    static void send_region_type_change(uint index, G1HeapRegionTraceType::Type from, G1HeapRegionTraceType::Type to, uintptr_t start, size_t used);
};

#endif
