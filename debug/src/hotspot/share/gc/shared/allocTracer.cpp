#include "precompiled.hpp"
#include "gc/shared/allocTracer.hpp"
// #include "jfr/jfrEvents.hpp"
#include "runtime/handles.hpp"
#include "utilities/globalDefinitions.hpp"
#include "utilities/macros.hpp"

void AllocTracer::send_allocation_outside_tlab(Klass* klass, HeapWord* obj, size_t alloc_size, Thread* thread) {
  EventObjectAllocationOutsideTLAB event;
  if (event.should_commit()) {
    event.set_objectClass(klass);
    event.set_allocationSize(alloc_size);
    event.commit();
  }
}

void AllocTracer::send_allocation_in_new_tlab(Klass* klass, HeapWord* obj, size_t tlab_size, size_t alloc_size, Thread* thread) {
  EventObjectAllocationInNewTLAB event;
  if (event.should_commit()) {
    event.set_objectClass(klass);
    event.set_allocationSize(alloc_size);
    event.set_tlabSize(tlab_size);
    event.commit();
  }
}

void AllocTracer::send_allocation_requiring_gc_event(size_t size, uint gcId) {
  EventAllocationRequiringGC event;
  if (event.should_commit()) {
    event.set_gcId(gcId);
    event.set_size(size);
    event.commit();
  }
}
