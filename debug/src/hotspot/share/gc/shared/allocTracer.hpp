#ifndef SHARE_VM_GC_SHARED_ALLOCTRACER_HPP
#define SHARE_VM_GC_SHARED_ALLOCTRACER_HPP

#include "memory/allocation.hpp"
#include "runtime/handles.hpp"

class AllocTracer : AllStatic {
  public:
    static void send_allocation_outside_tlab(Klass* klass, HeapWord* obj, size_t alloc_size, Thread* thread);
    static void send_allocation_in_new_tlab(Klass* klass, HeapWord* obj, size_t tlab_size, size_t alloc_size, Thread* thread);
    static void send_allocation_requiring_gc_event(size_t size, uint gcId);
};

#endif
