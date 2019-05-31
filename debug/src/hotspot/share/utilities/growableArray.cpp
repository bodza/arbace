#include "precompiled.hpp"
#include "memory/allocation.inline.hpp"
#include "memory/resourceArea.hpp"
#include "runtime/thread.inline.hpp"
#include "utilities/growableArray.hpp"

void* GenericGrowableArray::raw_allocate(int elementSize) {
  size_t byte_size = elementSize * (size_t) _max;
  if (on_stack()) {
    return (void*)resource_allocate_bytes(byte_size);
  } else if (on_C_heap()) {
    return (void*)AllocateHeap(byte_size, _memflags);
  } else {
    return _arena->Amalloc(byte_size);
  }
}

void GenericGrowableArray::free_C_heap(void* elements) {
  FreeHeap(elements);
}
