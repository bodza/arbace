#include "precompiled.hpp"

#include "memory/allocation.inline.hpp"
#include "oops/oop.inline.hpp"
#include "runtime/monitorChunk.hpp"

MonitorChunk::MonitorChunk(int number_on_monitors) {
  _number_of_monitors = number_on_monitors;
  _monitors           = NEW_C_HEAP_ARRAY(BasicObjectLock, number_on_monitors, mtInternal);
  _next               = NULL;
}

MonitorChunk::~MonitorChunk() {
  FreeHeap(monitors());
}

void MonitorChunk::oops_do(OopClosure* f) {
  for (int index = 0; index < number_of_monitors(); index++) {
    at(index)->oops_do(f);
  }
}
