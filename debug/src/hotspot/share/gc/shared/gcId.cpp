#include "precompiled.hpp"

#include "jvm.h"
#include "gc/shared/gcId.hpp"
#include "runtime/safepoint.hpp"
#include "runtime/thread.inline.hpp"

uint GCId::_next_id = 0;

NamedThread* currentNamedthread() {
  return (NamedThread*)Thread::current();
}

uint GCId::create() {
  return _next_id++;
}

uint GCId::peek() {
  return _next_id;
}

uint GCId::current() {
  const uint gc_id = currentNamedthread()->gc_id();
  return gc_id;
}

uint GCId::current_or_undefined() {
  return Thread::current()->is_Named_thread() ? currentNamedthread()->gc_id() : undefined();
}

size_t GCId::print_prefix(char* buf, size_t len) {
  Thread* thread = Thread::current_or_null();
  if (thread != NULL) {
    uint gc_id = current_or_undefined();
    if (gc_id != undefined()) {
      int ret = jio_snprintf(buf, len, "GC(%u) ", gc_id);
      return (size_t)ret;
    }
  }
  return 0;
}

GCIdMark::GCIdMark() : _previous_gc_id(currentNamedthread()->gc_id()) {
  currentNamedthread()->set_gc_id(GCId::create());
}

GCIdMark::GCIdMark(uint gc_id) : _previous_gc_id(currentNamedthread()->gc_id()) {
  currentNamedthread()->set_gc_id(gc_id);
}

GCIdMark::~GCIdMark() {
  currentNamedthread()->set_gc_id(_previous_gc_id);
}
