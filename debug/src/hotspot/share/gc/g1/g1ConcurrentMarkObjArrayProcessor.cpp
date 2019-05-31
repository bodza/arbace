#include "precompiled.hpp"
#include "gc/g1/g1ConcurrentMark.inline.hpp"
#include "gc/g1/g1ConcurrentMarkObjArrayProcessor.inline.hpp"

void G1CMObjArrayProcessor::push_array_slice(HeapWord* what) {
  _task->push(G1TaskQueueEntry::from_slice(what));
}

size_t G1CMObjArrayProcessor::process_array_slice(objArrayOop obj, HeapWord* start_from, size_t remaining) {
  size_t words_to_scan = MIN2(remaining, (size_t)ObjArrayMarkingStride);

  if (remaining > ObjArrayMarkingStride) {
    push_array_slice(start_from + ObjArrayMarkingStride);
  }

  // Then process current area.
  MemRegion mr(start_from, words_to_scan);
  return _task->scan_objArray(obj, mr);
}

size_t G1CMObjArrayProcessor::process_obj(oop obj) {

  return process_array_slice(objArrayOop(obj), (HeapWord*)obj, (size_t)objArrayOop(obj)->size());
}

size_t G1CMObjArrayProcessor::process_slice(HeapWord* slice) {

  // Find the start address of the objArrayOop.
  // Shortcut the BOT access if the given address is from a humongous object. The BOT
  // slide is fast enough for "smaller" objects in non-humongous regions, but is slower
  // than directly using heap region table.
  G1CollectedHeap* g1h = G1CollectedHeap::heap();
  HeapRegion* r = g1h->heap_region_containing(slice);

  HeapWord* const start_address = r->is_humongous() ? r->humongous_start_region()->bottom() : g1h->block_start(slice);

  objArrayOop objArray = objArrayOop(start_address);

  size_t already_scanned = slice - start_address;
  size_t remaining = objArray->size() - already_scanned;

  return process_array_slice(objArray, slice, remaining);
}
