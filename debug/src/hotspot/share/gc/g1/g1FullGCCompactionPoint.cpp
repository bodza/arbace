#include "precompiled.hpp"

#include "gc/g1/g1FullGCCompactionPoint.hpp"
#include "gc/g1/heapRegion.hpp"
#include "oops/oop.inline.hpp"
#include "utilities/debug.hpp"

G1FullGCCompactionPoint::G1FullGCCompactionPoint() :
    _current_region(NULL),
    _threshold(NULL),
    _compaction_top(NULL) {
  _compaction_regions = new (ResourceObj::C_HEAP, mtGC) GrowableArray<HeapRegion*>(32, true, mtGC);
  _compaction_region_iterator = _compaction_regions->begin();
}

G1FullGCCompactionPoint::~G1FullGCCompactionPoint() {
  delete _compaction_regions;
}

void G1FullGCCompactionPoint::update() {
  if (is_initialized()) {
    _current_region->set_compaction_top(_compaction_top);
  }
}

void G1FullGCCompactionPoint::initialize_values(bool init_threshold) {
  _compaction_top = _current_region->compaction_top();
  if (init_threshold) {
    _threshold = _current_region->initialize_threshold();
  }
}

bool G1FullGCCompactionPoint::has_regions() {
  return !_compaction_regions->is_empty();
}

bool G1FullGCCompactionPoint::is_initialized() {
  return _current_region != NULL;
}

void G1FullGCCompactionPoint::initialize(HeapRegion* hr, bool init_threshold) {
  _current_region = hr;
  initialize_values(init_threshold);
}

HeapRegion* G1FullGCCompactionPoint::current_region() {
  return *_compaction_region_iterator;
}

HeapRegion* G1FullGCCompactionPoint::next_region() {
  HeapRegion* next = *(++_compaction_region_iterator);
  return next;
}

GrowableArray<HeapRegion*>* G1FullGCCompactionPoint::regions() {
  return _compaction_regions;
}

bool G1FullGCCompactionPoint::object_will_fit(size_t size) {
  size_t space_left = pointer_delta(_current_region->end(), _compaction_top);
  return size <= space_left;
}

void G1FullGCCompactionPoint::switch_region() {
  // Save compaction top in the region.
  _current_region->set_compaction_top(_compaction_top);
  // Get the next region and re-initialize the values.
  _current_region = next_region();
  initialize_values(true);
}

void G1FullGCCompactionPoint::forward(oop object, size_t size) {

  // Ensure the object fit in the current region.
  while (!object_will_fit(size)) {
    switch_region();
  }

  // Store a forwarding pointer if the object should be moved.
  if ((HeapWord*)object != _compaction_top) {
    object->forward_to(oop(_compaction_top));
  } else {
    if (object->forwardee() != NULL) {
      // Object should not move but mark-word is used so it looks like the
      // object is forwarded. Need to clear the mark and it's no problem
      // since it will be restored by preserved marks. There is an exception
      // with BiasedLocking, in this case forwardee() will return NULL
      // even if the mark-word is used. This is no problem since
      // forwardee() will return NULL in the compaction phase as well.
      object->init_mark_raw();
    } else {
      // Make sure object has the correct mark-word set or that it will be
      // fixed when restoring the preserved marks.
      // Correct mark // Will be restored by PreservedMarksSet // Will be restored by BiasedLocking
    }
  }

  // Update compaction values.
  _compaction_top += size;
  if (_compaction_top > _threshold) {
    _threshold = _current_region->cross_threshold(_compaction_top - size, _compaction_top);
  }
}

void G1FullGCCompactionPoint::add(HeapRegion* hr) {
  _compaction_regions->append(hr);
}

void G1FullGCCompactionPoint::merge(G1FullGCCompactionPoint* other) {
   _compaction_regions->appendAll(other->regions());
}

HeapRegion* G1FullGCCompactionPoint::remove_last() {
  return _compaction_regions->pop();
}
