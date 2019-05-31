#include "precompiled.hpp"
#include "gc/g1/g1SurvivorRegions.hpp"
#include "gc/g1/heapRegion.hpp"
#include "utilities/growableArray.hpp"
#include "utilities/debug.hpp"

G1SurvivorRegions::G1SurvivorRegions() : _regions(new (ResourceObj::C_HEAP, mtGC) GrowableArray<HeapRegion*>(8, true, mtGC)) { }

void G1SurvivorRegions::add(HeapRegion* hr) {
  _regions->append(hr);
}

uint G1SurvivorRegions::length() const {
  return (uint)_regions->length();
}

void G1SurvivorRegions::convert_to_eden() {
  for (GrowableArrayIterator<HeapRegion*> it = _regions->begin();
       it != _regions->end();
       ++it) {
    HeapRegion* hr = *it;
    hr->set_eden_pre_gc();
  }
  clear();
}

void G1SurvivorRegions::clear() {
  _regions->clear();
}
