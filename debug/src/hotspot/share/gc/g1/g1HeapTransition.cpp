#include "precompiled.hpp"

#include "gc/g1/g1CollectedHeap.hpp"
#include "gc/g1/g1HeapTransition.hpp"
#include "gc/g1/g1Policy.hpp"
#include "logging/log.hpp"
#include "memory/metaspace.hpp"

G1HeapTransition::Data::Data(G1CollectedHeap* g1_heap) {
  _eden_length = g1_heap->eden_regions_count();
  _survivor_length = g1_heap->survivor_regions_count();
  _old_length = g1_heap->old_regions_count();
  _humongous_length = g1_heap->humongous_regions_count();
  _metaspace_used_bytes = MetaspaceUtils::used_bytes();
}

G1HeapTransition::G1HeapTransition(G1CollectedHeap* g1_heap) : _g1_heap(g1_heap), _before(g1_heap) { }

struct DetailedUsage : public StackObj {
  size_t _eden_used;
  size_t _survivor_used;
  size_t _old_used;
  size_t _humongous_used;

  size_t _eden_region_count;
  size_t _survivor_region_count;
  size_t _old_region_count;
  size_t _humongous_region_count;

  DetailedUsage() :
    _eden_used(0), _survivor_used(0), _old_used(0), _humongous_used(0),
    _eden_region_count(0), _survivor_region_count(0), _old_region_count(0), _humongous_region_count(0) { }
};

class DetailedUsageClosure: public HeapRegionClosure {
public:
  DetailedUsage _usage;
  bool do_heap_region(HeapRegion* r) {
    if (r->is_old()) {
      _usage._old_used += r->used();
      _usage._old_region_count++;
    } else if (r->is_survivor()) {
      _usage._survivor_used += r->used();
      _usage._survivor_region_count++;
    } else if (r->is_eden()) {
      _usage._eden_used += r->used();
      _usage._eden_region_count++;
    } else if (r->is_humongous()) {
      _usage._humongous_used += r->used();
      _usage._humongous_region_count++;
    }
    return false;
  }
};

void G1HeapTransition::print() {
  MetaspaceUtils::print_metaspace_change(_before._metaspace_used_bytes);
}
