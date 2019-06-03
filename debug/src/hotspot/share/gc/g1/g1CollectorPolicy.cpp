#include "precompiled.hpp"

#include "gc/g1/g1Analytics.hpp"
#include "gc/g1/g1CollectorPolicy.hpp"
#include "gc/g1/g1YoungGenSizer.hpp"
#include "gc/g1/heapRegion.hpp"
#include "gc/g1/heapRegionRemSet.hpp"
#include "gc/shared/gcPolicyCounters.hpp"
#include "runtime/globals.hpp"
#include "utilities/debug.hpp"

G1CollectorPolicy::G1CollectorPolicy() {
  // Set up the region size and associated fields. Given that the
  // policy is created before the heap, we have to set this up here,
  // so it's done as soon as possible.

  // It would have been natural to pass initial_heap_byte_size() and
  // max_heap_byte_size() to setup_heap_region_size() but those have
  // not been set up at this point since they should be aligned with
  // the region size. So, there is a circular dependency here. We base
  // the region size on the heap size, but the heap size should be
  // aligned with the region size. To get around this we use the
  // unaligned values for the heap.
  HeapRegion::setup_heap_region_size(InitialHeapSize, MaxHeapSize);
  HeapRegionRemSet::setup_remset_size();
}

void G1CollectorPolicy::initialize_alignments() {
  _space_alignment = HeapRegion::GrainBytes;
  size_t card_table_alignment = CardTableRS::ct_max_alignment_constraint();
  size_t page_size = UseLargePages ? os::large_page_size() : os::vm_page_size();
  _heap_alignment = MAX3(card_table_alignment, _space_alignment, page_size);
}
