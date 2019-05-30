#ifndef SHARE_VM_GC_G1_G1REMSETTRACKINGPOLICY_HPP
#define SHARE_VM_GC_G1_G1REMSETTRACKINGPOLICY_HPP

#include "gc/g1/heapRegion.hpp"
#include "gc/g1/heapRegionType.hpp"
#include "memory/allocation.hpp"

// The remembered set tracking policy determines for a given region the state of
// the remembered set, ie. when it should be tracked, and if/when the remembered
// set is complete.
class G1RemSetTrackingPolicy : public CHeapObj<mtGC> {
public:
  // Do we need to scan the given region to get all outgoing references for remembered
  // set rebuild?
  bool needs_scan_for_rebuild(HeapRegion* r) const;
  // Update remembered set tracking state at allocation of the region. May be
  // called at any time. The caller makes sure that the changes to the remembered
  // set state are visible to other threads.
  void update_at_allocate(HeapRegion* r);
  // Update remembered set tracking state for humongous regions before we are going to
  // rebuild remembered sets. Called at safepoint in the remark pause.
  bool update_humongous_before_rebuild(HeapRegion* r, bool is_live);
  // Update remembered set tracking state before we are going to rebuild remembered
  // sets. Called at safepoint in the remark pause.
  bool update_before_rebuild(HeapRegion* r, size_t live_bytes);
  // Update remembered set tracking state after rebuild is complete, i.e. the cleanup
  // pause. Called at safepoint.
  void update_after_rebuild(HeapRegion* r);
  // Update remembered set tracking state when the region is freed.
  void update_at_free(HeapRegion* r);
};

#endif
