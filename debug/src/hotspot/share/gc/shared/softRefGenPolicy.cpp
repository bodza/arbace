#include "precompiled.hpp"
#include "gc/shared/adaptiveSizePolicy.hpp"
#include "gc/shared/genCollectedHeap.hpp"
#include "gc/shared/softRefGenPolicy.hpp"

void SoftRefGenPolicy::cleared_all_soft_refs() {
  // If near gc overhear limit, continue to clear SoftRefs.  SoftRefs may
  // have been cleared in the last collection but if the gc overhear
  // limit continues to be near, SoftRefs should still be cleared.
  AdaptiveSizePolicy* size_policy = GenCollectedHeap::heap()->size_policy();
  if (size_policy != NULL) {
    set_should_clear_all_soft_refs(size_policy->gc_overhead_limit_near());
  }

  SoftRefPolicy::cleared_all_soft_refs();
}
