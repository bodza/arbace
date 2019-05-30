#ifndef SHARE_VM_GC_EPSILON_COLLECTORPOLICY_HPP
#define SHARE_VM_GC_EPSILON_COLLECTORPOLICY_HPP

#include "gc/shared/collectorPolicy.hpp"

class EpsilonCollectorPolicy: public CollectorPolicy {
protected:
  virtual void initialize_alignments() {
    size_t page_size = UseLargePages ? os::large_page_size() : os::vm_page_size();
    size_t align = MAX2((size_t)os::vm_allocation_granularity(), page_size);
    _space_alignment = align;
    _heap_alignment  = align;
  }

public:
  EpsilonCollectorPolicy() : CollectorPolicy() {};
};

#endif
