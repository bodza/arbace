#ifndef SHARE_VM_GC_G1_G1COLLECTORPOLICY_HPP
#define SHARE_VM_GC_G1_G1COLLECTORPOLICY_HPP

#include "gc/shared/collectorPolicy.hpp"

// G1CollectorPolicy is primarily used during initialization and to expose the
// functionality of the CollectorPolicy interface to the rest of the VM.

class G1YoungGenSizer;

class G1CollectorPolicy: public CollectorPolicy {
protected:
  void initialize_alignments();

public:
  G1CollectorPolicy();
};

#endif
