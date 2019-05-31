#ifndef SHARE_VM_GC_SHARED_GCPOLICYCOUNTERS_HPP
#define SHARE_VM_GC_SHARED_GCPOLICYCOUNTERS_HPP

#include "runtime/perfData.hpp"

// GCPolicyCounters is a holder class for performance counters
// that track a generation

class GCPolicyCounters: public CHeapObj<mtGC> {
  friend class VMStructs;

  private:

    // Constant PerfData types don't need to retain a reference.
    // However, it's a good idea to document them here.
    // PerfStringConstant*     _name;
    // PerfStringConstant*     _collector_size;
    // PerfStringConstant*     _generation_size;

    PerfVariable*     _tenuring_threshold;
    PerfVariable*     _desired_survivor_size;

    const char* _name_space;

 public:

  enum Name {
    NONE,
    GCPolicyCountersKind,
    GCAdaptivePolicyCountersKind,
    PSGCAdaptivePolicyCountersKind,
    CMSGCAdaptivePolicyCountersKind
  };

  GCPolicyCounters(const char* name, int collectors, int generations);

    inline PerfVariable* tenuring_threshold() const  {
      return _tenuring_threshold;
    }

    inline PerfVariable* desired_survivor_size() const  {
      return _desired_survivor_size;
    }

    const char* name_space() const { return _name_space; }

    virtual void update_counters() { }

    virtual GCPolicyCounters::Name kind() const {
      return GCPolicyCounters::GCPolicyCountersKind;
    }
};

#endif
