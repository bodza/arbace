#ifndef SHARE_VM_GC_SHARED_GCSTATS_HPP
#define SHARE_VM_GC_SHARED_GCSTATS_HPP

#include "gc/shared/gcUtil.hpp"

class GCStats : public CHeapObj<mtGC> {
 protected:
  // Avg amount promoted; used for avoiding promotion undo
  // This class does not update deviations if the sample is zero.
  AdaptivePaddedNoZeroDevAverage* _avg_promoted;

 public:
  GCStats();

  enum Name {
    GCStatsKind,
    CMSGCStatsKind
  };

  virtual Name kind() {
    return GCStatsKind;
  }

  AdaptivePaddedNoZeroDevAverage* avg_promoted() const { return _avg_promoted; }

  // Average in bytes
  size_t average_promoted_in_bytes() const {
    return (size_t)_avg_promoted->average();
  }

  // Padded average in bytes
  size_t padded_average_promoted_in_bytes() const {
    return (size_t)_avg_promoted->padded_average();
  }
};

#endif
