#ifndef SHARE_VM_GC_G1_G1HEAPSIZINGPOLICY_HPP
#define SHARE_VM_GC_G1_G1HEAPSIZINGPOLICY_HPP

#include "memory/allocation.hpp"

class G1Analytics;
class G1CollectedHeap;

class G1HeapSizingPolicy: public CHeapObj<mtGC> {
  // MinOverThresholdForGrowth must be less than the number of recorded
  // pause times in G1Analytics, representing the minimum number of pause
  // time ratios that exceed GCTimeRatio before a heap expansion will be triggered.
  const static uint MinOverThresholdForGrowth = 4;

  const G1CollectedHeap* _g1h;
  const G1Analytics* _analytics;

  const uint _num_prev_pauses_for_heuristics;
  // Ratio check data for determining if heap growth is necessary.
  uint _ratio_over_threshold_count;
  double _ratio_over_threshold_sum;
  uint _pauses_since_start;

protected:
  G1HeapSizingPolicy(const G1CollectedHeap* g1h, const G1Analytics* analytics);
public:

  // If an expansion would be appropriate, because recent GC overhead had
  // exceeded the desired limit, return an amount to expand by.
  virtual size_t expansion_amount();

  // Clear ratio tracking data used by expansion_amount().
  void clear_ratio_check_data();

  static G1HeapSizingPolicy* create(const G1CollectedHeap* g1h, const G1Analytics* analytics);
};

#endif
