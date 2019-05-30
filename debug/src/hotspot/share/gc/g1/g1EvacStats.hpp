#ifndef SHARE_VM_GC_G1_G1EVACSTATS_HPP
#define SHARE_VM_GC_G1_G1EVACSTATS_HPP

#include "gc/shared/plab.hpp"

// Records various memory allocation statistics gathered during evacuation.
class G1EvacStats : public PLABStats {
 private:
  size_t _region_end_waste; // Number of words wasted due to skipping to the next region.
  uint   _regions_filled;   // Number of regions filled completely.
  size_t _direct_allocated; // Number of words allocated directly into the regions.

  // Number of words in live objects remaining in regions that ultimately suffered an
  // evacuation failure. This is used in the regions when the regions are made old regions.
  size_t _failure_used;
  // Number of words wasted in regions which failed evacuation. This is the sum of space
  // for objects successfully copied out of the regions (now dead space) plus waste at the
  // end of regions.
  size_t _failure_waste;

  virtual void reset() {
    PLABStats::reset();
    _region_end_waste = 0;
    _regions_filled = 0;
    _direct_allocated = 0;
    _failure_used = 0;
    _failure_waste = 0;
  }

  virtual void log_plab_allocation();

  virtual size_t compute_desired_plab_sz();

 public:
  G1EvacStats(const char* description, size_t desired_plab_sz_, unsigned wt);

  ~G1EvacStats();

  uint regions_filled() const { return _regions_filled; }
  size_t region_end_waste() const { return _region_end_waste; }
  size_t direct_allocated() const { return _direct_allocated; }

  // Amount of space in heapwords used in the failing regions when an evacuation failure happens.
  size_t failure_used() const { return _failure_used; }
  // Amount of space in heapwords wasted (unused) in the failing regions when an evacuation failure happens.
  size_t failure_waste() const { return _failure_waste; }

  inline void add_direct_allocated(size_t value);
  inline void add_region_end_waste(size_t value);
  inline void add_failure_used_and_waste(size_t used, size_t waste);
};

#endif
