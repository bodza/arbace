#ifndef SHARE_VM_GC_SHARED_REFERENCEPROCESSORSTATS_HPP
#define SHARE_VM_GC_SHARED_REFERENCEPROCESSORSTATS_HPP

#include "utilities/globalDefinitions.hpp"

class ReferenceProcessor;

// ReferenceProcessorStats contains statistics about how many references that
// have been traversed when processing references during garbage collection.
class ReferenceProcessorStats {
  size_t _soft_count;
  size_t _weak_count;
  size_t _final_count;
  size_t _phantom_count;

 public:
  ReferenceProcessorStats() :
    _soft_count(0),
    _weak_count(0),
    _final_count(0),
    _phantom_count(0) { }

  ReferenceProcessorStats(size_t soft_count,
                          size_t weak_count,
                          size_t final_count,
                          size_t phantom_count) :
    _soft_count(soft_count),
    _weak_count(weak_count),
    _final_count(final_count),
    _phantom_count(phantom_count)
  { }

  size_t soft_count() const {
    return _soft_count;
  }

  size_t weak_count() const {
    return _weak_count;
  }

  size_t final_count() const {
    return _final_count;
  }

  size_t phantom_count() const {
    return _phantom_count;
  }
};
#endif
