#ifndef SHARE_VM_MEMORY_METASPACEGCTHRESHOLDUPDATER_HPP
#define SHARE_VM_MEMORY_METASPACEGCTHRESHOLDUPDATER_HPP

#include "memory/allocation.hpp"
#include "utilities/debug.hpp"

class MetaspaceGCThresholdUpdater : public AllStatic {
 public:
  enum Type {
    ComputeNewSize,
    ExpandAndAllocate,
    Last
  };

  static const char* to_string(MetaspaceGCThresholdUpdater::Type updater) {
    switch (updater) {
      case ComputeNewSize:
        return "compute_new_size";
      case ExpandAndAllocate:
        return "expand_and_allocate";
      default:
        ShouldNotReachHere();
        return NULL;
    };
  }
};

#endif
