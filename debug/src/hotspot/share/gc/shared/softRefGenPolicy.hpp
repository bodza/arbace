#ifndef SHARE_VM_GC_SHARED_SOFTREFGENPOLICY_HPP
#define SHARE_VM_GC_SHARED_SOFTREFGENPOLICY_HPP

#include "gc/shared/softRefPolicy.hpp"
#include "utilities/globalDefinitions.hpp"

class AdaptiveSizePolicy;

class SoftRefGenPolicy : public SoftRefPolicy {
public:
  virtual void cleared_all_soft_refs();
};

#endif
