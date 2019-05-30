#ifndef SHARE_VM_GC_SHARED_AGETABLETRACER_HPP
#define SHARE_VM_GC_SHARED_AGETABLETRACER_HPP

#include "memory/allocation.hpp"

class AgeTableTracer : AllStatic {
  public:
    static void send_tenuring_distribution_event(uint age, size_t size);
    static bool is_tenuring_distribution_event_enabled();
};

#endif
