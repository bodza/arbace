#ifndef SHARE_VM_GC_G1_G1HEAPREGIONEVENTSENDER_HPP
#define SHARE_VM_GC_G1_G1HEAPREGIONEVENTSENDER_HPP

#include "memory/allocation.hpp"

class G1HeapRegionEventSender : public AllStatic {
public:
  static void send_events();
};

#endif
