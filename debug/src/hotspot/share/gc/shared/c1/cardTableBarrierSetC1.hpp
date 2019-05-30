#ifndef SHARE_GC_SHARED_C1_CARDTABLEBARRIERSETC1_HPP
#define SHARE_GC_SHARED_C1_CARDTABLEBARRIERSETC1_HPP

#include "gc/shared/c1/modRefBarrierSetC1.hpp"

class CardTableBarrierSetC1 : public ModRefBarrierSetC1 {
protected:
  virtual void post_barrier(LIRAccess& access, LIR_OprDesc* addr, LIR_OprDesc* new_val);
};

#endif
