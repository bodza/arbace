#ifndef SHARE_GC_SHARED_C1_MODREFBARRIERSETC1_HPP
#define SHARE_GC_SHARED_C1_MODREFBARRIERSETC1_HPP

#include "gc/shared/c1/barrierSetC1.hpp"

// The ModRefBarrierSetC1 filters away accesses on BasicTypes other
// than T_OBJECT/T_ARRAY (oops). The oop accesses call one of the protected
// accesses, which are overridden in the concrete BarrierSetAssembler.

class ModRefBarrierSetC1 : public BarrierSetC1 {
protected:
  virtual void pre_barrier(LIRAccess& access, LIR_Opr addr_opr, LIR_Opr pre_val, CodeEmitInfo* info) { }
  virtual void post_barrier(LIRAccess& access, LIR_OprDesc* addr, LIR_OprDesc* new_val) { }

  virtual LIR_Opr resolve_address(LIRAccess& access, bool resolve_in_register);

  virtual void store_at_resolved(LIRAccess& access, LIR_Opr value);

  virtual LIR_Opr atomic_cmpxchg_at_resolved(LIRAccess& access, LIRItem& cmp_value, LIRItem& new_value);

  virtual LIR_Opr atomic_xchg_at_resolved(LIRAccess& access, LIRItem& value);
};

#endif
