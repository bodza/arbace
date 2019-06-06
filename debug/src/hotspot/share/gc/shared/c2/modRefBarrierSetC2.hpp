#ifndef SHARE_GC_SHARED_C2_MODREFBARRIERSETC2_HPP
#define SHARE_GC_SHARED_C2_MODREFBARRIERSETC2_HPP

#include "gc/shared/c2/barrierSetC2.hpp"

class TypeOopPtr;

class ModRefBarrierSetC2: public BarrierSetC2 {
protected:
  virtual void pre_barrier(GraphKit* kit, bool do_load, Node* ctl, Node* obj, Node* adr, uint adr_idx, Node* val, const TypeOopPtr* val_type, Node* pre_val, BasicType bt) const { }
  virtual void post_barrier(GraphKit* kit, Node* ctl, Node* store, Node* obj, Node* adr, uint adr_idx, Node* val, BasicType bt, bool use_precise) const { }

  virtual Node* store_at_resolved(C2Access& access, C2AccessValue& val) const;

  virtual Node* atomic_cmpxchg_val_at_resolved(C2AtomicAccess& access, Node* expected_val, Node* new_val, const Type* value_type) const;
  virtual Node* atomic_cmpxchg_bool_at_resolved(C2AtomicAccess& access, Node* expected_val, Node* new_val, const Type* value_type) const;
  virtual Node* atomic_xchg_at_resolved(C2AtomicAccess& access, Node* new_val, const Type* value_type) const;
};

#endif
