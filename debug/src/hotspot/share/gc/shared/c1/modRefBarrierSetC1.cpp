#include "precompiled.hpp"

#include "gc/shared/c1/modRefBarrierSetC1.hpp"
#include "utilities/macros.hpp"

#define __ gen->lir()->

void ModRefBarrierSetC1::store_at_resolved(LIRAccess& access, LIR_Opr value) {
  DecoratorSet decorators = access.decorators();
  bool is_array = (decorators & IS_ARRAY) != 0;
  bool on_anonymous = (decorators & ON_UNKNOWN_OOP_REF) != 0;

  if (access.is_oop()) {
    pre_barrier(access, access.resolved_addr(),
                LIR_OprFact::illegalOpr /* pre_val */, access.patch_emit_info());
  }

  BarrierSetC1::store_at_resolved(access, value);

  if (access.is_oop()) {
    bool precise = is_array || on_anonymous;
    LIR_Opr post_addr = precise ? access.resolved_addr() : access.base().opr();
    post_barrier(access, post_addr, value);
  }
}

LIR_Opr ModRefBarrierSetC1::atomic_cmpxchg_at_resolved(LIRAccess& access, LIRItem& cmp_value, LIRItem& new_value) {
  if (access.is_oop()) {
    pre_barrier(access, access.resolved_addr(),
                LIR_OprFact::illegalOpr /* pre_val */, NULL);
  }

  LIR_Opr result = BarrierSetC1::atomic_cmpxchg_at_resolved(access, cmp_value, new_value);

  if (access.is_oop()) {
    post_barrier(access, access.resolved_addr(), new_value.result());
  }

  return result;
}

LIR_Opr ModRefBarrierSetC1::atomic_xchg_at_resolved(LIRAccess& access, LIRItem& value) {
  if (access.is_oop()) {
    pre_barrier(access, access.resolved_addr(),
                LIR_OprFact::illegalOpr /* pre_val */, NULL);
  }

  LIR_Opr result = BarrierSetC1::atomic_xchg_at_resolved(access, value);

  if (access.is_oop()) {
    post_barrier(access, access.resolved_addr(), value.result());
  }

  return result;
}

// This overrides the default to resolve the address into a register,
// assuming it will be used by a write barrier anyway.
LIR_Opr ModRefBarrierSetC1::resolve_address(LIRAccess& access, bool resolve_in_register) {
  DecoratorSet decorators = access.decorators();
  bool needs_patching = (decorators & C1_NEEDS_PATCHING) != 0;
  bool is_write = (decorators & C1_WRITE_ACCESS) != 0;
  bool is_array = (decorators & IS_ARRAY) != 0;
  bool on_anonymous = (decorators & ON_UNKNOWN_OOP_REF) != 0;
  bool precise = is_array || on_anonymous;
  resolve_in_register |= !needs_patching && is_write && access.is_oop() && precise;
  return BarrierSetC1::resolve_address(access, resolve_in_register);
}
