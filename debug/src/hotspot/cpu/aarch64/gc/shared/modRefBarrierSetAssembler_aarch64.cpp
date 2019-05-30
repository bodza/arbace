#include "precompiled.hpp"
#include "asm/macroAssembler.inline.hpp"
#include "gc/shared/modRefBarrierSetAssembler.hpp"

#define __ masm->

void ModRefBarrierSetAssembler::arraycopy_prologue(MacroAssembler* masm, DecoratorSet decorators, bool is_oop, Register addr, Register count, RegSet saved_regs) {

  if (is_oop) {
    gen_write_ref_array_pre_barrier(masm, decorators, addr, count, saved_regs);
  }
}

void ModRefBarrierSetAssembler::arraycopy_epilogue(MacroAssembler* masm, DecoratorSet decorators, bool is_oop, Register start, Register end, Register tmp, RegSet saved_regs) {
  if (is_oop) {
    gen_write_ref_array_post_barrier(masm, decorators, start, end, tmp, saved_regs);
  }
}

void ModRefBarrierSetAssembler::store_at(MacroAssembler* masm, DecoratorSet decorators, BasicType type, Address dst, Register val, Register tmp1, Register tmp2) {
  if (type == T_OBJECT || type == T_ARRAY) {
    oop_store_at(masm, decorators, type, dst, val, tmp1, tmp2);
  } else {
    BarrierSetAssembler::store_at(masm, decorators, type, dst, val, tmp1, tmp2);
  }
}
