#ifndef CPU_AARCH64_GC_SHARED_CARDTABLEBARRIERSETASSEMBLER_AARCH64_HPP
#define CPU_AARCH64_GC_SHARED_CARDTABLEBARRIERSETASSEMBLER_AARCH64_HPP

#include "asm/macroAssembler.hpp"
#include "gc/shared/modRefBarrierSetAssembler.hpp"

class CardTableBarrierSetAssembler: public ModRefBarrierSetAssembler {
protected:
  void store_check(MacroAssembler* masm, Register obj, Address dst);

  virtual void gen_write_ref_array_post_barrier(MacroAssembler* masm, DecoratorSet decorators, Register start, Register end, Register tmp, RegSet saved_regs);
  virtual void oop_store_at(MacroAssembler* masm, DecoratorSet decorators, BasicType type, Address dst, Register val, Register tmp1, Register tmp2);
};

#endif
