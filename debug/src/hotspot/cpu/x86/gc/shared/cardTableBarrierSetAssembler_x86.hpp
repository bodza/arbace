#ifndef CPU_X86_GC_SHARED_CARDTABLEBARRIERSETASSEMBLER_X86_HPP
#define CPU_X86_GC_SHARED_CARDTABLEBARRIERSETASSEMBLER_X86_HPP

#include "asm/macroAssembler.hpp"
#include "gc/shared/modRefBarrierSetAssembler.hpp"

class CardTableBarrierSetAssembler: public ModRefBarrierSetAssembler {
protected:
  void store_check(MacroAssembler* masm, Register obj, Address dst);

  virtual void gen_write_ref_array_post_barrier(MacroAssembler* masm, DecoratorSet decorators, Register addr, Register count, Register tmp);
  virtual void oop_store_at(MacroAssembler* masm, DecoratorSet decorators, BasicType type, Address dst, Register val, Register tmp1, Register tmp2);
};

#endif
