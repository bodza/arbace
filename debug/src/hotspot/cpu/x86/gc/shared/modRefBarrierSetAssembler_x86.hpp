#ifndef CPU_X86_GC_SHARED_MODREFBARRIERSETASSEMBLER_X86_HPP
#define CPU_X86_GC_SHARED_MODREFBARRIERSETASSEMBLER_X86_HPP

#include "asm/macroAssembler.hpp"
#include "gc/shared/barrierSetAssembler.hpp"

// The ModRefBarrierSetAssembler filters away accesses on BasicTypes other
// than T_OBJECT/T_ARRAY (oops). The oop accesses call one of the protected
// accesses, which are overridden in the concrete BarrierSetAssembler.

class ModRefBarrierSetAssembler: public BarrierSetAssembler {
protected:
  virtual void gen_write_ref_array_pre_barrier(MacroAssembler* masm, DecoratorSet decorators,
                                               Register addr, Register count) {}
  virtual void gen_write_ref_array_post_barrier(MacroAssembler* masm, DecoratorSet decorators,
                                                Register addr, Register count, Register tmp) {}
  virtual void oop_store_at(MacroAssembler* masm, DecoratorSet decorators, BasicType type,
                            Address dst, Register val, Register tmp1, Register tmp2) = 0;
public:
  virtual void arraycopy_prologue(MacroAssembler* masm, DecoratorSet decorators, BasicType type,
                                  Register src, Register dst, Register count);
  virtual void arraycopy_epilogue(MacroAssembler* masm, DecoratorSet decorators, BasicType type,
                                  Register src, Register dst, Register count);

  virtual void store_at(MacroAssembler* masm, DecoratorSet decorators, BasicType type,
                        Address dst, Register val, Register tmp1, Register tmp2);
};

#endif
