#ifndef CPU_X86_GC_G1_G1BARRIERSETASSEMBLER_X86_HPP
#define CPU_X86_GC_G1_G1BARRIERSETASSEMBLER_X86_HPP

#include "asm/macroAssembler.hpp"
#include "gc/shared/modRefBarrierSetAssembler.hpp"

class LIR_Assembler;
class StubAssembler;
class G1PreBarrierStub;
class G1PostBarrierStub;

class G1BarrierSetAssembler: public ModRefBarrierSetAssembler {
 protected:
  virtual void gen_write_ref_array_pre_barrier(MacroAssembler* masm, DecoratorSet decorators, Register addr, Register count);
  virtual void gen_write_ref_array_post_barrier(MacroAssembler* masm, DecoratorSet decorators, Register addr, Register count, Register tmp);

  void g1_write_barrier_pre(MacroAssembler* masm,
                            Register obj,
                            Register pre_val,
                            Register thread,
                            Register tmp,
                            bool tosca_live,
                            bool expand_call);

  void g1_write_barrier_post(MacroAssembler* masm,
                             Register store_addr,
                             Register new_val,
                             Register thread,
                             Register tmp,
                             Register tmp2);

  virtual void oop_store_at(MacroAssembler* masm, DecoratorSet decorators, BasicType type,
                            Address dst, Register val, Register tmp1, Register tmp2);

 public:
  void gen_pre_barrier_stub(LIR_Assembler* ce, G1PreBarrierStub* stub);
  void gen_post_barrier_stub(LIR_Assembler* ce, G1PostBarrierStub* stub);

  void generate_c1_pre_barrier_runtime_stub(StubAssembler* sasm);
  void generate_c1_post_barrier_runtime_stub(StubAssembler* sasm);

  virtual void load_at(MacroAssembler* masm, DecoratorSet decorators, BasicType type,
                       Register dst, Address src, Register tmp1, Register tmp_thread);
};

#endif
