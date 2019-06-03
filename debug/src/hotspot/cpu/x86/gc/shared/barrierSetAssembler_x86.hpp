#ifndef CPU_X86_GC_SHARED_BARRIERSETASSEMBLER_X86_HPP
#define CPU_X86_GC_SHARED_BARRIERSETASSEMBLER_X86_HPP

#include "asm/macroAssembler.hpp"
#include "memory/allocation.hpp"
#include "oops/access.hpp"

class InterpreterMacroAssembler;

class BarrierSetAssembler: public CHeapObj<mtGC> {
private:
  void incr_allocated_bytes(MacroAssembler* masm, Register thread, Register var_size_in_bytes, int con_size_in_bytes, Register t1);

public:
  virtual void arraycopy_prologue(MacroAssembler* masm, DecoratorSet decorators, BasicType type, Register src, Register dst, Register count) { }
  virtual void arraycopy_epilogue(MacroAssembler* masm, DecoratorSet decorators, BasicType type, Register src, Register dst, Register count) { }

  virtual void load_at(MacroAssembler* masm, DecoratorSet decorators, BasicType type, Register dst, Address src, Register tmp1, Register tmp_thread);
  virtual void store_at(MacroAssembler* masm, DecoratorSet decorators, BasicType type, Address dst, Register val, Register tmp1, Register tmp2);

  virtual void obj_equals(MacroAssembler* masm, Register obj1, Register obj2);
  virtual void obj_equals(MacroAssembler* masm, Register obj1, Address obj2);

  // Support for jniFastGetField to try resolving a jobject/jweak in native
  virtual void try_resolve_jobject_in_native(MacroAssembler* masm, Register jni_env, Register obj, Register tmp, Label& slowpath);

  virtual void tlab_allocate(MacroAssembler* masm,
                             Register thread, Register obj,
                             Register var_size_in_bytes,
                             int con_size_in_bytes,
                             Register t1, Register t2,
                             Label& slow_case);
  virtual void eden_allocate(MacroAssembler* masm,
                             Register thread, Register obj,
                             Register var_size_in_bytes,
                             int con_size_in_bytes,
                             Register t1,
                             Label& slow_case);

  virtual void barrier_stubs_init() { }
};

#endif
