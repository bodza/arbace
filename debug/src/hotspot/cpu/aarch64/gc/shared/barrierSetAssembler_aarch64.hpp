#ifndef CPU_AARCH64_GC_SHARED_BARRIERSETASSEMBLER_AARCH64_HPP
#define CPU_AARCH64_GC_SHARED_BARRIERSETASSEMBLER_AARCH64_HPP

#include "asm/macroAssembler.hpp"
#include "memory/allocation.hpp"
#include "oops/access.hpp"

class BarrierSetAssembler: public CHeapObj<mtGC> {
private:
  void incr_allocated_bytes(MacroAssembler* masm,
                            Register var_size_in_bytes, int con_size_in_bytes,
                            Register t1 = noreg);

public:
  virtual void arraycopy_prologue(MacroAssembler* masm, DecoratorSet decorators, bool is_oop,
                                  Register addr, Register count, RegSet saved_regs) {}
  virtual void arraycopy_epilogue(MacroAssembler* masm, DecoratorSet decorators, bool is_oop,
                                  Register start, Register end, Register tmp, RegSet saved_regs) {}
  virtual void load_at(MacroAssembler* masm, DecoratorSet decorators, BasicType type,
                       Register dst, Address src, Register tmp1, Register tmp_thread);
  virtual void store_at(MacroAssembler* masm, DecoratorSet decorators, BasicType type,
                        Address dst, Register val, Register tmp1, Register tmp2);

  virtual void obj_equals(MacroAssembler* masm,
                          Register obj1, Register obj2);

  virtual void try_resolve_jobject_in_native(MacroAssembler* masm, Register jni_env,
                                             Register obj, Register tmp, Label& slowpath);

  virtual void tlab_allocate(MacroAssembler* masm,
    Register obj,                      // result: pointer to object after successful allocation
    Register var_size_in_bytes,        // object size in bytes if unknown at compile time; invalid otherwise
    int      con_size_in_bytes,        // object size in bytes if   known at compile time
    Register t1,                       // temp register
    Register t2,                       // temp register
    Label&   slow_case                 // continuation point if fast allocation fails
  );

  void eden_allocate(MacroAssembler* masm,
    Register obj,                      // result: pointer to object after successful allocation
    Register var_size_in_bytes,        // object size in bytes if unknown at compile time; invalid otherwise
    int      con_size_in_bytes,        // object size in bytes if   known at compile time
    Register t1,                       // temp register
    Label&   slow_case                 // continuation point if fast allocation fails
  );
  virtual void barrier_stubs_init() {}
};

#endif
