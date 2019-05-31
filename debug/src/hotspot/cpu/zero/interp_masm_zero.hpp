#ifndef CPU_ZERO_VM_INTERP_MASM_ZERO_HPP
#define CPU_ZERO_VM_INTERP_MASM_ZERO_HPP

#include "asm/codeBuffer.hpp"
#include "asm/macroAssembler.hpp"
#include "interpreter/invocationCounter.hpp"

// This file specializes the assember with interpreter-specific macros

class InterpreterMacroAssembler : public MacroAssembler {
 public:
  InterpreterMacroAssembler(CodeBuffer* code) : MacroAssembler(code) { }

 public:
  RegisterOrConstant delayed_value_impl(intptr_t* delayed_value_addr,
                                        Register  tmp,
                                        int       offset) {
    ShouldNotCallThis();
    return RegisterOrConstant();
  }
};

#endif
