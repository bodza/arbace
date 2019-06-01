#include "precompiled.hpp"

#include "asm/assembler.hpp"
#include "code/vmreg.hpp"

// First VMReg value that could refer to a stack slot
VMReg VMRegImpl::stack0 = (VMReg)(intptr_t)((ConcreteRegisterImpl::number_of_registers + 7) & ~7);

// VMRegs are 4 bytes wide on all platforms
const int VMRegImpl::stack_slot_size = 4;
const int VMRegImpl::slots_per_word = wordSize / stack_slot_size;

const int VMRegImpl::register_count = ConcreteRegisterImpl::number_of_registers;
// Register names
const char *VMRegImpl::regName[ConcreteRegisterImpl::number_of_registers];

void VMRegImpl::print_on(outputStream* st) const {
  if (is_reg()) {
    st->print("%s",VMRegImpl::regName[value()]);
  } else if (is_stack()) {
    int stk = value() - stack0->value();
    st->print("[%d]", stk*4);
  } else {
    st->print("BAD!");
  }
}
