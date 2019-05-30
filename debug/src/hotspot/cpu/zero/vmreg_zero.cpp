#include "precompiled.hpp"
#include "asm/assembler.hpp"
#include "code/vmreg.hpp"

void VMRegImpl::set_regName() {
  int i = 0;
  Register reg = ::as_Register(0);
  for ( ; i < ConcreteRegisterImpl::max_gpr ; ) {
    regName[i++] = reg->name();
    reg = reg->successor();
  }
  FloatRegister freg = ::as_FloatRegister(0);
  for ( ; i < ConcreteRegisterImpl::max_fpr ; ) {
    regName[i++] = freg->name();
    freg = freg->successor();
  }
  assert(i == ConcreteRegisterImpl::number_of_registers, "fix this");
}

bool VMRegImpl::is_Register() {
  return value() >= 0 &&
         value() < ConcreteRegisterImpl::max_gpr;
}

bool VMRegImpl::is_FloatRegister() {
  return value() >= ConcreteRegisterImpl::max_gpr &&
         value() < ConcreteRegisterImpl::max_fpr;
}

Register VMRegImpl::as_Register() {
  assert(is_Register(), "must be");
  return ::as_Register(value());
}

FloatRegister VMRegImpl::as_FloatRegister() {
  assert(is_FloatRegister(), "must be" );
  return ::as_FloatRegister(value() - ConcreteRegisterImpl::max_gpr);
}
