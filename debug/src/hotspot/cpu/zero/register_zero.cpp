#include "precompiled.hpp"
#include "register_zero.hpp"

const int ConcreteRegisterImpl::max_gpr = RegisterImpl::number_of_registers;
const int ConcreteRegisterImpl::max_fpr =
  ConcreteRegisterImpl::max_gpr + FloatRegisterImpl::number_of_registers;

const char* RegisterImpl::name() const {
  ShouldNotCallThis();
  return NULL;
}

const char* FloatRegisterImpl::name() const {
  ShouldNotCallThis();
  return NULL;
}
