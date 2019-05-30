#ifndef CPU_AARCH64_VM_VMREG_AARCH64_HPP
#define CPU_AARCH64_VM_VMREG_AARCH64_HPP

inline bool is_Register() {
  return (unsigned int) value() < (unsigned int) ConcreteRegisterImpl::max_gpr;
}

inline bool is_FloatRegister() {
  return value() >= ConcreteRegisterImpl::max_gpr && value() < ConcreteRegisterImpl::max_fpr;
}

inline Register as_Register() {

  assert( is_Register(), "must be");
  // Yuk
  return ::as_Register(value() >> 1);
}

inline FloatRegister as_FloatRegister() {
  assert( is_FloatRegister() && is_even(value()), "must be" );
  // Yuk
  return ::as_FloatRegister((value() - ConcreteRegisterImpl::max_gpr) >> 1);
}

inline   bool is_concrete() {
  assert(is_reg(), "must be");
  return is_even(value());
}

#endif
