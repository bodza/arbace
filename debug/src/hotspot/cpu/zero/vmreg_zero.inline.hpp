#ifndef CPU_ZERO_VM_VMREG_ZERO_INLINE_HPP
#define CPU_ZERO_VM_VMREG_ZERO_INLINE_HPP

inline VMReg RegisterImpl::as_VMReg() {
  return VMRegImpl::as_VMReg(encoding());
}

inline VMReg FloatRegisterImpl::as_VMReg() {
  return VMRegImpl::as_VMReg(encoding() + ConcreteRegisterImpl::max_gpr);
}

#endif
