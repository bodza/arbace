#ifndef CPU_AARCH64_VM_VMREG_AARCH64_INLINE_HPP
#define CPU_AARCH64_VM_VMREG_AARCH64_INLINE_HPP

inline VMReg RegisterImpl::as_VMReg() {
  if (this==noreg ) return VMRegImpl::Bad();
  return VMRegImpl::as_VMReg(encoding() << 1 );
}

inline VMReg FloatRegisterImpl::as_VMReg() {
  return VMRegImpl::as_VMReg((encoding() << 1) + ConcreteRegisterImpl::max_gpr);
}

#endif
