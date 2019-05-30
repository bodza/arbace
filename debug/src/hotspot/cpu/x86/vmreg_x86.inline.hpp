#ifndef CPU_X86_VM_VMREG_X86_INLINE_HPP
#define CPU_X86_VM_VMREG_X86_INLINE_HPP

inline VMReg RegisterImpl::as_VMReg() {
  if( this==noreg ) return VMRegImpl::Bad();
#ifdef AMD64
  return VMRegImpl::as_VMReg(encoding() << 1 );
#else
  return VMRegImpl::as_VMReg(encoding() );
#endif
}

inline VMReg FloatRegisterImpl::as_VMReg() {
  return VMRegImpl::as_VMReg((encoding() << 1) + ConcreteRegisterImpl::max_gpr);
}

inline VMReg XMMRegisterImpl::as_VMReg() {
  return VMRegImpl::as_VMReg((encoding() << 4) + ConcreteRegisterImpl::max_fpr);
}

inline VMReg KRegisterImpl::as_VMReg() {
  return VMRegImpl::as_VMReg(encoding() + ConcreteRegisterImpl::max_xmm);
}

#endif
