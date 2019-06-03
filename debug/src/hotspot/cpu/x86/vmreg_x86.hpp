#ifndef CPU_X86_VM_VMREG_X86_HPP
#define CPU_X86_VM_VMREG_X86_HPP

inline bool is_Register() {
  return (unsigned int) value() < (unsigned int) ConcreteRegisterImpl::max_gpr;
}

inline bool is_FloatRegister() {
  return value() >= ConcreteRegisterImpl::max_gpr && value() < ConcreteRegisterImpl::max_fpr;
}

inline bool is_XMMRegister() {
  int uarch_max_xmm = ConcreteRegisterImpl::max_xmm;

  if (UseAVX < 3) {
    int half_xmm = (XMMRegisterImpl::max_slots_per_register * XMMRegisterImpl::number_of_registers) / 2;
    uarch_max_xmm -= half_xmm;
  }

  return (value() >= ConcreteRegisterImpl::max_fpr && value() < uarch_max_xmm);
}

inline bool is_KRegister() {
  if (UseAVX > 2) {
    return value() >= ConcreteRegisterImpl::max_xmm && value() < ConcreteRegisterImpl::max_kpr;
  } else {
    return false;
  }
}

inline Register as_Register() {
  // Yuk
#ifdef AMD64
  return ::as_Register(value() >> 1);
#else
  return ::as_Register(value());
#endif
}

inline FloatRegister as_FloatRegister() {
  // Yuk
  return ::as_FloatRegister((value() - ConcreteRegisterImpl::max_gpr) >> 1);
}

inline XMMRegister as_XMMRegister() {
  // Yuk
  return ::as_XMMRegister((value() - ConcreteRegisterImpl::max_fpr) >> 4);
}

inline KRegister as_KRegister() {
  // Yuk
  return ::as_KRegister((value() - ConcreteRegisterImpl::max_xmm));
}

inline bool is_concrete() {
#ifndef AMD64
  if (is_Register()) return true;
#endif
  return is_even(value());
}

#endif
