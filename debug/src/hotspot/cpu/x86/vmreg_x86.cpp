#include "precompiled.hpp"
#include "asm/assembler.hpp"
#include "code/vmreg.hpp"

void VMRegImpl::set_regName() {
  Register reg = ::as_Register(0);
  int i;
  for (i = 0; i < ConcreteRegisterImpl::max_gpr; ) {
    regName[i++] = reg->name();
#ifdef AMD64
    regName[i++] = reg->name();
#endif
    reg = reg->successor();
  }

  FloatRegister freg = ::as_FloatRegister(0);
  for ( ; i < ConcreteRegisterImpl::max_fpr; ) {
    regName[i++] = freg->name();
    regName[i++] = freg->name();
    freg = freg->successor();
  }

  XMMRegister xreg = ::as_XMMRegister(0);
  for (; i < ConcreteRegisterImpl::max_xmm;) {
    for (int j = 0; j < XMMRegisterImpl::max_slots_per_register; j++) {
      regName[i++] = xreg->name();
    }
    xreg = xreg->successor();
  }

  KRegister kreg = ::as_KRegister(0);
  for (; i < ConcreteRegisterImpl::max_kpr;) {
    for (int j = 0; j < KRegisterImpl::max_slots_per_register; j++) {
      regName[i++] = kreg->name();
    }
    kreg = kreg->successor();
  }

  for ( ; i < ConcreteRegisterImpl::number_of_registers; i ++ ) {
    regName[i] = "NON-GPR-FPR-XMM-KREG";
  }
}
