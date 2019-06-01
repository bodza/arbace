#include "precompiled.hpp"

#include "asm/register.hpp"
#include "c1/c1_FrameMap.hpp"
#include "c1/c1_LIR.hpp"

FloatRegister LIR_OprDesc::as_float_reg() const {
  ShouldNotReachHere();
  return fnoreg;
}

FloatRegister LIR_OprDesc::as_double_reg() const {
  ShouldNotReachHere();
  return fnoreg;
}

XMMRegister LIR_OprDesc::as_xmm_float_reg() const {
  return FrameMap::nr2xmmreg(xmm_regnr());
}

XMMRegister LIR_OprDesc::as_xmm_double_reg() const {
  return FrameMap::nr2xmmreg(xmm_regnrLo());
}

// Reg2 unused.
LIR_Opr LIR_OprFact::double_fpu(int reg1, int reg2) {
  return (LIR_Opr)(intptr_t)((reg1 << LIR_OprDesc::reg1_shift) |
                             (reg1 << LIR_OprDesc::reg2_shift) |
                             LIR_OprDesc::double_type          |
                             LIR_OprDesc::fpu_register         |
                             LIR_OprDesc::double_size);
}
