#include "precompiled.hpp"
#include "opto/compile.hpp"
#include "opto/node.hpp"
#include "opto/optoreg.hpp"

// processor dependent initialization for i486

void Compile::pd_compiler2_init() {
  guarantee(CodeEntryAlignment >= InteriorEntryAlignment, "" );
  // QQQ presumably all 64bit cpu's support this. Seems like the ifdef could
  // simply be left out.
#ifndef AMD64
  if (!VM_Version::supports_cmov()) {
    ConditionalMoveLimit = 0;
  }
#endif

  if (UseAVX < 3) {
    int delta = XMMRegisterImpl::max_slots_per_register * XMMRegisterImpl::number_of_registers;
    int bottom = ConcreteRegisterImpl::max_fpr;
    int top = bottom + delta;
    int middle = bottom + (delta / 2);
    int xmm_slots = XMMRegisterImpl::max_slots_per_register;
    int lower = xmm_slots / 2;
    // mark bad every register that we cannot get to if AVX less than 3, we have all slots in the array
    // Note: vm2opto is allocated to ConcreteRegisterImpl::number_of_registers
    for (int i = bottom; i < middle; i += xmm_slots) {
      for (OptoReg::Name j = OptoReg::Name(i + lower); j<OptoReg::Name(i + xmm_slots); j = OptoReg::add(j, 1)) {
        OptoReg::invalidate(j);
      }
    }
    // mark the upper zmm bank bad and all the mask registers bad in this case
    for (OptoReg::Name i = OptoReg::Name(middle); i<OptoReg::Name(_last_Mach_Reg - 1); i = OptoReg::add(i, 1)) {
      OptoReg::invalidate(i);
    }
  }
}
