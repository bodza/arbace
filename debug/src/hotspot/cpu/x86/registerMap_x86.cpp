#include "precompiled.hpp"

#include "runtime/registerMap.hpp"
#include "vmreg_x86.inline.hpp"

address RegisterMap::pd_location(VMReg reg) const {
  if (reg->is_XMMRegister()) {
    int regBase = reg->value() - ConcreteRegisterImpl::max_fpr;
    if (regBase % 4 == 0) {
      // Reads of the low and high 16 byte parts should be handled by location itself
      // because they have separate callee saved entries.
      // See RegisterSaver::save_live_registers().
      return NULL;
    }
    VMReg baseReg = as_XMMRegister(regBase / XMMRegisterImpl::max_slots_per_register)->as_VMReg();
    intptr_t offset = (reg->value() - baseReg->value()) * VMRegImpl::stack_slot_size; // offset in bytes
    if (offset >= 16) {
      // The high part of YMM registers are saved in a their own area in the frame
      baseReg = baseReg->next()->next()->next()->next();
      offset -= 16;
    }
    address baseLocation = location(baseReg);
    if (baseLocation != NULL) {
      return baseLocation + offset;
    }
  }
  return NULL;
}
