#include "precompiled.hpp"

#include "compiler/disassembler.hpp"
#include "oops/oop.inline.hpp"
#include "runtime/handles.inline.hpp"
#include "runtime/javaCalls.hpp"
#include "runtime/sharedRuntime.hpp"
#include "jvmci/jvmciEnv.hpp"
#include "jvmci/jvmciCodeInstaller.hpp"
#include "jvmci/jvmciJavaClasses.hpp"
#include "jvmci/jvmciCompilerToVM.hpp"
#include "jvmci/jvmciRuntime.hpp"
#include "asm/register.hpp"
#include "classfile/vmSymbols.hpp"
#include "code/vmreg.hpp"
#include "vmreg_x86.inline.hpp"

jint CodeInstaller::pd_next_offset(NativeInstruction* inst, jint pc_offset, Handle method, TRAPS) {
  if (inst->is_call() || inst->is_jump()) {
    return (pc_offset + NativeCall::instruction_size);
  } else if (inst->is_mov_literal64()) {
    // mov+call instruction pair
    jint offset = pc_offset + NativeMovConstReg::instruction_size;
    u_char* call = (u_char*) (_instructions->start() + offset);
    if (call[0] == Assembler::REX_B) {
      offset += 1; /* prefix byte for extended register R8-R15 */
      call++;
    }
    offset += 2; /* opcode byte + modrm byte */
    return (offset);
  } else if (inst->is_call_reg()) {
    // the inlined vtable stub contains a "call register" instruction
    return (pc_offset + ((NativeCallReg *) inst)->next_instruction_offset());
  } else if (inst->is_cond_jump()) {
    address pc = (address) (inst);
    return pc_offset + (jint) (Assembler::locate_next_instruction(pc) - pc);
  } else {
    JVMCI_ERROR_0("unsupported type of instruction for call site");
  }
}

void CodeInstaller::pd_patch_OopConstant(int pc_offset, Handle constant, TRAPS) {
  address pc = _instructions->start() + pc_offset;
  Handle obj(THREAD, HotSpotObjectConstantImpl::object(constant));
  jobject value = JNIHandles::make_local(obj());
  if (HotSpotObjectConstantImpl::compressed(constant)) {
    address operand = Assembler::locate_operand(pc, Assembler::narrow_oop_operand);
    int oop_index = _oop_recorder->find_index(value);
    _instructions->relocate(pc, oop_Relocation::spec(oop_index), Assembler::narrow_oop_operand);
  } else {
    address operand = Assembler::locate_operand(pc, Assembler::imm_operand);
    *((jobject*) operand) = value;
    _instructions->relocate(pc, oop_Relocation::spec_for_immediate(), Assembler::imm_operand);
  }
}

void CodeInstaller::pd_patch_MetaspaceConstant(int pc_offset, Handle constant, TRAPS) {
  address pc = _instructions->start() + pc_offset;
  if (HotSpotMetaspaceConstantImpl::compressed(constant)) {
    address operand = Assembler::locate_operand(pc, Assembler::narrow_oop_operand);
    *((narrowKlass*) operand) = record_narrow_metadata_reference(_instructions, operand, constant, CHECK);
  } else {
    address operand = Assembler::locate_operand(pc, Assembler::imm_operand);
    *((void**) operand) = record_metadata_reference(_instructions, operand, constant, CHECK);
  }
}

void CodeInstaller::pd_patch_DataSectionReference(int pc_offset, int data_offset, TRAPS) {
  address pc = _instructions->start() + pc_offset;

  address operand = Assembler::locate_operand(pc, Assembler::disp32_operand);
  address next_instruction = Assembler::locate_next_instruction(pc);
  address dest = _constants->start() + data_offset;

  long disp = dest - next_instruction;
  *((jint*) operand) = (jint) disp;

  _instructions->relocate(pc, section_word_Relocation::spec((address) dest, CodeBuffer::SECT_CONSTS), Assembler::disp32_operand);
}

void CodeInstaller::pd_relocate_ForeignCall(NativeInstruction* inst, jlong foreign_call_destination, TRAPS) {
  address pc = (address) inst;
  if (inst->is_call()) {
    // NOTE: for call without a mov, the offset must fit a 32-bit immediate
    NativeCall* call = nativeCall_at(pc);
    call->set_destination((address) foreign_call_destination);
    _instructions->relocate(call->instruction_address(), runtime_call_Relocation::spec(), Assembler::call32_operand);
  } else if (inst->is_mov_literal64()) {
    NativeMovConstReg* mov = nativeMovConstReg_at(pc);
    mov->set_data((intptr_t) foreign_call_destination);
    _instructions->relocate(mov->instruction_address(), runtime_call_Relocation::spec(), Assembler::imm_operand);
  } else if (inst->is_jump()) {
    NativeJump* jump = nativeJump_at(pc);
    jump->set_jump_destination((address) foreign_call_destination);
    _instructions->relocate(jump->instruction_address(), runtime_call_Relocation::spec(), Assembler::call32_operand);
  } else if (inst->is_cond_jump()) {
    address old_dest = nativeGeneralJump_at(pc)->jump_destination();
    address disp = Assembler::locate_operand(pc, Assembler::call32_operand);
    *(jint*) disp += ((address) foreign_call_destination) - old_dest;
    _instructions->relocate(pc, runtime_call_Relocation::spec(), Assembler::call32_operand);
  } else {
    JVMCI_ERROR("unsupported relocation for foreign call");
  }
}

void CodeInstaller::pd_relocate_JavaMethod(CodeBuffer &, Handle hotspot_method, jint pc_offset, TRAPS) {
  switch (_next_call_type) {
    case INLINE_INVOKE:
      break;
    case INVOKEVIRTUAL:
    case INVOKEINTERFACE: {
      NativeCall* call = nativeCall_at(_instructions->start() + pc_offset);
      call->set_destination(SharedRuntime::get_resolve_virtual_call_stub());
      _instructions->relocate(call->instruction_address(),
                                             virtual_call_Relocation::spec(_invoke_mark_pc),
                                             Assembler::call32_operand);
      break;
    }
    case INVOKESTATIC: {
      NativeCall* call = nativeCall_at(_instructions->start() + pc_offset);
      call->set_destination(SharedRuntime::get_resolve_static_call_stub());
      _instructions->relocate(call->instruction_address(),
                                             relocInfo::static_call_type, Assembler::call32_operand);
      break;
    }
    case INVOKESPECIAL: {
      NativeCall* call = nativeCall_at(_instructions->start() + pc_offset);
      call->set_destination(SharedRuntime::get_resolve_opt_virtual_call_stub());
      _instructions->relocate(call->instruction_address(),
                              relocInfo::opt_virtual_call_type, Assembler::call32_operand);
      break;
    }
    default:
      JVMCI_ERROR("invalid _next_call_type value");
      break;
  }
}

static void relocate_poll_near(address pc) {
  NativeInstruction* ni = nativeInstruction_at(pc);
  int32_t* disp = (int32_t*) Assembler::locate_operand(pc, Assembler::disp32_operand);
  int32_t offset = *disp; // The Java code installed the polling page offset into the disp32 operand
  intptr_t new_disp = (intptr_t) (os::get_polling_page() + offset) - (intptr_t) ni;
  *disp = (int32_t)new_disp;
}

void CodeInstaller::pd_relocate_poll(address pc, jint mark, TRAPS) {
  switch (mark) {
    case POLL_NEAR: {
      relocate_poll_near(pc);
      _instructions->relocate(pc, relocInfo::poll_type, Assembler::disp32_operand);
      break;
    }
    case POLL_FAR:
      // This is a load from a register so there is no relocatable operand.
      // We just have to ensure that the format is not disp32_operand
      // so that poll_Relocation::fix_relocation_after_move does the right
      // thing (i.e. ignores this relocation record)
      _instructions->relocate(pc, relocInfo::poll_type, Assembler::imm_operand);
      break;
    case POLL_RETURN_NEAR: {
      relocate_poll_near(pc);
      _instructions->relocate(pc, relocInfo::poll_return_type, Assembler::disp32_operand);
      break;
    }
    case POLL_RETURN_FAR:
      // see comment above for POLL_FAR
      _instructions->relocate(pc, relocInfo::poll_return_type, Assembler::imm_operand);
      break;
    default:
      JVMCI_ERROR("invalid mark value: %d", mark);
      break;
  }
}

// convert JVMCI register indices (as used in oop maps) to HotSpot registers
VMReg CodeInstaller::get_hotspot_reg(jint jvmci_reg, TRAPS) {
  if (jvmci_reg < RegisterImpl::number_of_registers) {
    return as_Register(jvmci_reg)->as_VMReg();
  } else {
    jint floatRegisterNumber = jvmci_reg - RegisterImpl::number_of_registers;
    if (floatRegisterNumber < XMMRegisterImpl::number_of_registers) {
      return as_XMMRegister(floatRegisterNumber)->as_VMReg();
    }
    JVMCI_ERROR_NULL("invalid register number: %d", jvmci_reg);
  }
}

bool CodeInstaller::is_general_purpose_reg(VMReg hotspotRegister) {
  return !(hotspotRegister->is_FloatRegister() || hotspotRegister->is_XMMRegister());
}
