#include "jvmci/jvmciCodeInstaller.hpp"
#include "jvmci/jvmciRuntime.hpp"
#include "jvmci/jvmciCompilerToVM.hpp"
#include "jvmci/jvmciJavaClasses.hpp"
#include "oops/oop.inline.hpp"
#include "runtime/handles.inline.hpp"
#include "runtime/sharedRuntime.hpp"
#include "vmreg_aarch64.inline.hpp"

jint CodeInstaller::pd_next_offset(NativeInstruction* inst, jint pc_offset, Handle method, TRAPS) {
  if (inst->is_call() || inst->is_jump() || inst->is_blr()) {
    return pc_offset + NativeCall::instruction_size;
  } else if (inst->is_general_jump()) {
    return pc_offset + NativeGeneralJump::instruction_size;
  } else if (NativeInstruction::is_adrp_at((address)inst)) {
    // adrp; add; blr
    return pc_offset + 3 * NativeInstruction::instruction_size;
  } else {
    JVMCI_ERROR_0("unsupported type of instruction for call site");
  }
}

void CodeInstaller::pd_patch_OopConstant(int pc_offset, Handle constant, TRAPS) {
  address pc = _instructions->start() + pc_offset;
  Handle obj(THREAD, HotSpotObjectConstantImpl::object(constant));
  jobject value = JNIHandles::make_local(obj());
  MacroAssembler::patch_oop(pc, (address)obj());
  int oop_index = _oop_recorder->find_index(value);
  RelocationHolder rspec = oop_Relocation::spec(oop_index);
  _instructions->relocate(pc, rspec);
}

void CodeInstaller::pd_patch_MetaspaceConstant(int pc_offset, Handle constant, TRAPS) {
  address pc = _instructions->start() + pc_offset;
  if (HotSpotMetaspaceConstantImpl::compressed(constant)) {
    narrowKlass narrowOop = record_narrow_metadata_reference(_instructions, pc, constant, CHECK);
    MacroAssembler::patch_narrow_klass(pc, narrowOop);
  } else {
    NativeMovConstReg* move = nativeMovConstReg_at(pc);
    void* reference = record_metadata_reference(_instructions, pc, constant, CHECK);
    move->set_data((intptr_t) reference);
  }
}

void CodeInstaller::pd_patch_DataSectionReference(int pc_offset, int data_offset, TRAPS) {
  address pc = _instructions->start() + pc_offset;
  NativeInstruction* inst = nativeInstruction_at(pc);
  if (inst->is_adr_aligned() || inst->is_ldr_literal() || (NativeInstruction::maybe_cpool_ref(pc))) {
    address dest = _constants->start() + data_offset;
    _instructions->relocate(pc, section_word_Relocation::spec((address) dest, CodeBuffer::SECT_CONSTS));
  } else {
    JVMCI_ERROR("unknown load or move instruction at " PTR_FORMAT, p2i(pc));
  }
}

void CodeInstaller::pd_relocate_ForeignCall(NativeInstruction* inst, jlong foreign_call_destination, TRAPS) {
  address pc = (address) inst;
  if (inst->is_call()) {
    NativeCall* call = nativeCall_at(pc);
    call->set_destination((address) foreign_call_destination);
    _instructions->relocate(call->instruction_address(), runtime_call_Relocation::spec());
  } else if (inst->is_jump()) {
    NativeJump* jump = nativeJump_at(pc);
    jump->set_jump_destination((address) foreign_call_destination);
    _instructions->relocate(jump->instruction_address(), runtime_call_Relocation::spec());
  } else if (inst->is_general_jump()) {
    NativeGeneralJump* jump = nativeGeneralJump_at(pc);
    jump->set_jump_destination((address) foreign_call_destination);
    _instructions->relocate(jump->instruction_address(), runtime_call_Relocation::spec());
  } else if (NativeInstruction::is_adrp_at((address)inst)) {
    // adrp; add; blr
    MacroAssembler::pd_patch_instruction_size((address)inst,
                                              (address)foreign_call_destination);
  } else {
    JVMCI_ERROR("unknown call or jump instruction at " PTR_FORMAT, p2i(pc));
  }
}

void CodeInstaller::pd_relocate_JavaMethod(CodeBuffer &cbuf, Handle hotspot_method, jint pc_offset, TRAPS) {
  switch (_next_call_type) {
    case INLINE_INVOKE:
      break;
    case INVOKEVIRTUAL:
    case INVOKEINTERFACE: {
      NativeCall* call = nativeCall_at(_instructions->start() + pc_offset);
      _instructions->relocate(call->instruction_address(), virtual_call_Relocation::spec(_invoke_mark_pc));
      call->trampoline_jump(cbuf, SharedRuntime::get_resolve_virtual_call_stub());
      break;
    }
    case INVOKESTATIC: {
      NativeCall* call = nativeCall_at(_instructions->start() + pc_offset);
      _instructions->relocate(call->instruction_address(), relocInfo::static_call_type);
      call->trampoline_jump(cbuf, SharedRuntime::get_resolve_static_call_stub());
      break;
    }
    case INVOKESPECIAL: {
      NativeCall* call = nativeCall_at(_instructions->start() + pc_offset);
      _instructions->relocate(call->instruction_address(), relocInfo::opt_virtual_call_type);
      call->trampoline_jump(cbuf, SharedRuntime::get_resolve_opt_virtual_call_stub());
      break;
    }
    default:
      JVMCI_ERROR("invalid _next_call_type value");
      break;
  }
}

void CodeInstaller::pd_relocate_poll(address pc, jint mark, TRAPS) {
  switch (mark) {
    case POLL_NEAR:
      JVMCI_ERROR("unimplemented");
      break;
    case POLL_FAR:
      _instructions->relocate(pc, relocInfo::poll_type);
      break;
    case POLL_RETURN_NEAR:
      JVMCI_ERROR("unimplemented");
      break;
    case POLL_RETURN_FAR:
      _instructions->relocate(pc, relocInfo::poll_return_type);
      break;
    default:
      JVMCI_ERROR("invalid mark value");
      break;
  }
}

// convert JVMCI register indices (as used in oop maps) to HotSpot registers
VMReg CodeInstaller::get_hotspot_reg(jint jvmci_reg, TRAPS) {
  if (jvmci_reg < RegisterImpl::number_of_registers) {
    return as_Register(jvmci_reg)->as_VMReg();
  } else {
    jint floatRegisterNumber = jvmci_reg - RegisterImpl::number_of_registers_for_jvmci;
    if (floatRegisterNumber >= 0 && floatRegisterNumber < FloatRegisterImpl::number_of_registers) {
      return as_FloatRegister(floatRegisterNumber)->as_VMReg();
    }
    JVMCI_ERROR_NULL("invalid register number: %d", jvmci_reg);
  }
}

bool CodeInstaller::is_general_purpose_reg(VMReg hotspotRegister) {
  return !hotspotRegister->is_FloatRegister();
}
