#include "precompiled.hpp"

#include "asm/macroAssembler.hpp"
#include "code/relocInfo.hpp"
#include "nativeInst_aarch64.hpp"
#include "oops/oop.inline.hpp"
#include "runtime/safepoint.hpp"

void Relocation::pd_set_data_value(address x, intptr_t o, bool verify_only) {
  if (verify_only)
    return;

  int bytes;

  switch(type()) {
  case relocInfo::oop_type:
    {
      oop_Relocation *reloc = (oop_Relocation *)this;
      if (NativeInstruction::is_ldr_literal_at(addr())) {
        address constptr = (address)code()->oop_addr_at(reloc->oop_index());
        bytes = MacroAssembler::pd_patch_instruction_size(addr(), constptr);
      } else{
        bytes = MacroAssembler::patch_oop(addr(), x);
      }
    }
    break;
  default:
    bytes = MacroAssembler::pd_patch_instruction_size(addr(), x);
    break;
  }
  ICache::invalidate_range(addr(), bytes);
}

address Relocation::pd_call_destination(address orig_addr) {
  if (NativeCall::is_call_at(addr())) {
    address trampoline = nativeCall_at(addr())->get_trampoline();
    if (trampoline) {
      return nativeCallTrampolineStub_at(trampoline)->destination();
    }
  }
  if (orig_addr != NULL) {
    address new_addr = MacroAssembler::pd_call_destination(orig_addr);
    // If call is branch to self, don't try to relocate it, just leave it
    // as branch to self. This happens during code generation if the code
    // buffer expands. It will be relocated to the trampoline above once
    // code generation is complete.
    new_addr = (new_addr == orig_addr) ? addr() : new_addr;
    return new_addr;
  }
  return MacroAssembler::pd_call_destination(addr());
}

void Relocation::pd_set_call_destination(address x) {
  if (NativeCall::is_call_at(addr())) {
    address trampoline = nativeCall_at(addr())->get_trampoline();
    if (trampoline) {
      nativeCall_at(addr())->set_destination_mt_safe(x, /* assert_lock */false);
      return;
    }
  }
  MacroAssembler::pd_patch_instruction(addr(), x);
}

address* Relocation::pd_address_in_code() {
  return (address*)(addr() + 8);
}

address Relocation::pd_get_address_from_code() {
  return MacroAssembler::pd_call_destination(addr());
}

void poll_Relocation::fix_relocation_after_move(const CodeBuffer* src, CodeBuffer* dest) {
  if (NativeInstruction::maybe_cpool_ref(addr())) {
    address old_addr = old_addr_for(addr(), src, dest);
    MacroAssembler::pd_patch_instruction(addr(), MacroAssembler::target_addr_for_insn(old_addr));
  }
}

void metadata_Relocation::pd_fix_value(address x) { }
