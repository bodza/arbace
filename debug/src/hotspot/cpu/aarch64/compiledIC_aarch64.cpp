#include "precompiled.hpp"
#include "asm/macroAssembler.inline.hpp"
#include "code/compiledIC.hpp"
#include "code/icBuffer.hpp"
#include "code/nmethod.hpp"
#include "memory/resourceArea.hpp"
#include "runtime/mutexLocker.hpp"
#include "runtime/safepoint.hpp"

// ----------------------------------------------------------------------------

#define __ _masm.
address CompiledStaticCall::emit_to_interp_stub(CodeBuffer &cbuf, address mark) {
  // Stub is fixed up when the corresponding call is converted from
  // calling compiled code to calling interpreted code.
  // mov rmethod, 0
  // jmp -4 # to self

  if (mark == NULL) {
    mark = cbuf.insts_mark();  // Get mark within main instrs section.
  }

  // Note that the code buffer's insts_mark is always relative to insts.
  // That's why we must use the macroassembler to generate a stub.
  MacroAssembler _masm(&cbuf);

  address base = __ start_a_stub(to_interp_stub_size());
  int offset = __ offset();
  if (base == NULL) {
    return NULL;  // CodeBuffer::expand failed
  }
  // static stub relocation stores the instruction address of the call
  __ relocate(static_stub_Relocation::spec(mark));

  __ mov_metadata(rmethod, (Metadata*)NULL);
  __ movptr(rscratch1, 0);
  __ br(rscratch1);

  __ end_a_stub();
  return base;
}
#undef __

int CompiledStaticCall::to_interp_stub_size() {
  return 7 * NativeInstruction::instruction_size;
}

int CompiledStaticCall::to_trampoline_stub_size() {
  // Somewhat pessimistically, we count 3 instructions here (although
  // there are only two) because we sometimes emit an alignment nop.
  // Trampoline stubs are always word aligned.
  return 3 * NativeInstruction::instruction_size + wordSize;
}

// Relocation entries for call stub, compiled java to interpreter.
int CompiledStaticCall::reloc_to_interp_stub() {
  return 4; // 3 in emit_to_interp_stub + 1 in emit_call
}

void CompiledDirectStaticCall::set_to_interpreted(const methodHandle& callee, address entry) {
  address stub = find_stub(false /* is_aot */);
  guarantee(stub != NULL, "stub not found");

  if (TraceICs) {
    ResourceMark rm;
    tty->print_cr("CompiledDirectStaticCall@" INTPTR_FORMAT ": set_to_interpreted %s",
                  p2i(instruction_address()),
                  callee->name_and_sig_as_C_string());
  }

  // Creation also verifies the object.
  NativeMovConstReg* method_holder = nativeMovConstReg_at(stub);
  // Update stub.
  method_holder->set_data((intptr_t)callee());
  NativeGeneralJump::insert_unconditional(method_holder->next_instruction_address(), entry);
  ICache::invalidate_range(stub, to_interp_stub_size());
  // Update jump to call.
  set_destination_mt_safe(stub);
}

void CompiledDirectStaticCall::set_stub_to_clean(static_stub_Relocation* static_stub) {
  // Reset stub.
  address stub = static_stub->addr();
  // Creation also verifies the object.
  NativeMovConstReg* method_holder = nativeMovConstReg_at(stub);
  method_holder->set_data(0);
}
