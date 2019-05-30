#include "precompiled.hpp"

#include "aot/compiledIC_aot.hpp"
#include "code/codeCache.hpp"
#include "memory/resourceArea.hpp"

void CompiledDirectStaticCall::set_to_far(const methodHandle& callee, address entry) {
  address stub = find_stub(true /* is_far */);
  guarantee(stub != NULL, "stub not found");

  if (TraceICs) {
    ResourceMark rm;
    tty->print_cr("CompiledDirectStaticCall@" INTPTR_FORMAT ": set_to_far %s",
                  p2i(instruction_address()),
                  callee->name_and_sig_as_C_string());
  }

  // Creation also verifies the object.
  // mov rax,imm_aot_addr
  // jmp rax
  NativeMovConstReg* destination_holder = nativeMovConstReg_at(stub);

  // Update stub.
  destination_holder->set_data((intptr_t)entry);

  // Update jump to call.
  set_destination_mt_safe(stub);
}

void CompiledPltStaticCall::set_to_interpreted(const methodHandle& callee, address entry) {
  address stub = find_stub();
  guarantee(stub != NULL, "stub not found");
  if (TraceICs) {
    ResourceMark rm;
    tty->print_cr("CompiledPltStaticCall@" INTPTR_FORMAT ": set_to_interpreted %s",
                  p2i(instruction_address()),
                  callee->name_and_sig_as_C_string());
  }

  // Creation also verifies the object.
  NativeLoadGot* method_loader = nativeLoadGot_at(stub);
  NativeGotJump* jump          = nativeGotJump_at(method_loader->next_instruction_address());

  intptr_t data = method_loader->data();
  address destination = jump->destination();
  assert(data == 0 || data == (intptr_t)callee(), "a) MT-unsafe modification of inline cache");
  assert(destination == (address)-1 || destination == entry, "b) MT-unsafe modification of inline cache");

  // Update stub.
  method_loader->set_data((intptr_t)callee());
  jump->set_jump_destination(entry);

  // Update jump to call.
  set_destination_mt_safe(stub);
}

#ifdef NEVER_CALLED
void CompiledPltStaticCall::set_stub_to_clean(static_stub_Relocation* static_stub) {
  assert(CompiledIC_lock->is_locked() || SafepointSynchronize::is_at_safepoint(), "mt unsafe call");
  // Reset stub.
  address stub = static_stub->addr();
  assert(stub != NULL, "stub not found");
  // Creation also verifies the object.
  NativeLoadGot* method_loader = nativeLoadGot_at(stub);
  NativeGotJump* jump          = nativeGotJump_at(method_loader->next_instruction_address());
  method_loader->set_data(0);
  jump->set_jump_destination((address)-1);
}
#endif
