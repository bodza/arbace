#include "aot/compiledIC_aot.hpp"
#include "code/codeCache.hpp"
#include "memory/resourceArea.hpp"

void CompiledDirectStaticCall::set_to_far(const methodHandle& callee, address entry) {
  if (TraceICs) {
    ResourceMark rm;
    tty->print_cr("CompiledDirectStaticCall@" INTPTR_FORMAT ": set_to_far %s", p2i(instruction_address()), callee->name_and_sig_as_C_string());
  }

  set_destination_mt_safe(entry);
}

void CompiledPltStaticCall::set_to_interpreted(const methodHandle& callee, address entry) {
  address stub = find_stub();
  guarantee(stub != NULL, "stub not found");
  if (TraceICs) {
    ResourceMark rm;
    tty->print_cr("CompiledPltStaticCall@" INTPTR_FORMAT ": set_to_interpreted %s", p2i(instruction_address()), callee->name_and_sig_as_C_string());
  }

  // Creation also verifies the object.
  NativeLoadGot* method_loader = nativeLoadGot_at(stub);
  NativeGotJump* jump          = nativeGotJump_at(method_loader->next_instruction_address());

  intptr_t data = method_loader->data();
  address destination = jump->destination();

  // Update stub.
  method_loader->set_data((intptr_t)callee());
  jump->set_jump_destination(entry);

  // Update jump to call.
  set_destination_mt_safe(stub);
}

#ifdef NEVER_CALLED
void CompiledPltStaticCall::set_stub_to_clean(static_stub_Relocation* static_stub) {
  // Reset stub.
  address stub = static_stub->addr();
  // Creation also verifies the object.
  NativeLoadGot* method_loader = nativeLoadGot_at(stub);
  NativeGotJump* jump          = nativeGotJump_at(method_loader->next_instruction_address());
  method_loader->set_data(0);
  jump->set_jump_destination((address)-1);
}
#endif
