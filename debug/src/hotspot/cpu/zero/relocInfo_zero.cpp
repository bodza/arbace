#include "precompiled.hpp"
#include "asm/codeBuffer.hpp"
#include "code/relocInfo.hpp"
#include "nativeInst_zero.hpp"
#include "oops/oop.inline.hpp"
#include "runtime/safepoint.hpp"

void Relocation::pd_set_data_value(address x, intptr_t o, bool verify_only) {
  ShouldNotCallThis();
}

address Relocation::pd_call_destination(address orig_addr) {
  ShouldNotCallThis();
  return NULL;
}

void Relocation::pd_set_call_destination(address x) {
  ShouldNotCallThis();
}

address Relocation::pd_get_address_from_code() {
  ShouldNotCallThis();
  return NULL;
}

address* Relocation::pd_address_in_code() {
  ShouldNotCallThis();
  return (address *) addr();
}

void poll_Relocation::fix_relocation_after_move(const CodeBuffer* src,
                                                CodeBuffer*       dst) {
  ShouldNotCallThis();
}

void metadata_Relocation::pd_fix_value(address x) {
  ShouldNotCallThis();
}
