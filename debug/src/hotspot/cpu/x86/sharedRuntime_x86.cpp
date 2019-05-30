#include "precompiled.hpp"
#include "asm/macroAssembler.hpp"
#include "runtime/sharedRuntime.hpp"
#include "vmreg_x86.inline.hpp"
#include "c1/c1_Runtime1.hpp"

#define __ masm->

// ---------------------------------------------------------------------------
// Object.hashCode, System.identityHashCode can pull the hashCode from the
// header word instead of doing a full VM transition once it's been computed.
// Since hashCode is usually polymorphic at call sites we can't do this
// optimization at the call site without a lot of work.
void SharedRuntime::inline_check_hashcode_from_object_header(MacroAssembler* masm,
                                 const methodHandle& method,
                                 Register obj_reg,
                                 Register result) {
  Label slowCase;

  // Unlike for Object.hashCode, System.identityHashCode is static method and
  // gets object as argument instead of the receiver.
  if (method->intrinsic_id() == vmIntrinsics::_identityHashCode) {
    Label Continue;
    // return 0 for null reference input
    __ cmpptr(obj_reg, (int32_t)NULL_WORD);
    __ jcc(Assembler::notEqual, Continue);
    __ xorptr(result, result);
    __ ret(0);
    __ bind(Continue);
  }

  __ movptr(result, Address(obj_reg, oopDesc::mark_offset_in_bytes()));

  // check if locked
  __ testptr(result, markOopDesc::unlocked_value);
  __ jcc(Assembler::zero, slowCase);

  if (UseBiasedLocking) {
    // Check if biased and fall through to runtime if so
    __ testptr(result, markOopDesc::biased_lock_bit_in_place);
    __ jcc(Assembler::notZero, slowCase);
  }

  // get hash
  // Read the header and build a mask to get its hash field.
  // Depend on hash_mask being at most 32 bits and avoid the use of hash_mask_in_place
  // because it could be larger than 32 bits in a 64-bit vm. See markOop.hpp.
  __ shrptr(result, markOopDesc::hash_shift);
  __ andptr(result, markOopDesc::hash_mask);

  // test if hashCode exists
  __ jcc(Assembler::zero, slowCase);
  __ ret(0);
  __ bind(slowCase);
}
