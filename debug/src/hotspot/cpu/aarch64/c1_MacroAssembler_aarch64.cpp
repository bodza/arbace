#include "precompiled.hpp"

#include "c1/c1_MacroAssembler.hpp"
#include "c1/c1_Runtime1.hpp"
#include "classfile/systemDictionary.hpp"
#include "gc/shared/collectedHeap.hpp"
#include "interpreter/interpreter.hpp"
#include "oops/arrayOop.hpp"
#include "oops/markOop.hpp"
#include "runtime/basicLock.hpp"
#include "runtime/biasedLocking.hpp"
#include "runtime/os.hpp"
#include "runtime/sharedRuntime.hpp"
#include "runtime/stubRoutines.hpp"

void C1_MacroAssembler::float_cmp(bool is_float, int unordered_result, FloatRegister f0, FloatRegister f1, Register result)
{
  Label done;
  if (is_float) {
    fcmps(f0, f1);
  } else {
    fcmpd(f0, f1);
  }
  if (unordered_result < 0) {
    // we want -1 for unordered or less than, 0 for equal and 1 for
    // greater than.
    cset(result, NE);  // Not equal or unordered
    cneg(result, result, LT);  // Less than or unordered
  } else {
    // we want -1 for less than, 0 for equal and 1 for unordered or
    // greater than.
    cset(result, NE);  // Not equal or unordered
    cneg(result, result, LO);  // Less than
  }
}

int C1_MacroAssembler::lock_object(Register hdr, Register obj, Register disp_hdr, Register scratch, Label& slow_case) {
  const int aligned_mask = BytesPerWord -1;
  const int hdr_offset = oopDesc::mark_offset_in_bytes();
  Label done, fail;
  int null_check_offset = -1;

  verify_oop(obj);

  // save object being locked into the BasicObjectLock
  str(obj, Address(disp_hdr, BasicObjectLock::obj_offset_in_bytes()));

  if (UseBiasedLocking) {
    null_check_offset = biased_locking_enter(disp_hdr, obj, hdr, scratch, false, done, &slow_case);
  } else {
    null_check_offset = offset();
  }

  // Load object header
  ldr(hdr, Address(obj, hdr_offset));
  // and mark it as unlocked
  orr(hdr, hdr, markOopDesc::unlocked_value);
  // save unlocked object header into the displaced header location on the stack
  str(hdr, Address(disp_hdr, 0));
  // test if object header is still the same (i.e. unlocked), and if so, store the
  // displaced header address in the object header - if it is not the same, get the
  // object header instead
  lea(rscratch2, Address(obj, hdr_offset));
  cmpxchgptr(hdr, disp_hdr, rscratch2, rscratch1, done, /*fallthough*/NULL);
  // if the object header was the same, we're done
  // if the object header was not the same, it is now in the hdr register
  // => test if it is a stack pointer into the same stack (recursive locking), i.e.:
  //
  // 1) (hdr & aligned_mask) == 0
  // 2) sp <= hdr
  // 3) hdr <= sp + page_size
  //
  // these 3 tests can be done by evaluating the following expression:
  //
  // (hdr - sp) & (aligned_mask - page_size)
  //
  // assuming both the stack pointer and page_size have their least
  // significant 2 bits cleared and page_size is a power of 2
  mov(rscratch1, sp);
  sub(hdr, hdr, rscratch1);
  ands(hdr, hdr, aligned_mask - os::vm_page_size());
  // for recursive locking, the result is zero => save it in the displaced header
  // location (NULL in the displaced hdr location indicates recursive locking)
  str(hdr, Address(disp_hdr, 0));
  // otherwise we don't care about the result and handle locking via runtime call
  cbnz(hdr, slow_case);
  // done
  bind(done);
  if (PrintBiasedLockingStatistics) {
    lea(rscratch2, ExternalAddress((address)BiasedLocking::fast_path_entry_count_addr()));
    addmw(Address(rscratch2, 0), 1, rscratch1);
  }
  return null_check_offset;
}

void C1_MacroAssembler::unlock_object(Register hdr, Register obj, Register disp_hdr, Label& slow_case) {
  const int aligned_mask = BytesPerWord -1;
  const int hdr_offset = oopDesc::mark_offset_in_bytes();
  Label done;

  if (UseBiasedLocking) {
    // load object
    ldr(obj, Address(disp_hdr, BasicObjectLock::obj_offset_in_bytes()));
    biased_locking_exit(obj, hdr, done);
  }

  // load displaced header
  ldr(hdr, Address(disp_hdr, 0));
  // if the loaded hdr is NULL we had recursive locking
  // if we had recursive locking, we are done
  cbz(hdr, done);
  if (!UseBiasedLocking) {
    // load object
    ldr(obj, Address(disp_hdr, BasicObjectLock::obj_offset_in_bytes()));
  }
  verify_oop(obj);
  // test if object header is pointing to the displaced header, and if so, restore
  // the displaced header in the object - if the object header is not pointing to
  // the displaced header, get the object header instead
  // if the object header was not pointing to the displaced header,
  // we do unlocking via runtime call
  if (hdr_offset) {
    lea(rscratch1, Address(obj, hdr_offset));
    cmpxchgptr(disp_hdr, hdr, rscratch1, rscratch2, done, &slow_case);
  } else {
    cmpxchgptr(disp_hdr, hdr, obj, rscratch2, done, &slow_case);
  }
  // done
  bind(done);
}

// Defines obj, preserves var_size_in_bytes
void C1_MacroAssembler::try_allocate(Register obj, Register var_size_in_bytes, int con_size_in_bytes, Register t1, Register t2, Label& slow_case) {
  if (UseTLAB) {
    tlab_allocate(obj, var_size_in_bytes, con_size_in_bytes, t1, t2, slow_case);
  } else {
    eden_allocate(obj, var_size_in_bytes, con_size_in_bytes, t1, slow_case);
  }
}

void C1_MacroAssembler::initialize_header(Register obj, Register klass, Register len, Register t1, Register t2) {
  if (UseBiasedLocking && !len->is_valid()) {
    ldr(t1, Address(klass, Klass::prototype_header_offset()));
  } else {
    // This assumes that all prototype bits fit in an int32_t
    mov(t1, (int32_t)(intptr_t)markOopDesc::prototype());
  }
  str(t1, Address(obj, oopDesc::mark_offset_in_bytes()));

  if (UseCompressedClassPointers) { // Take care not to kill klass
    encode_klass_not_null(t1, klass);
    strw(t1, Address(obj, oopDesc::klass_offset_in_bytes()));
  } else {
    str(klass, Address(obj, oopDesc::klass_offset_in_bytes()));
  }

  if (len->is_valid()) {
    strw(len, Address(obj, arrayOopDesc::length_offset_in_bytes()));
  } else if (UseCompressedClassPointers) {
    store_klass_gap(obj, zr);
  }
}

// preserves obj, destroys len_in_bytes
void C1_MacroAssembler::initialize_body(Register obj, Register len_in_bytes, int hdr_size_in_bytes, Register t1) {
  Label done;

  // len_in_bytes is positive and ptr sized
  subs(len_in_bytes, len_in_bytes, hdr_size_in_bytes);
  br(Assembler::EQ, done);

  // Preserve obj
  if (hdr_size_in_bytes)
    add(obj, obj, hdr_size_in_bytes);
  zero_memory(obj, len_in_bytes, t1);
  if (hdr_size_in_bytes)
    sub(obj, obj, hdr_size_in_bytes);

  bind(done);
}

void C1_MacroAssembler::allocate_object(Register obj, Register t1, Register t2, int header_size, int object_size, Register klass, Label& slow_case) {
  try_allocate(obj, noreg, object_size * BytesPerWord, t1, t2, slow_case);

  initialize_object(obj, klass, noreg, object_size * HeapWordSize, t1, t2, UseTLAB);
}

void C1_MacroAssembler::initialize_object(Register obj, Register klass, Register var_size_in_bytes, int con_size_in_bytes, Register t1, Register t2, bool is_tlab_allocated) {
  const int hdr_size_in_bytes = instanceOopDesc::header_size() * HeapWordSize;

  initialize_header(obj, klass, noreg, t1, t2);

  if (!(UseTLAB && ZeroTLAB && is_tlab_allocated)) {
     // clear rest of allocated space
     const Register index = t2;
     const int threshold = 16 * BytesPerWord;   // approximate break even point for code size (see comments below)
     if (var_size_in_bytes != noreg) {
       mov(index, var_size_in_bytes);
       initialize_body(obj, index, hdr_size_in_bytes, t1);
     } else if (con_size_in_bytes <= threshold) {
       // use explicit null stores
       int i = hdr_size_in_bytes;
       if (i < con_size_in_bytes && (con_size_in_bytes % (2 * BytesPerWord))) {
         str(zr, Address(obj, i));
         i += BytesPerWord;
       }
       for (; i < con_size_in_bytes; i += 2 * BytesPerWord)
         stp(zr, zr, Address(obj, i));
     } else if (con_size_in_bytes > hdr_size_in_bytes) {
       block_comment("zero memory");
      // use loop to null out the fields

       int words = (con_size_in_bytes - hdr_size_in_bytes) / BytesPerWord;
       mov(index,  words / 8);

       const int unroll = 8; // Number of str(zr) instructions we'll unroll
       int remainder = words % unroll;
       lea(rscratch1, Address(obj, hdr_size_in_bytes + remainder * BytesPerWord));

       Label entry_point, loop;
       b(entry_point);

       bind(loop);
       sub(index, index, 1);
       for (int i = -unroll; i < 0; i++) {
         if (-i == remainder)
           bind(entry_point);
         str(zr, Address(rscratch1, i * wordSize));
       }
       if (remainder == 0)
         bind(entry_point);
       add(rscratch1, rscratch1, unroll * wordSize);
       cbnz(index, loop);
     }
  }

  membar(StoreStore);

  verify_oop(obj);
}
void C1_MacroAssembler::allocate_array(Register obj, Register len, Register t1, Register t2, int header_size, int f, Register klass, Label& slow_case) {
  // check for negative or excessive length
  mov(rscratch1, (int32_t)max_array_allocation_length);
  cmp(len, rscratch1);
  br(Assembler::HS, slow_case);

  const Register arr_size = t2; // okay to be the same
  // align object end
  mov(arr_size, (int32_t)header_size * BytesPerWord + MinObjAlignmentInBytesMask);
  add(arr_size, arr_size, len, ext::uxtw, f);
  andr(arr_size, arr_size, ~MinObjAlignmentInBytesMask);

  try_allocate(obj, arr_size, 0, t1, t2, slow_case);

  initialize_header(obj, klass, len, t1, t2);

  // clear rest of allocated space
  const Register len_zero = len;
  initialize_body(obj, arr_size, header_size * BytesPerWord, len_zero);

  membar(StoreStore);

  verify_oop(obj);
}

void C1_MacroAssembler::inline_cache_check(Register receiver, Register iCache) {
  verify_oop(receiver);

  cmp_klass(receiver, iCache, rscratch1);
}

void C1_MacroAssembler::build_frame(int framesize, int bang_size_in_bytes) {
  // If we have to make this method not-entrant we'll overwrite its
  // first instruction with a jump.  For this action to be legal we
  // must ensure that this first instruction is a B, BL, NOP, BKPT,
  // SVC, HVC, or SMC.  Make it a NOP.
  nop();
  // Make sure there is enough stack space for this method's activation.
  // Note that we do this before doing an enter().
  generate_stack_overflow_check(bang_size_in_bytes);
  MacroAssembler::build_frame(framesize + 2 * wordSize);
  if (NotifySimulator) {
    notify(Assembler::method_entry);
  }
}

void C1_MacroAssembler::remove_frame(int framesize) {
  MacroAssembler::remove_frame(framesize + 2 * wordSize);
  if (NotifySimulator) {
    notify(Assembler::method_reentry);
  }
}

void C1_MacroAssembler::verified_entry() { }

void C1_MacroAssembler::load_parameter(int offset_in_words, Register reg) {
  // rbp, + 0: link
  //     + 1: return address
  //     + 2: argument with offset 0
  //     + 3: argument with offset 1
  //     + 4: ...

  ldr(reg, Address(rfp, (offset_in_words + 2) * BytesPerWord));
}
