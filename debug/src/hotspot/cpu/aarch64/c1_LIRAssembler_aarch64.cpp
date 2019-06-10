#include "precompiled.hpp"

#include "asm/macroAssembler.inline.hpp"
#include "asm/assembler.hpp"
#include "c1/c1_CodeStubs.hpp"
#include "c1/c1_Compilation.hpp"
#include "c1/c1_LIRAssembler.hpp"
#include "c1/c1_MacroAssembler.hpp"
#include "c1/c1_Runtime1.hpp"
#include "c1/c1_ValueStack.hpp"
#include "ci/ciArrayKlass.hpp"
#include "ci/ciInstance.hpp"
#include "gc/shared/barrierSet.hpp"
#include "gc/shared/cardTableBarrierSet.hpp"
#include "gc/shared/collectedHeap.hpp"
#include "nativeInst_aarch64.hpp"
#include "oops/objArrayKlass.hpp"
#include "runtime/frame.inline.hpp"
#include "runtime/sharedRuntime.hpp"
#include "vmreg_aarch64.inline.hpp"

#define COMMENT(x)

NEEDS_CLEANUP // remove this definitions ?
const Register IC_Klass    = rscratch2;   // where the IC klass is cached
const Register SYNC_header = r0;   // synchronization header
const Register SHIFT_count = r0;   // where count for shift operations must be

#define __ _masm->

static void select_different_registers(Register preserve, Register extra, Register &tmp1, Register &tmp2) {
  if (tmp1 == preserve) {
    tmp1 = extra;
  } else if (tmp2 == preserve) {
    tmp2 = extra;
  }
}

static void select_different_registers(Register preserve, Register extra, Register &tmp1, Register &tmp2, Register &tmp3) {
  if (tmp1 == preserve) {
    tmp1 = extra;
  } else if (tmp2 == preserve) {
    tmp2 = extra;
  } else if (tmp3 == preserve) {
    tmp3 = extra;
  }
}

bool LIR_Assembler::is_small_constant(LIR_Opr opr) { Unimplemented(); return false; }

LIR_Opr LIR_Assembler::receiverOpr() {
  return FrameMap::receiver_opr;
}

//--------------fpu register translations-----------------------

address LIR_Assembler::float_constant(float f) {
  address const_addr = __ float_constant(f);
  if (const_addr == NULL) {
    bailout("const section overflow");
    return __ code()->consts()->start();
  } else {
    return const_addr;
  }
}

address LIR_Assembler::double_constant(double d) {
  address const_addr = __ double_constant(d);
  if (const_addr == NULL) {
    bailout("const section overflow");
    return __ code()->consts()->start();
  } else {
    return const_addr;
  }
}

address LIR_Assembler::int_constant(jlong n) {
  address const_addr = __ long_constant(n);
  if (const_addr == NULL) {
    bailout("const section overflow");
    return __ code()->consts()->start();
  } else {
    return const_addr;
  }
}

void LIR_Assembler::set_24bit_FPU() { Unimplemented(); }

void LIR_Assembler::reset_FPU() { Unimplemented(); }

void LIR_Assembler::fpop() { Unimplemented(); }

void LIR_Assembler::fxch(int i) { Unimplemented(); }

void LIR_Assembler::fld(int i) { Unimplemented(); }

void LIR_Assembler::ffree(int i) { Unimplemented(); }

void LIR_Assembler::breakpoint() { Unimplemented(); }

void LIR_Assembler::push(LIR_Opr opr) { Unimplemented(); }

void LIR_Assembler::pop(LIR_Opr opr) { Unimplemented(); }

bool LIR_Assembler::is_literal_address(LIR_Address* addr) { Unimplemented(); return false; }
//-------------------------------------------

static Register as_reg(LIR_Opr op) {
  return op->is_double_cpu() ? op->as_register_lo() : op->as_register();
}

static jlong as_long(LIR_Opr data) {
  jlong result;
  switch (data->type()) {
  case T_INT:
    result = (data->as_jint());
    break;
  case T_LONG:
    result = (data->as_jlong());
    break;
  default:
    ShouldNotReachHere();
    result = 0;  // unreachable
  }
  return result;
}

Address LIR_Assembler::as_Address(LIR_Address* addr, Register tmp) {
  Register base = addr->base()->as_pointer_register();
  LIR_Opr opr = addr->index();
  if (opr->is_cpu_register()) {
    Register index;
    if (opr->is_single_cpu())
      index = opr->as_register();
    else
      index = opr->as_register_lo();
    switch (opr->type()) {
      case T_INT:
        return Address(base, index, Address::sxtw(addr->scale()));
      case T_LONG:
        return Address(base, index, Address::lsl(addr->scale()));
      default:
        ShouldNotReachHere();
      }
  } else {
    intptr_t addr_offset = intptr_t(addr->disp());
    if (Address::offset_ok_for_immed(addr_offset, addr->scale()))
      return Address(base, addr_offset, Address::lsl(addr->scale()));
    else {
      __ mov(tmp, addr_offset);
      return Address(base, tmp, Address::lsl(addr->scale()));
    }
  }
  return Address();
}

Address LIR_Assembler::as_Address_hi(LIR_Address* addr) {
  ShouldNotReachHere();
  return Address();
}

Address LIR_Assembler::as_Address(LIR_Address* addr) {
  return as_Address(addr, rscratch1);
}

Address LIR_Assembler::as_Address_lo(LIR_Address* addr) {
  return as_Address(addr, rscratch1);  // Ouch
  // FIXME: This needs to be much more clever.  See x86.
}

// inline cache check; done before the frame is built.
int LIR_Assembler::check_icache() {
  Register receiver = FrameMap::receiver_opr->as_register();
  Register ic_klass = IC_Klass;
  int start_offset = __ offset();
  __ inline_cache_check(receiver, ic_klass);

  // if icache check fails, then jump to runtime routine
  // Note: RECEIVER must still contain the receiver!
  Label dont;
  __ br(Assembler::EQ, dont);
  __ far_jump(RuntimeAddress(SharedRuntime::get_ic_miss_stub()));

  // We align the verified entry point unless the method body
  // (including its inline cache check) will fit in a single 64-byte
  // icache line.
  if (!method()->is_accessor() || __ offset() - start_offset > 4 * 4) {
    // force alignment after the cache check.
    __ align(CodeEntryAlignment);
  }

  __ bind(dont);
  return start_offset;
}

void LIR_Assembler::jobject2reg(jobject o, Register reg) {
  if (o == NULL) {
    __ mov(reg, zr);
  } else {
    __ movoop(reg, o, /*immediate*/true);
  }
}

void LIR_Assembler::jobject2reg_with_patching(Register reg, CodeEmitInfo *info) {
  ShouldNotReachHere();
}

// This specifies the rsp decrement needed to build the frame
int LIR_Assembler::initial_frame_size_in_bytes() const {
  // if rounding, must let FrameMap know!

  // The frame_map records size in slots (32bit word)

  // subtract two words to account for return address and link
  return (frame_map()->framesize() - (2 * VMRegImpl::slots_per_word)) * VMRegImpl::stack_slot_size;
}

int LIR_Assembler::emit_exception_handler() {
  // if the last instruction is a call (typically to do a throw which
  // is coming at the end after block reordering) the return address
  // must still point into the code area in order to avoid assertion
  // failures when searching for the corresponding bci => add a nop
  // (was bug 5/14/1999 - gri)
  __ nop();

  // generate code for exception handler
  address handler_base = __ start_a_stub(exception_handler_size());
  if (handler_base == NULL) {
    // not enough space left for the handler
    bailout("exception handler overflow");
    return -1;
  }

  int offset = code_offset();

  // the exception oop and pc are in r0, and r3
  // no other registers need to be preserved, so invalidate them
  __ invalidate_registers(false, true, true, false, true, true);

  // check that there is really an exception
  __ verify_not_null_oop(r0);

  // search an exception handler (r0: exception oop, r3: throwing pc)
  __ far_call(RuntimeAddress(Runtime1::entry_for(Runtime1::handle_exception_from_callee_id)));  __ should_not_reach_here();
  guarantee(code_offset() - offset <= exception_handler_size(), "overflow");
  __ end_a_stub();

  return offset;
}

void LIR_Assembler::add_debug_info_for_branch(address adr, CodeEmitInfo* info) {
  _masm->code_section()->relocate(adr, relocInfo::poll_type);
  int pc_offset = code_offset();
  flush_debug_info(pc_offset);
  info->record_debug_info(compilation()->debug_info_recorder(), pc_offset);
  if (info->exception_handlers() != NULL) {
    compilation()->add_exception_handlers_for_pco(pc_offset, info->exception_handlers());
  }
}

void LIR_Assembler::return_op(LIR_Opr result) {
  // Pop the stack before the safepoint code
  __ remove_frame(initial_frame_size_in_bytes());

  address polling_page(os::get_polling_page());
  __ read_polling_page(rscratch1, polling_page, relocInfo::poll_return_type);
  __ ret(lr);
}

int LIR_Assembler::safepoint_poll(LIR_Opr tmp, CodeEmitInfo* info) {
  address polling_page(os::get_polling_page());
  guarantee(info != NULL, "Shouldn't be NULL");
  __ get_polling_page(rscratch1, polling_page, relocInfo::poll_type);
  add_debug_info_for_branch(info);  // This isn't just debug info: it's the oop map
  __ read_polling_page(rscratch1, relocInfo::poll_type);
  return __ offset();
}

void LIR_Assembler::move_regs(Register from_reg, Register to_reg) {
  if (from_reg == r31_sp)
    from_reg = sp;
  if (to_reg == r31_sp)
    to_reg = sp;
  __ mov(to_reg, from_reg);
}

void LIR_Assembler::swap_reg(Register a, Register b) { Unimplemented(); }

void LIR_Assembler::const2reg(LIR_Opr src, LIR_Opr dest, LIR_PatchCode patch_code, CodeEmitInfo* info) {
  LIR_Const* c = src->as_constant_ptr();

  switch (c->type()) {
    case T_INT: {
      __ movw(dest->as_register(), c->as_jint());
      break;
    }

    case T_ADDRESS: {
      __ mov(dest->as_register(), c->as_jint());
      break;
    }

    case T_LONG: {
      __ mov(dest->as_register_lo(), (intptr_t)c->as_jlong());
      break;
    }

    case T_OBJECT: {
        if (patch_code == lir_patch_none) {
          jobject2reg(c->as_jobject(), dest->as_register());
        } else {
          jobject2reg_with_patching(dest->as_register(), info);
        }
      break;
    }

    case T_METADATA: {
      if (patch_code != lir_patch_none) {
        klass2reg_with_patching(dest->as_register(), info);
      } else {
        __ mov_metadata(dest->as_register(), c->as_metadata());
      }
      break;
    }

    case T_FLOAT: {
      if (__ operand_valid_for_float_immediate(c->as_jfloat())) {
        __ fmovs(dest->as_float_reg(), (c->as_jfloat()));
      } else {
        __ adr(rscratch1, InternalAddress(float_constant(c->as_jfloat())));
        __ ldrs(dest->as_float_reg(), Address(rscratch1));
      }
      break;
    }

    case T_DOUBLE: {
      if (__ operand_valid_for_float_immediate(c->as_jdouble())) {
        __ fmovd(dest->as_double_reg(), (c->as_jdouble()));
      } else {
        __ adr(rscratch1, InternalAddress(double_constant(c->as_jdouble())));
        __ ldrd(dest->as_double_reg(), Address(rscratch1));
      }
      break;
    }

    default:
      ShouldNotReachHere();
  }
}

void LIR_Assembler::const2stack(LIR_Opr src, LIR_Opr dest) {
  LIR_Const* c = src->as_constant_ptr();
  switch (c->type()) {
  case T_OBJECT:
    {
      if (!c->as_jobject())
        __ str(zr, frame_map()->address_for_slot(dest->single_stack_ix()));
      else {
        const2reg(src, FrameMap::rscratch1_opr, lir_patch_none, NULL);
        reg2stack(FrameMap::rscratch1_opr, dest, c->type(), false);
      }
    }
    break;
  case T_ADDRESS:
    {
      const2reg(src, FrameMap::rscratch1_opr, lir_patch_none, NULL);
      reg2stack(FrameMap::rscratch1_opr, dest, c->type(), false);
    }
  case T_INT:
  case T_FLOAT:
    {
      Register reg = zr;
      if (c->as_jint_bits() == 0)
        __ strw(zr, frame_map()->address_for_slot(dest->single_stack_ix()));
      else {
        __ movw(rscratch1, c->as_jint_bits());
        __ strw(rscratch1, frame_map()->address_for_slot(dest->single_stack_ix()));
      }
    }
    break;
  case T_LONG:
  case T_DOUBLE:
    {
      Register reg = zr;
      if (c->as_jlong_bits() == 0)
        __ str(zr, frame_map()->address_for_slot(dest->double_stack_ix(), lo_word_offset_in_bytes));
      else {
        __ mov(rscratch1, (intptr_t)c->as_jlong_bits());
        __ str(rscratch1, frame_map()->address_for_slot(dest->double_stack_ix(), lo_word_offset_in_bytes));
      }
    }
    break;
  default:
    ShouldNotReachHere();
  }
}

void LIR_Assembler::const2mem(LIR_Opr src, LIR_Opr dest, BasicType type, CodeEmitInfo* info, bool wide) {
  LIR_Const* c = src->as_constant_ptr();
  LIR_Address* to_addr = dest->as_address_ptr();

  void (Assembler::* insn)(Register Rt, const Address &adr);

  switch (type) {
  case T_ADDRESS:
    insn = &Assembler::str;
    break;
  case T_LONG:
    insn = &Assembler::str;
    break;
  case T_INT:
    insn = &Assembler::strw;
    break;
  case T_OBJECT:
  case T_ARRAY:
    if (UseCompressedOops && !wide) {
      insn = &Assembler::strw;
    } else {
      insn = &Assembler::str;
    }
    break;
  case T_CHAR:
  case T_SHORT:
    insn = &Assembler::strh;
    break;
  case T_BOOLEAN:
  case T_BYTE:
    insn = &Assembler::strb;
    break;
  default:
    ShouldNotReachHere();
    insn = &Assembler::str;  // unreachable
  }

  if (info) add_debug_info_for_null_check_here(info);
  (_masm->*insn)(zr, as_Address(to_addr, rscratch1));
}

void LIR_Assembler::reg2reg(LIR_Opr src, LIR_Opr dest) {
  // move between cpu-registers
  if (dest->is_single_cpu()) {
    if (src->type() == T_LONG) {
      // Can do LONG -> OBJECT
      move_regs(src->as_register_lo(), dest->as_register());
      return;
    }
    move_regs(src->as_register(), dest->as_register());
  } else if (dest->is_double_cpu()) {
    if (src->type() == T_OBJECT || src->type() == T_ARRAY) {
      move_regs(src->as_register(), dest->as_register_lo());
      return;
    }
    Register f_lo = src->as_register_lo();
    Register f_hi = src->as_register_hi();
    Register t_lo = dest->as_register_lo();
    Register t_hi = dest->as_register_hi();
    move_regs(f_lo, t_lo);
  } else if (dest->is_single_fpu()) {
    __ fmovs(dest->as_float_reg(), src->as_float_reg());
  } else if (dest->is_double_fpu()) {
    __ fmovd(dest->as_double_reg(), src->as_double_reg());
  } else {
    ShouldNotReachHere();
  }
}

void LIR_Assembler::reg2stack(LIR_Opr src, LIR_Opr dest, BasicType type, bool pop_fpu_stack) {
  if (src->is_single_cpu()) {
    if (type == T_ARRAY || type == T_OBJECT) {
      __ str(src->as_register(), frame_map()->address_for_slot(dest->single_stack_ix()));
    } else if (type == T_METADATA || type == T_DOUBLE) {
      __ str(src->as_register(), frame_map()->address_for_slot(dest->single_stack_ix()));
    } else {
      __ strw(src->as_register(), frame_map()->address_for_slot(dest->single_stack_ix()));
    }
  } else if (src->is_double_cpu()) {
    Address dest_addr_LO = frame_map()->address_for_slot(dest->double_stack_ix(), lo_word_offset_in_bytes);
    __ str(src->as_register_lo(), dest_addr_LO);
  } else if (src->is_single_fpu()) {
    Address dest_addr = frame_map()->address_for_slot(dest->single_stack_ix());
    __ strs(src->as_float_reg(), dest_addr);
  } else if (src->is_double_fpu()) {
    Address dest_addr = frame_map()->address_for_slot(dest->double_stack_ix());
    __ strd(src->as_double_reg(), dest_addr);
  } else {
    ShouldNotReachHere();
  }
}

void LIR_Assembler::reg2mem(LIR_Opr src, LIR_Opr dest, BasicType type, LIR_PatchCode patch_code, CodeEmitInfo* info, bool pop_fpu_stack, bool wide, bool /* unaligned */) {
  LIR_Address* to_addr = dest->as_address_ptr();
  PatchingStub* patch = NULL;
  Register compressed_src = rscratch1;

  if (patch_code != lir_patch_none) {
    ShouldNotReachHere();
    return;
  }

  if (type == T_ARRAY || type == T_OBJECT) {
    if (UseCompressedOops && !wide) {
      __ encode_heap_oop(compressed_src, src->as_register());
    } else {
      compressed_src = src->as_register();
    }
  }

  int null_check_here = code_offset();
  switch (type) {
    case T_FLOAT: {
      __ strs(src->as_float_reg(), as_Address(to_addr));
      break;
    }

    case T_DOUBLE: {
      __ strd(src->as_double_reg(), as_Address(to_addr));
      break;
    }

    case T_ARRAY:   // fall through
    case T_OBJECT:  // fall through
      if (UseCompressedOops && !wide) {
        __ strw(compressed_src, as_Address(to_addr, rscratch2));
      } else {
         __ str(compressed_src, as_Address(to_addr));
      }
      break;
    case T_METADATA:
      // We get here to store a method pointer to the stack to pass to
      // a dtrace runtime call. This can't work on 64 bit with
      // compressed klass ptrs: T_METADATA can be a compressed klass
      // ptr or a 64 bit method pointer.
      ShouldNotReachHere();
      __ str(src->as_register(), as_Address(to_addr));
      break;
    case T_ADDRESS:
      __ str(src->as_register(), as_Address(to_addr));
      break;
    case T_INT:
      __ strw(src->as_register(), as_Address(to_addr));
      break;

    case T_LONG: {
      __ str(src->as_register_lo(), as_Address_lo(to_addr));
      break;
    }

    case T_BYTE:    // fall through
    case T_BOOLEAN: {
      __ strb(src->as_register(), as_Address(to_addr));
      break;
    }

    case T_CHAR:    // fall through
    case T_SHORT:
      __ strh(src->as_register(), as_Address(to_addr));
      break;

    default:
      ShouldNotReachHere();
  }
  if (info != NULL) {
    add_debug_info_for_null_check(null_check_here, info);
  }
}

void LIR_Assembler::stack2reg(LIR_Opr src, LIR_Opr dest, BasicType type) {
  if (dest->is_single_cpu()) {
    if (type == T_ARRAY || type == T_OBJECT) {
      __ ldr(dest->as_register(), frame_map()->address_for_slot(src->single_stack_ix()));
    } else if (type == T_METADATA) {
      __ ldr(dest->as_register(), frame_map()->address_for_slot(src->single_stack_ix()));
    } else {
      __ ldrw(dest->as_register(), frame_map()->address_for_slot(src->single_stack_ix()));
    }
  } else if (dest->is_double_cpu()) {
    Address src_addr_LO = frame_map()->address_for_slot(src->double_stack_ix(), lo_word_offset_in_bytes);
    __ ldr(dest->as_register_lo(), src_addr_LO);
  } else if (dest->is_single_fpu()) {
    Address src_addr = frame_map()->address_for_slot(src->single_stack_ix());
    __ ldrs(dest->as_float_reg(), src_addr);
  } else if (dest->is_double_fpu()) {
    Address src_addr = frame_map()->address_for_slot(src->double_stack_ix());
    __ ldrd(dest->as_double_reg(), src_addr);
  } else {
    ShouldNotReachHere();
  }
}

void LIR_Assembler::klass2reg_with_patching(Register reg, CodeEmitInfo* info) {
  address target = NULL;
  relocInfo::relocType reloc_type = relocInfo::none;

  switch (patching_id(info)) {
  case PatchingStub::access_field_id:
    target = Runtime1::entry_for(Runtime1::access_field_patching_id);
    reloc_type = relocInfo::section_word_type;
    break;
  case PatchingStub::load_klass_id:
    target = Runtime1::entry_for(Runtime1::load_klass_patching_id);
    reloc_type = relocInfo::metadata_type;
    break;
  case PatchingStub::load_mirror_id:
    target = Runtime1::entry_for(Runtime1::load_mirror_patching_id);
    reloc_type = relocInfo::oop_type;
    break;
  default: ShouldNotReachHere();
  }

  __ far_call(RuntimeAddress(target));
  add_call_info_here(info);
}

void LIR_Assembler::stack2stack(LIR_Opr src, LIR_Opr dest, BasicType type) {
  LIR_Opr temp;
  if (type == T_LONG || type == T_DOUBLE)
    temp = FrameMap::rscratch1_long_opr;
  else
    temp = FrameMap::rscratch1_opr;

  stack2reg(src, temp, src->type());
  reg2stack(temp, dest, dest->type(), false);
}

void LIR_Assembler::mem2reg(LIR_Opr src, LIR_Opr dest, BasicType type, LIR_PatchCode patch_code, CodeEmitInfo* info, bool wide, bool /* unaligned */) {
  LIR_Address* addr = src->as_address_ptr();
  LIR_Address* from_addr = src->as_address_ptr();

  if (patch_code != lir_patch_none) {
    ShouldNotReachHere();
    return;
  }

  if (info != NULL) {
    add_debug_info_for_null_check_here(info);
  }
  int null_check_here = code_offset();
  switch (type) {
    case T_FLOAT: {
      __ ldrs(dest->as_float_reg(), as_Address(from_addr));
      break;
    }

    case T_DOUBLE: {
      __ ldrd(dest->as_double_reg(), as_Address(from_addr));
      break;
    }

    case T_ARRAY:   // fall through
    case T_OBJECT:  // fall through
      if (UseCompressedOops && !wide) {
        __ ldrw(dest->as_register(), as_Address(from_addr));
      } else {
         __ ldr(dest->as_register(), as_Address(from_addr));
      }
      break;
    case T_METADATA:
      // We get here to store a method pointer to the stack to pass to
      // a dtrace runtime call. This can't work on 64 bit with
      // compressed klass ptrs: T_METADATA can be a compressed klass
      // ptr or a 64 bit method pointer.
      ShouldNotReachHere();
      __ ldr(dest->as_register(), as_Address(from_addr));
      break;
    case T_ADDRESS:
      // FIXME: OMG this is a horrible kludge.  Any offset from an
      // address that matches klass_offset_in_bytes() will be loaded
      // as a word, not a long.
      if (UseCompressedClassPointers && addr->disp() == oopDesc::klass_offset_in_bytes()) {
        __ ldrw(dest->as_register(), as_Address(from_addr));
      } else {
        __ ldr(dest->as_register(), as_Address(from_addr));
      }
      break;
    case T_INT:
      __ ldrw(dest->as_register(), as_Address(from_addr));
      break;

    case T_LONG: {
      __ ldr(dest->as_register_lo(), as_Address_lo(from_addr));
      break;
    }

    case T_BYTE:
      __ ldrsb(dest->as_register(), as_Address(from_addr));
      break;
    case T_BOOLEAN: {
      __ ldrb(dest->as_register(), as_Address(from_addr));
      break;
    }

    case T_CHAR:
      __ ldrh(dest->as_register(), as_Address(from_addr));
      break;
    case T_SHORT:
      __ ldrsh(dest->as_register(), as_Address(from_addr));
      break;

    default:
      ShouldNotReachHere();
  }

  if (type == T_ARRAY || type == T_OBJECT) {
    if (UseCompressedOops && !wide) {
      __ decode_heap_oop(dest->as_register());
    }
  } else if (type == T_ADDRESS && addr->disp() == oopDesc::klass_offset_in_bytes()) {
    if (UseCompressedClassPointers) {
      __ decode_klass_not_null(dest->as_register());
    }
  }
}

int LIR_Assembler::array_element_size(BasicType type) const {
  int elem_size = type2aelembytes(type);
  return exact_log2(elem_size);
}

void LIR_Assembler::arithmetic_idiv(LIR_Op3* op, bool is_irem) {
  Register Rdividend = op->in_opr1()->as_register();
  Register Rdivisor  = op->in_opr2()->as_register();
  Register Rscratch  = op->in_opr3()->as_register();
  Register Rresult   = op->result_opr()->as_register();
  int divisor = -1;

  /*
  TODO: For some reason, using the Rscratch that gets passed in is
  not possible because the register allocator does not see the tmp reg
  as used, and assignes it the same register as Rdividend. We use rscratch1
   instead.

  */

  if (Rdivisor == noreg && is_power_of_2(divisor)) {
    // convert division by a power of two into some shifts and logical operations
  }

  __ corrected_idivl(Rresult, Rdividend, Rdivisor, is_irem, rscratch1);
}

void LIR_Assembler::emit_op3(LIR_Op3* op) {
  switch (op->code()) {
  case lir_idiv:
    arithmetic_idiv(op, false);
    break;
  case lir_irem:
    arithmetic_idiv(op, true);
    break;
  case lir_fmad:
    __ fmaddd(op->result_opr()->as_double_reg(), op->in_opr1()->as_double_reg(), op->in_opr2()->as_double_reg(), op->in_opr3()->as_double_reg());
    break;
  case lir_fmaf:
    __ fmadds(op->result_opr()->as_float_reg(), op->in_opr1()->as_float_reg(), op->in_opr2()->as_float_reg(), op->in_opr3()->as_float_reg());
    break;
  default:      ShouldNotReachHere(); break;
  }
}

void LIR_Assembler::emit_opBranch(LIR_OpBranch* op) {
  if (op->cond() == lir_cond_always) {
    if (op->info() != NULL) add_debug_info_for_branch(op->info());
    __ b(*(op->label()));
  } else {
    Assembler::Condition acond;
    if (op->code() == lir_cond_float_branch) {
      bool is_unordered = (op->ublock() == op->block());
      // Assembler::EQ does not permit unordered branches, so we add
      // another branch here.  Likewise, Assembler::NE does not permit
      // ordered branches.
      if (is_unordered && op->cond() == lir_cond_equal || !is_unordered && op->cond() == lir_cond_notEqual)
        __ br(Assembler::VS, *(op->ublock()->label()));
      switch (op->cond()) {
      case lir_cond_equal:        acond = Assembler::EQ; break;
      case lir_cond_notEqual:     acond = Assembler::NE; break;
      case lir_cond_less:         acond = (is_unordered ? Assembler::LT : Assembler::LO); break;
      case lir_cond_lessEqual:    acond = (is_unordered ? Assembler::LE : Assembler::LS); break;
      case lir_cond_greaterEqual: acond = (is_unordered ? Assembler::HS : Assembler::GE); break;
      case lir_cond_greater:      acond = (is_unordered ? Assembler::HI : Assembler::GT); break;
      default:                    ShouldNotReachHere();
        acond = Assembler::EQ;  // unreachable
      }
    } else {
      switch (op->cond()) {
        case lir_cond_equal:        acond = Assembler::EQ; break;
        case lir_cond_notEqual:     acond = Assembler::NE; break;
        case lir_cond_less:         acond = Assembler::LT; break;
        case lir_cond_lessEqual:    acond = Assembler::LE; break;
        case lir_cond_greaterEqual: acond = Assembler::GE; break;
        case lir_cond_greater:      acond = Assembler::GT; break;
        case lir_cond_belowEqual:   acond = Assembler::LS; break;
        case lir_cond_aboveEqual:   acond = Assembler::HS; break;
        default:                    ShouldNotReachHere();
          acond = Assembler::EQ;  // unreachable
      }
    }
    __ br(acond,*(op->label()));
  }
}

void LIR_Assembler::emit_opConvert(LIR_OpConvert* op) {
  LIR_Opr src  = op->in_opr();
  LIR_Opr dest = op->result_opr();

  switch (op->bytecode()) {
    case Bytecodes::_i2f:
      {
        __ scvtfws(dest->as_float_reg(), src->as_register());
        break;
      }
    case Bytecodes::_i2d:
      {
        __ scvtfwd(dest->as_double_reg(), src->as_register());
        break;
      }
    case Bytecodes::_l2d:
      {
        __ scvtfd(dest->as_double_reg(), src->as_register_lo());
        break;
      }
    case Bytecodes::_l2f:
      {
        __ scvtfs(dest->as_float_reg(), src->as_register_lo());
        break;
      }
    case Bytecodes::_f2d:
      {
        __ fcvts(dest->as_double_reg(), src->as_float_reg());
        break;
      }
    case Bytecodes::_d2f:
      {
        __ fcvtd(dest->as_float_reg(), src->as_double_reg());
        break;
      }
    case Bytecodes::_i2c:
      {
        __ ubfx(dest->as_register(), src->as_register(), 0, 16);
        break;
      }
    case Bytecodes::_i2l:
      {
        __ sxtw(dest->as_register_lo(), src->as_register());
        break;
      }
    case Bytecodes::_i2s:
      {
        __ sxth(dest->as_register(), src->as_register());
        break;
      }
    case Bytecodes::_i2b:
      {
        __ sxtb(dest->as_register(), src->as_register());
        break;
      }
    case Bytecodes::_l2i:
      {
        _masm->block_comment("FIXME: This could be a no-op");
        __ uxtw(dest->as_register(), src->as_register_lo());
        break;
      }
    case Bytecodes::_d2l:
      {
        __ fcvtzd(dest->as_register_lo(), src->as_double_reg());
        break;
      }
    case Bytecodes::_f2i:
      {
        __ fcvtzsw(dest->as_register(), src->as_float_reg());
        break;
      }
    case Bytecodes::_f2l:
      {
        __ fcvtzs(dest->as_register_lo(), src->as_float_reg());
        break;
      }
    case Bytecodes::_d2i:
      {
        __ fcvtzdw(dest->as_register(), src->as_double_reg());
        break;
      }
    default: ShouldNotReachHere();
  }
}

void LIR_Assembler::emit_alloc_obj(LIR_OpAllocObj* op) {
  if (op->init_check()) {
    __ ldrb(rscratch1, Address(op->klass()->as_register(), InstanceKlass::init_state_offset()));
    __ cmpw(rscratch1, InstanceKlass::fully_initialized);
    add_debug_info_for_null_check_here(op->stub()->info());
    __ br(Assembler::NE, *op->stub()->entry());
  }
  __ allocate_object(op->obj()->as_register(),
                     op->tmp1()->as_register(),
                     op->tmp2()->as_register(),
                     op->header_size(),
                     op->object_size(),
                     op->klass()->as_register(),
                     *op->stub()->entry());
  __ bind(*op->stub()->continuation());
}

void LIR_Assembler::emit_alloc_array(LIR_OpAllocArray* op) {
  Register len =  op->len()->as_register();
  __ uxtw(len, len);

  if ((!UseFastNewObjectArray && (op->type() == T_OBJECT || op->type() == T_ARRAY)) ||
      (!UseFastNewTypeArray   && (op->type() != T_OBJECT && op->type() != T_ARRAY))) {
    __ b(*op->stub()->entry());
  } else {
    Register tmp1 = op->tmp1()->as_register();
    Register tmp2 = op->tmp2()->as_register();
    Register tmp3 = op->tmp3()->as_register();
    if (len == tmp1) {
      tmp1 = tmp3;
    } else if (len == tmp2) {
      tmp2 = tmp3;
    } else if (len == tmp3) {
      // everything is ok
    } else {
      __ mov(tmp3, len);
    }
    __ allocate_array(op->obj()->as_register(), len, tmp1, tmp2, arrayOopDesc::header_size(op->type()), array_element_size(op->type()), op->klass()->as_register(), *op->stub()->entry());
  }
  __ bind(*op->stub()->continuation());
}

void LIR_Assembler::emit_typecheck_helper(LIR_OpTypeCheck *op, Label* success, Label* failure, Label* obj_is_null) {
  // we always need a stub for the failure case.
  CodeStub* stub = op->stub();
  Register obj = op->object()->as_register();
  Register k_RInfo = op->tmp1()->as_register();
  Register klass_RInfo = op->tmp2()->as_register();
  Register dst = op->result_opr()->as_register();
  ciKlass* k = op->klass();
  Register Rtmp1 = noreg;

  Label *success_target = success;
  Label *failure_target = failure;

  if (obj == k_RInfo) {
    k_RInfo = dst;
  } else if (obj == klass_RInfo) {
    klass_RInfo = dst;
  }
  if (k->is_loaded() && !UseCompressedClassPointers) {
    select_different_registers(obj, dst, k_RInfo, klass_RInfo);
  } else {
    Rtmp1 = op->tmp3()->as_register();
    select_different_registers(obj, dst, k_RInfo, klass_RInfo, Rtmp1);
  }

  __ cbz(obj, *obj_is_null);

  if (!k->is_loaded()) {
    klass2reg_with_patching(k_RInfo, op->info_for_patch());
  } else {
    __ mov_metadata(k_RInfo, k->constant_encoding());
  }

  if (op->fast_check()) {
    // get object class
    // not a safepoint as obj null check happens earlier
    __ load_klass(rscratch1, obj);
    __ cmp( rscratch1, k_RInfo);

    __ br(Assembler::NE, *failure_target);
    // successful cast, fall through to profile or jump
  } else {
    // get object class
    // not a safepoint as obj null check happens earlier
    __ load_klass(klass_RInfo, obj);
    if (k->is_loaded()) {
      // See if we get an immediate positive hit
      __ ldr(rscratch1, Address(klass_RInfo, long(k->super_check_offset())));
      __ cmp(k_RInfo, rscratch1);
      if ((juint)in_bytes(Klass::secondary_super_cache_offset()) != k->super_check_offset()) {
        __ br(Assembler::NE, *failure_target);
        // successful cast, fall through to profile or jump
      } else {
        // See if we get an immediate positive hit
        __ br(Assembler::EQ, *success_target);
        // check for self
        __ cmp(klass_RInfo, k_RInfo);
        __ br(Assembler::EQ, *success_target);

        __ stp(klass_RInfo, k_RInfo, Address(__ pre(sp, -2 * wordSize)));
        __ far_call(RuntimeAddress(Runtime1::entry_for(Runtime1::slow_subtype_check_id)));
        __ ldr(klass_RInfo, Address(__ post(sp, 2 * wordSize)));
        // result is a boolean
        __ cbzw(klass_RInfo, *failure_target);
        // successful cast, fall through to profile or jump
      }
    } else {
      // perform the fast part of the checking logic
      __ check_klass_subtype_fast_path(klass_RInfo, k_RInfo, Rtmp1, success_target, failure_target, NULL);
      // call out-of-line instance of __ check_klass_subtype_slow_path(...):
      __ stp(klass_RInfo, k_RInfo, Address(__ pre(sp, -2 * wordSize)));
      __ far_call(RuntimeAddress(Runtime1::entry_for(Runtime1::slow_subtype_check_id)));
      __ ldp(k_RInfo, klass_RInfo, Address(__ post(sp, 2 * wordSize)));
      // result is a boolean
      __ cbz(k_RInfo, *failure_target);
      // successful cast, fall through to profile or jump
    }
  }
  __ b(*success);
}

void LIR_Assembler::emit_opTypeCheck(LIR_OpTypeCheck* op) {
  LIR_Code code = op->code();
  if (code == lir_store_check) {
    Register value = op->object()->as_register();
    Register array = op->array()->as_register();
    Register k_RInfo = op->tmp1()->as_register();
    Register klass_RInfo = op->tmp2()->as_register();
    Register Rtmp1 = op->tmp3()->as_register();

    CodeStub* stub = op->stub();

    Label done;
    Label *success_target = &done;
    Label *failure_target = stub->entry();

    __ cbz(value, done);

    add_debug_info_for_null_check_here(op->info_for_exception());
    __ load_klass(k_RInfo, array);
    __ load_klass(klass_RInfo, value);

    // get instance klass (it's already uncompressed)
    __ ldr(k_RInfo, Address(k_RInfo, ObjArrayKlass::element_klass_offset()));
    // perform the fast part of the checking logic
    __ check_klass_subtype_fast_path(klass_RInfo, k_RInfo, Rtmp1, success_target, failure_target, NULL);
    // call out-of-line instance of __ check_klass_subtype_slow_path(...):
    __ stp(klass_RInfo, k_RInfo, Address(__ pre(sp, -2 * wordSize)));
    __ far_call(RuntimeAddress(Runtime1::entry_for(Runtime1::slow_subtype_check_id)));
    __ ldp(k_RInfo, klass_RInfo, Address(__ post(sp, 2 * wordSize)));
    // result is a boolean
    __ cbzw(k_RInfo, *failure_target);
    // fall through to the success case

    __ bind(done);
  } else if (code == lir_checkcast) {
    Register obj = op->object()->as_register();
    Register dst = op->result_opr()->as_register();
    Label success;
    emit_typecheck_helper(op, &success, op->stub()->entry(), &success);
    __ bind(success);
    if (dst != obj) {
      __ mov(dst, obj);
    }
  } else if (code == lir_instanceof) {
    Register obj = op->object()->as_register();
    Register dst = op->result_opr()->as_register();
    Label success, failure, done;
    emit_typecheck_helper(op, &success, &failure, &failure);
    __ bind(failure);
    __ mov(dst, zr);
    __ b(done);
    __ bind(success);
    __ mov(dst, 1);
    __ bind(done);
  } else {
    ShouldNotReachHere();
  }
}

void LIR_Assembler::casw(Register addr, Register newval, Register cmpval) {
  __ cmpxchg(addr, cmpval, newval, Assembler::word, /* acquire*/ true, /* release*/ true, /* weak*/ false, rscratch1);
  __ cset(rscratch1, Assembler::NE);
  __ membar(__ AnyAny);
}

void LIR_Assembler::casl(Register addr, Register newval, Register cmpval) {
  __ cmpxchg(addr, cmpval, newval, Assembler::xword, /* acquire*/ true, /* release*/ true, /* weak*/ false, rscratch1);
  __ cset(rscratch1, Assembler::NE);
  __ membar(__ AnyAny);
}

void LIR_Assembler::emit_compare_and_swap(LIR_OpCompareAndSwap* op) {
  Register addr;
  if (op->addr()->is_register()) {
    addr = as_reg(op->addr());
  } else {
    LIR_Address* addr_ptr = op->addr()->as_address_ptr();
    addr = as_reg(addr_ptr->base());
  }
  Register newval = as_reg(op->new_value());
  Register cmpval = as_reg(op->cmp_value());
  Label succeed, fail, around;

  if (op->code() == lir_cas_obj) {
    if (UseCompressedOops) {
      Register t1 = op->tmp1()->as_register();
      __ encode_heap_oop(t1, cmpval);
      cmpval = t1;
      __ encode_heap_oop(rscratch2, newval);
      newval = rscratch2;
      casw(addr, newval, cmpval);
    } else {
      casl(addr, newval, cmpval);
    }
  } else if (op->code() == lir_cas_int) {
    casw(addr, newval, cmpval);
  } else {
    casl(addr, newval, cmpval);
  }
}

void LIR_Assembler::cmove(LIR_Condition condition, LIR_Opr opr1, LIR_Opr opr2, LIR_Opr result, BasicType type) {
  Assembler::Condition acond, ncond;
  switch (condition) {
  case lir_cond_equal:        acond = Assembler::EQ; ncond = Assembler::NE; break;
  case lir_cond_notEqual:     acond = Assembler::NE; ncond = Assembler::EQ; break;
  case lir_cond_less:         acond = Assembler::LT; ncond = Assembler::GE; break;
  case lir_cond_lessEqual:    acond = Assembler::LE; ncond = Assembler::GT; break;
  case lir_cond_greaterEqual: acond = Assembler::GE; ncond = Assembler::LT; break;
  case lir_cond_greater:      acond = Assembler::GT; ncond = Assembler::LE; break;
  case lir_cond_belowEqual:
  case lir_cond_aboveEqual:
  default:                    ShouldNotReachHere();
    acond = Assembler::EQ; ncond = Assembler::NE;  // unreachable
  }

  if (opr1->is_constant() && opr2->is_constant() && opr1->type() == T_INT && opr2->type() == T_INT) {
    jint val1 = opr1->as_jint();
    jint val2 = opr2->as_jint();
    if (val1 == 0 && val2 == 1) {
      __ cset(result->as_register(), ncond);
      return;
    } else if (val1 == 1 && val2 == 0) {
      __ cset(result->as_register(), acond);
      return;
    }
  }

  if (opr1->is_constant() && opr2->is_constant() && opr1->type() == T_LONG && opr2->type() == T_LONG) {
    jlong val1 = opr1->as_jlong();
    jlong val2 = opr2->as_jlong();
    if (val1 == 0 && val2 == 1) {
      __ cset(result->as_register_lo(), ncond);
      return;
    } else if (val1 == 1 && val2 == 0) {
      __ cset(result->as_register_lo(), acond);
      return;
    }
  }

  if (opr1->is_stack()) {
    stack2reg(opr1, FrameMap::rscratch1_opr, result->type());
    opr1 = FrameMap::rscratch1_opr;
  } else if (opr1->is_constant()) {
    LIR_Opr tmp = opr1->type() == T_LONG ? FrameMap::rscratch1_long_opr : FrameMap::rscratch1_opr;
    const2reg(opr1, tmp, lir_patch_none, NULL);
    opr1 = tmp;
  }

  if (opr2->is_stack()) {
    stack2reg(opr2, FrameMap::rscratch2_opr, result->type());
    opr2 = FrameMap::rscratch2_opr;
  } else if (opr2->is_constant()) {
    LIR_Opr tmp = opr2->type() == T_LONG ? FrameMap::rscratch2_long_opr : FrameMap::rscratch2_opr;
    const2reg(opr2, tmp, lir_patch_none, NULL);
    opr2 = tmp;
  }

  if (result->type() == T_LONG)
    __ csel(result->as_register_lo(), opr1->as_register_lo(), opr2->as_register_lo(), acond);
  else
    __ csel(result->as_register(), opr1->as_register(), opr2->as_register(), acond);
}

void LIR_Assembler::arith_op(LIR_Code code, LIR_Opr left, LIR_Opr right, LIR_Opr dest, CodeEmitInfo* info, bool pop_fpu_stack) {
  if (left->is_single_cpu()) {
    Register lreg = left->as_register();
    Register dreg = as_reg(dest);

    if (right->is_single_cpu()) {
      // cpu register - cpu register

      Register rreg = right->as_register();
      switch (code) {
      case lir_add: __ addw (dest->as_register(), lreg, rreg); break;
      case lir_sub: __ subw (dest->as_register(), lreg, rreg); break;
      case lir_mul: __ mulw (dest->as_register(), lreg, rreg); break;
      default:      ShouldNotReachHere();
      }
    } else if (right->is_double_cpu()) {
      Register rreg = right->as_register_lo();
      switch (code) {
      case lir_add: __ add(dreg, lreg, rreg); break;
      case lir_sub: __ sub(dreg, lreg, rreg); break;
      default: ShouldNotReachHere();
      }
    } else if (right->is_constant()) {
      // cpu register - constant
      jlong c;

      // FIXME.  This is fugly: we really need to factor all this logic.
      switch (right->type()) {
      case T_LONG:
        c = right->as_constant_ptr()->as_jlong();
        break;
      case T_INT:
      case T_ADDRESS:
        c = right->as_constant_ptr()->as_jint();
        break;
      default:
        ShouldNotReachHere();
        c = 0;  // unreachable
        break;
      }

      if (c == 0 && dreg == lreg) {
        COMMENT("effective nop elided");
        return;
      }
      switch (left->type()) {
      case T_INT:
        switch (code) {
        case lir_add: __ addw(dreg, lreg, c); break;
        case lir_sub: __ subw(dreg, lreg, c); break;
        default: ShouldNotReachHere();
        }
        break;
      case T_OBJECT:
      case T_ADDRESS:
        switch (code) {
        case lir_add: __ add(dreg, lreg, c); break;
        case lir_sub: __ sub(dreg, lreg, c); break;
        default: ShouldNotReachHere();
        }
        break;
        ShouldNotReachHere();
      }
    } else {
      ShouldNotReachHere();
    }
  } else if (left->is_double_cpu()) {
    Register lreg_lo = left->as_register_lo();

    if (right->is_double_cpu()) {
      // cpu register - cpu register
      Register rreg_lo = right->as_register_lo();
      switch (code) {
      case lir_add: __ add (dest->as_register_lo(), lreg_lo, rreg_lo); break;
      case lir_sub: __ sub (dest->as_register_lo(), lreg_lo, rreg_lo); break;
      case lir_mul: __ mul (dest->as_register_lo(), lreg_lo, rreg_lo); break;
      case lir_div: __ corrected_idivq(dest->as_register_lo(), lreg_lo, rreg_lo, false, rscratch1); break;
      case lir_rem: __ corrected_idivq(dest->as_register_lo(), lreg_lo, rreg_lo, true, rscratch1); break;
      default:
        ShouldNotReachHere();
      }
    } else if (right->is_constant()) {
      jlong c = right->as_constant_ptr()->as_jlong_bits();
      Register dreg = as_reg(dest);
      if (c == 0 && dreg == lreg_lo) {
        COMMENT("effective nop elided");
        return;
      }
      switch (code) {
        case lir_add: __ add(dreg, lreg_lo, c); break;
        case lir_sub: __ sub(dreg, lreg_lo, c); break;
        default:
          ShouldNotReachHere();
      }
    } else {
      ShouldNotReachHere();
    }
  } else if (left->is_single_fpu()) {
    switch (code) {
    case lir_add: __ fadds (dest->as_float_reg(), left->as_float_reg(), right->as_float_reg()); break;
    case lir_sub: __ fsubs (dest->as_float_reg(), left->as_float_reg(), right->as_float_reg()); break;
    case lir_mul: __ fmuls (dest->as_float_reg(), left->as_float_reg(), right->as_float_reg()); break;
    case lir_div: __ fdivs (dest->as_float_reg(), left->as_float_reg(), right->as_float_reg()); break;
    default:
      ShouldNotReachHere();
    }
  } else if (left->is_double_fpu()) {
    if (right->is_double_fpu()) {
      // cpu register - cpu register
      switch (code) {
      case lir_add: __ faddd (dest->as_double_reg(), left->as_double_reg(), right->as_double_reg()); break;
      case lir_sub: __ fsubd (dest->as_double_reg(), left->as_double_reg(), right->as_double_reg()); break;
      case lir_mul: __ fmuld (dest->as_double_reg(), left->as_double_reg(), right->as_double_reg()); break;
      case lir_div: __ fdivd (dest->as_double_reg(), left->as_double_reg(), right->as_double_reg()); break;
      default:
        ShouldNotReachHere();
      }
    } else {
      if (right->is_constant()) {
        ShouldNotReachHere();
      }
      ShouldNotReachHere();
    }
  } else if (left->is_single_stack() || left->is_address()) {
    ShouldNotReachHere();
  } else {
    ShouldNotReachHere();
  }
}

void LIR_Assembler::arith_fpu_implementation(LIR_Code code, int left_index, int right_index, int dest_index, bool pop_fpu_stack) { Unimplemented(); }

void LIR_Assembler::intrinsic_op(LIR_Code code, LIR_Opr value, LIR_Opr unused, LIR_Opr dest, LIR_Op* op) {
  switch (code) {
  case lir_abs : __ fabsd(dest->as_double_reg(), value->as_double_reg()); break;
  case lir_sqrt: __ fsqrtd(dest->as_double_reg(), value->as_double_reg()); break;
  default      : ShouldNotReachHere();
  }
}

void LIR_Assembler::logic_op(LIR_Code code, LIR_Opr left, LIR_Opr right, LIR_Opr dst) {
  Register Rleft = left->is_single_cpu() ? left->as_register() :
                                           left->as_register_lo();
   if (dst->is_single_cpu()) {
     Register Rdst = dst->as_register();
     if (right->is_constant()) {
       switch (code) {
         case lir_logic_and: __ andw (Rdst, Rleft, right->as_jint()); break;
         case lir_logic_or:  __ orrw (Rdst, Rleft, right->as_jint()); break;
         case lir_logic_xor: __ eorw (Rdst, Rleft, right->as_jint()); break;
         default: ShouldNotReachHere(); break;
       }
     } else {
       Register Rright = right->is_single_cpu() ? right->as_register() :
                                                  right->as_register_lo();
       switch (code) {
         case lir_logic_and: __ andw (Rdst, Rleft, Rright); break;
         case lir_logic_or:  __ orrw (Rdst, Rleft, Rright); break;
         case lir_logic_xor: __ eorw (Rdst, Rleft, Rright); break;
         default: ShouldNotReachHere(); break;
       }
     }
   } else {
     Register Rdst = dst->as_register_lo();
     if (right->is_constant()) {
       switch (code) {
         case lir_logic_and: __ andr (Rdst, Rleft, right->as_jlong()); break;
         case lir_logic_or:  __ orr (Rdst, Rleft, right->as_jlong()); break;
         case lir_logic_xor: __ eor (Rdst, Rleft, right->as_jlong()); break;
         default: ShouldNotReachHere(); break;
       }
     } else {
       Register Rright = right->is_single_cpu() ? right->as_register() :
                                                  right->as_register_lo();
       switch (code) {
         case lir_logic_and: __ andr (Rdst, Rleft, Rright); break;
         case lir_logic_or:  __ orr (Rdst, Rleft, Rright); break;
         case lir_logic_xor: __ eor (Rdst, Rleft, Rright); break;
         default: ShouldNotReachHere(); break;
       }
     }
   }
}

void LIR_Assembler::arithmetic_idiv(LIR_Code code, LIR_Opr left, LIR_Opr right, LIR_Opr temp, LIR_Opr result, CodeEmitInfo* info) { Unimplemented(); }

void LIR_Assembler::comp_op(LIR_Condition condition, LIR_Opr opr1, LIR_Opr opr2, LIR_Op2* op) {
  if (opr1->is_constant() && opr2->is_single_cpu()) {
    // tableswitch
    Register reg = as_reg(opr2);
    struct tableswitch &table = switches[opr1->as_constant_ptr()->as_jint()];
    __ tableswitch(reg, table._first_key, table._last_key, table._branches, table._after);
  } else if (opr1->is_single_cpu() || opr1->is_double_cpu()) {
    Register reg1 = as_reg(opr1);
    if (opr2->is_single_cpu()) {
      // cpu register - cpu register
      Register reg2 = opr2->as_register();
      if (opr1->type() == T_OBJECT || opr1->type() == T_ARRAY) {
        __ cmpoop(reg1, reg2);
      } else {
        __ cmpw(reg1, reg2);
      }
      return;
    }
    if (opr2->is_double_cpu()) {
      // cpu register - cpu register
      Register reg2 = opr2->as_register_lo();
      __ cmp(reg1, reg2);
      return;
    }

    if (opr2->is_constant()) {
      bool is_32bit = false; // width of register operand
      jlong imm;

      switch (opr2->type()) {
      case T_INT:
        imm = opr2->as_constant_ptr()->as_jint();
        is_32bit = true;
        break;
      case T_LONG:
        imm = opr2->as_constant_ptr()->as_jlong();
        break;
      case T_ADDRESS:
        imm = opr2->as_constant_ptr()->as_jint();
        break;
      case T_OBJECT:
      case T_ARRAY:
        jobject2reg(opr2->as_constant_ptr()->as_jobject(), rscratch1);
        __ cmpoop(reg1, rscratch1);
        return;
      default:
        ShouldNotReachHere();
        imm = 0;  // unreachable
        break;
      }

      if (Assembler::operand_valid_for_add_sub_immediate(imm)) {
        if (is_32bit)
          __ cmpw(reg1, imm);
        else
          __ cmp(reg1, imm);
        return;
      } else {
        __ mov(rscratch1, imm);
        if (is_32bit)
          __ cmpw(reg1, rscratch1);
        else
          __ cmp(reg1, rscratch1);
        return;
      }
    } else
      ShouldNotReachHere();
  } else if (opr1->is_single_fpu()) {
    FloatRegister reg1 = opr1->as_float_reg();
    FloatRegister reg2 = opr2->as_float_reg();
    __ fcmps(reg1, reg2);
  } else if (opr1->is_double_fpu()) {
    FloatRegister reg1 = opr1->as_double_reg();
    FloatRegister reg2 = opr2->as_double_reg();
    __ fcmpd(reg1, reg2);
  } else {
    ShouldNotReachHere();
  }
}

void LIR_Assembler::comp_fl2i(LIR_Code code, LIR_Opr left, LIR_Opr right, LIR_Opr dst, LIR_Op2* op) {
  if (code == lir_cmp_fd2i || code == lir_ucmp_fd2i) {
    bool is_unordered_less = (code == lir_ucmp_fd2i);
    if (left->is_single_fpu()) {
      __ float_cmp(true, is_unordered_less ? -1 : 1, left->as_float_reg(), right->as_float_reg(), dst->as_register());
    } else if (left->is_double_fpu()) {
      __ float_cmp(false, is_unordered_less ? -1 : 1, left->as_double_reg(), right->as_double_reg(), dst->as_register());
    } else {
      ShouldNotReachHere();
    }
  } else if (code == lir_cmp_l2i) {
    Label done;
    __ cmp(left->as_register_lo(), right->as_register_lo());
    __ mov(dst->as_register(), (u_int64_t)-1L);
    __ br(Assembler::LT, done);
    __ csinc(dst->as_register(), zr, zr, Assembler::EQ);
    __ bind(done);
  } else {
    ShouldNotReachHere();
  }
}

void LIR_Assembler::align_call(LIR_Code code) { }

void LIR_Assembler::call(LIR_OpJavaCall* op, relocInfo::relocType rtype) {
  address call = __ trampoline_call(Address(op->addr(), rtype));
  if (call == NULL) {
    bailout("trampoline stub overflow");
    return;
  }
  add_call_info(code_offset(), op->info());
}

void LIR_Assembler::ic_call(LIR_OpJavaCall* op) {
  address call = __ ic_call(op->addr());
  if (call == NULL) {
    bailout("trampoline stub overflow");
    return;
  }
  add_call_info(code_offset(), op->info());
}

/* Currently, vtable-dispatch is only enabled for sparc platforms */
void LIR_Assembler::vtable_call(LIR_OpJavaCall* op) {
  ShouldNotReachHere();
}

void LIR_Assembler::emit_static_call_stub() {
  address call_pc = __ pc();
  address stub = __ start_a_stub(call_stub_size());
  if (stub == NULL) {
    bailout("static call stub overflow");
    return;
  }

  int start = __ offset();

  __ relocate(static_stub_Relocation::spec(call_pc));
  __ mov_metadata(rmethod, (Metadata*)NULL);
  __ movptr(rscratch1, 0);
  __ br(rscratch1);

  __ end_a_stub();
}

void LIR_Assembler::throw_op(LIR_Opr exceptionPC, LIR_Opr exceptionOop, CodeEmitInfo* info) {
  // exception object is not added to oop map by LinearScan
  // (LinearScan assumes that no oops are in fixed registers)
  info->add_register_oop(exceptionOop);
  Runtime1::StubID unwind_id;

  // get current pc information
  // pc is only needed if the method has an exception handler, the unwind code does not need it.
  int pc_for_athrow_offset = __ offset();
  InternalAddress pc_for_athrow(__ pc());
  __ adr(exceptionPC->as_register(), pc_for_athrow);
  add_call_info(pc_for_athrow_offset, info); // for exception handler

  __ verify_not_null_oop(r0);
  // search an exception handler (r0: exception oop, r3: throwing pc)
  if (compilation()->has_fpu_code()) {
    unwind_id = Runtime1::handle_exception_id;
  } else {
    unwind_id = Runtime1::handle_exception_nofpu_id;
  }
  __ far_call(RuntimeAddress(Runtime1::entry_for(unwind_id)));

  // FIXME: enough room for two byte trap   ????
  __ nop();
}

void LIR_Assembler::unwind_op(LIR_Opr exceptionOop) {
  __ b(_unwind_handler_entry);
}

void LIR_Assembler::shift_op(LIR_Code code, LIR_Opr left, LIR_Opr count, LIR_Opr dest, LIR_Opr tmp) {
  Register lreg = left->is_single_cpu() ? left->as_register() : left->as_register_lo();
  Register dreg = dest->is_single_cpu() ? dest->as_register() : dest->as_register_lo();

  switch (left->type()) {
    case T_INT: {
      switch (code) {
      case lir_shl:  __ lslvw (dreg, lreg, count->as_register()); break;
      case lir_shr:  __ asrvw (dreg, lreg, count->as_register()); break;
      case lir_ushr: __ lsrvw (dreg, lreg, count->as_register()); break;
      default:
        ShouldNotReachHere();
        break;
      }
      break;
    case T_LONG:
    case T_ADDRESS:
    case T_OBJECT:
      switch (code) {
      case lir_shl:  __ lslv (dreg, lreg, count->as_register()); break;
      case lir_shr:  __ asrv (dreg, lreg, count->as_register()); break;
      case lir_ushr: __ lsrv (dreg, lreg, count->as_register()); break;
      default:
        ShouldNotReachHere();
        break;
      }
      break;
    default:
      ShouldNotReachHere();
      break;
    }
  }
}

void LIR_Assembler::shift_op(LIR_Code code, LIR_Opr left, jint count, LIR_Opr dest) {
  Register dreg = dest->is_single_cpu() ? dest->as_register() : dest->as_register_lo();
  Register lreg = left->is_single_cpu() ? left->as_register() : left->as_register_lo();

  switch (left->type()) {
    case T_INT: {
      switch (code) {
      case lir_shl:  __ lslw (dreg, lreg, count); break;
      case lir_shr:  __ asrw (dreg, lreg, count); break;
      case lir_ushr: __ lsrw (dreg, lreg, count); break;
      default:
        ShouldNotReachHere();
        break;
      }
      break;
    case T_LONG:
    case T_ADDRESS:
    case T_OBJECT:
      switch (code) {
      case lir_shl:  __ lsl (dreg, lreg, count); break;
      case lir_shr:  __ asr (dreg, lreg, count); break;
      case lir_ushr: __ lsr (dreg, lreg, count); break;
      default:
        ShouldNotReachHere();
        break;
      }
      break;
    default:
      ShouldNotReachHere();
      break;
    }
  }
}

void LIR_Assembler::store_parameter(Register r, int offset_from_rsp_in_words) {
  int offset_from_rsp_in_bytes = offset_from_rsp_in_words * BytesPerWord;
  __ str (r, Address(sp, offset_from_rsp_in_bytes));
}

void LIR_Assembler::store_parameter(jint c, int offset_from_rsp_in_words) {
  int offset_from_rsp_in_bytes = offset_from_rsp_in_words * BytesPerWord;
  __ mov (rscratch1, c);
  __ str (rscratch1, Address(sp, offset_from_rsp_in_bytes));
}

void LIR_Assembler::store_parameter(jobject o, int offset_from_rsp_in_words) {
  ShouldNotReachHere();
  int offset_from_rsp_in_bytes = offset_from_rsp_in_words * BytesPerWord;
  __ lea(rscratch1, __ constant_oop_address(o));
  __ str(rscratch1, Address(sp, offset_from_rsp_in_bytes));
}

// This code replaces a call to arraycopy; no exception may
// be thrown in this code, they must be thrown in the System.arraycopy
// activation frame; we could save some checks if this would not be the case
void LIR_Assembler::emit_arraycopy(LIR_OpArrayCopy* op) {
  ciArrayKlass* default_type = op->expected_type();
  Register src = op->src()->as_register();
  Register dst = op->dst()->as_register();
  Register src_pos = op->src_pos()->as_register();
  Register dst_pos = op->dst_pos()->as_register();
  Register length  = op->length()->as_register();
  Register tmp = op->tmp()->as_register();

  CodeStub* stub = op->stub();
  int flags = op->flags();
  BasicType basic_type = default_type != NULL ? default_type->element_type()->basic_type() : T_ILLEGAL;
  if (basic_type == T_ARRAY) basic_type = T_OBJECT;

  // if we don't know anything, just go through the generic arraycopy
  if (default_type == NULL) { // || basic_type == T_OBJECT
    Label done;

    // Save the arguments in case the generic arraycopy fails and we
    // have to fall back to the JNI stub
    __ stp(dst,     dst_pos, Address(sp, 0*BytesPerWord));
    __ stp(length,  src_pos, Address(sp, 2*BytesPerWord));
    __ str(src,              Address(sp, 4*BytesPerWord));

    address copyfunc_addr = StubRoutines::generic_arraycopy();

    // The arguments are in java calling convention so we shift them
    // to C convention
    __ mov(c_rarg0, j_rarg0);
    __ mov(c_rarg1, j_rarg1);
    __ mov(c_rarg2, j_rarg2);
    __ mov(c_rarg3, j_rarg3);
    __ mov(c_rarg4, j_rarg4);
    __ far_call(RuntimeAddress(copyfunc_addr));

    __ cbz(r0, *stub->continuation());

    // Reload values from the stack so they are where the stub
    // expects them.
    __ ldp(dst,     dst_pos, Address(sp, 0*BytesPerWord));
    __ ldp(length,  src_pos, Address(sp, 2*BytesPerWord));
    __ ldr(src,              Address(sp, 4*BytesPerWord));

    // r0 is -1^K where K == partial copied count
    __ eonw(rscratch1, r0, 0);
    // adjust length down and src/end pos up by partial copied count
    __ subw(length, length, rscratch1);
    __ addw(src_pos, src_pos, rscratch1);
    __ addw(dst_pos, dst_pos, rscratch1);
    __ b(*stub->entry());

    __ bind(*stub->continuation());
    return;
  }

  int elem_size = type2aelembytes(basic_type);
  int shift_amount;
  int scale = exact_log2(elem_size);

  Address src_length_addr = Address(src, arrayOopDesc::length_offset_in_bytes());
  Address dst_length_addr = Address(dst, arrayOopDesc::length_offset_in_bytes());
  Address src_klass_addr = Address(src, oopDesc::klass_offset_in_bytes());
  Address dst_klass_addr = Address(dst, oopDesc::klass_offset_in_bytes());

  // test for NULL
  if (flags & LIR_OpArrayCopy::src_null_check) {
    __ cbz(src, *stub->entry());
  }
  if (flags & LIR_OpArrayCopy::dst_null_check) {
    __ cbz(dst, *stub->entry());
  }

  // If the compiler was not able to prove that exact type of the source or the destination
  // of the arraycopy is an array type, check at runtime if the source or the destination is
  // an instance type.
  if (flags & LIR_OpArrayCopy::type_check) {
    if (!(flags & LIR_OpArrayCopy::LIR_OpArrayCopy::dst_objarray)) {
      __ load_klass(tmp, dst);
      __ ldrw(rscratch1, Address(tmp, in_bytes(Klass::layout_helper_offset())));
      __ cmpw(rscratch1, Klass::_lh_neutral_value);
      __ br(Assembler::GE, *stub->entry());
    }

    if (!(flags & LIR_OpArrayCopy::LIR_OpArrayCopy::src_objarray)) {
      __ load_klass(tmp, src);
      __ ldrw(rscratch1, Address(tmp, in_bytes(Klass::layout_helper_offset())));
      __ cmpw(rscratch1, Klass::_lh_neutral_value);
      __ br(Assembler::GE, *stub->entry());
    }
  }

  // check if negative
  if (flags & LIR_OpArrayCopy::src_pos_positive_check) {
    __ cmpw(src_pos, 0);
    __ br(Assembler::LT, *stub->entry());
  }
  if (flags & LIR_OpArrayCopy::dst_pos_positive_check) {
    __ cmpw(dst_pos, 0);
    __ br(Assembler::LT, *stub->entry());
  }

  if (flags & LIR_OpArrayCopy::length_positive_check) {
    __ cmpw(length, 0);
    __ br(Assembler::LT, *stub->entry());
  }

  if (flags & LIR_OpArrayCopy::src_range_check) {
    __ addw(tmp, src_pos, length);
    __ ldrw(rscratch1, src_length_addr);
    __ cmpw(tmp, rscratch1);
    __ br(Assembler::HI, *stub->entry());
  }
  if (flags & LIR_OpArrayCopy::dst_range_check) {
    __ addw(tmp, dst_pos, length);
    __ ldrw(rscratch1, dst_length_addr);
    __ cmpw(tmp, rscratch1);
    __ br(Assembler::HI, *stub->entry());
  }

  if (flags & LIR_OpArrayCopy::type_check) {
    // We don't know the array types are compatible
    if (basic_type != T_OBJECT) {
      // Simple test for basic type arrays
      if (UseCompressedClassPointers) {
        __ ldrw(tmp, src_klass_addr);
        __ ldrw(rscratch1, dst_klass_addr);
        __ cmpw(tmp, rscratch1);
      } else {
        __ ldr(tmp, src_klass_addr);
        __ ldr(rscratch1, dst_klass_addr);
        __ cmp(tmp, rscratch1);
      }
      __ br(Assembler::NE, *stub->entry());
    } else {
      // For object arrays, if src is a sub class of dst then we can
      // safely do the copy.
      Label cont, slow;

#define PUSH(r1, r2) \
      stp(r1, r2, __ pre(sp, -2 * wordSize));

#define POP(r1, r2) \
      ldp(r1, r2, __ post(sp, 2 * wordSize));

      __ PUSH(src, dst);

      __ load_klass(src, src);
      __ load_klass(dst, dst);

      __ check_klass_subtype_fast_path(src, dst, tmp, &cont, &slow, NULL);

      __ PUSH(src, dst);
      __ far_call(RuntimeAddress(Runtime1::entry_for(Runtime1::slow_subtype_check_id)));
      __ POP(src, dst);

      __ cbnz(src, cont);

      __ bind(slow);
      __ POP(src, dst);

      address copyfunc_addr = StubRoutines::checkcast_arraycopy();
      if (copyfunc_addr != NULL) { // use stub if available
        // src is not a sub class of dst so we have to do a
        // per-element check.

        int mask = LIR_OpArrayCopy::src_objarray|LIR_OpArrayCopy::dst_objarray;
        if ((flags & mask) != mask) {
          if (!(flags & LIR_OpArrayCopy::src_objarray)) {
            __ load_klass(tmp, src);
          } else if (!(flags & LIR_OpArrayCopy::dst_objarray)) {
            __ load_klass(tmp, dst);
          }
          int lh_offset = in_bytes(Klass::layout_helper_offset());
          Address klass_lh_addr(tmp, lh_offset);
          jint objArray_lh = Klass::array_layout_helper(T_OBJECT);
          __ ldrw(rscratch1, klass_lh_addr);
          __ mov(rscratch2, objArray_lh);
          __ eorw(rscratch1, rscratch1, rscratch2);
          __ cbnzw(rscratch1, *stub->entry());
        }

       // Spill because stubs can use any register they like and it's
       // easier to restore just those that we care about.
        __ stp(dst,     dst_pos, Address(sp, 0*BytesPerWord));
        __ stp(length,  src_pos, Address(sp, 2*BytesPerWord));
        __ str(src,              Address(sp, 4*BytesPerWord));

        __ lea(c_rarg0, Address(src, src_pos, Address::uxtw(scale)));
        __ add(c_rarg0, c_rarg0, arrayOopDesc::base_offset_in_bytes(basic_type));
        __ lea(c_rarg1, Address(dst, dst_pos, Address::uxtw(scale)));
        __ add(c_rarg1, c_rarg1, arrayOopDesc::base_offset_in_bytes(basic_type));
        __ uxtw(c_rarg2, length);

        __ load_klass(c_rarg4, dst);
        __ ldr(c_rarg4, Address(c_rarg4, ObjArrayKlass::element_klass_offset()));
        __ ldrw(c_rarg3, Address(c_rarg4, Klass::super_check_offset_offset()));
        __ far_call(RuntimeAddress(copyfunc_addr));

        __ cbz(r0, *stub->continuation());

        // Restore previously spilled arguments
        __ ldp(dst,     dst_pos, Address(sp, 0*BytesPerWord));
        __ ldp(length,  src_pos, Address(sp, 2*BytesPerWord));
        __ ldr(src,              Address(sp, 4*BytesPerWord));

        // return value is -1^K where K is partial copied count
        __ eonw(rscratch1, r0, zr);
        // adjust length down and src/end pos up by partial copied count
        __ subw(length, length, rscratch1);
        __ addw(src_pos, src_pos, rscratch1);
        __ addw(dst_pos, dst_pos, rscratch1);
      }

      __ b(*stub->entry());

      __ bind(cont);
      __ POP(src, dst);
    }
  }

  __ lea(c_rarg0, Address(src, src_pos, Address::uxtw(scale)));
  __ add(c_rarg0, c_rarg0, arrayOopDesc::base_offset_in_bytes(basic_type));
  __ lea(c_rarg1, Address(dst, dst_pos, Address::uxtw(scale)));
  __ add(c_rarg1, c_rarg1, arrayOopDesc::base_offset_in_bytes(basic_type));
  __ uxtw(c_rarg2, length);

  bool disjoint = (flags & LIR_OpArrayCopy::overlapping) == 0;
  bool aligned = (flags & LIR_OpArrayCopy::unaligned) == 0;
  const char *name;
  address entry = StubRoutines::select_arraycopy_function(basic_type, aligned, disjoint, name, false);

 CodeBlob *cb = CodeCache::find_blob(entry);
 if (cb) {
   __ far_call(RuntimeAddress(entry));
 } else {
   __ call_VM_leaf(entry, 3);
 }

  __ bind(*stub->continuation());
}

void LIR_Assembler::emit_lock(LIR_OpLock* op) {
  Register obj = op->obj_opr()->as_register();  // may not be an oop
  Register hdr = op->hdr_opr()->as_register();
  Register lock = op->lock_opr()->as_register();
  if (!UseFastLocking) {
    __ b(*op->stub()->entry());
  } else if (op->code() == lir_lock) {
    Register scratch = noreg;
    if (UseBiasedLocking) {
      scratch = op->scratch_opr()->as_register();
    }
    // add debug info for NullPointerException only if one is possible
    int null_check_offset = __ lock_object(hdr, obj, lock, scratch, *op->stub()->entry());
    if (op->info() != NULL) {
      add_debug_info_for_null_check(null_check_offset, op->info());
    }
    // done
  } else if (op->code() == lir_unlock) {
    __ unlock_object(hdr, obj, lock, *op->stub()->entry());
  } else {
    Unimplemented();
  }
  __ bind(*op->stub()->continuation());
}

void LIR_Assembler::emit_delay(LIR_OpDelay*) {
  Unimplemented();
}

void LIR_Assembler::monitor_address(int monitor_no, LIR_Opr dst) {
  __ lea(dst->as_register(), frame_map()->address_for_monitor_lock(monitor_no));
}

void LIR_Assembler::emit_updatecrc32(LIR_OpUpdateCRC32* op) {
  Register crc = op->crc()->as_register();
  Register val = op->val()->as_register();
  Register res = op->result_opr()->as_register();

  unsigned long offset;
  __ adrp(res, ExternalAddress(StubRoutines::crc_table_addr()), offset);
  if (offset) __ add(res, res, offset);

  __ mvnw(crc, crc); // ~crc
  __ update_byte_crc32(crc, val, res);
  __ mvnw(res, crc); // ~crc
}

void LIR_Assembler::align_backward_branch_target() { }

void LIR_Assembler::negate(LIR_Opr left, LIR_Opr dest, LIR_Opr tmp) {
  if (left->is_single_cpu()) {
    __ negw(dest->as_register(), left->as_register());
  } else if (left->is_double_cpu()) {
    __ neg(dest->as_register_lo(), left->as_register_lo());
  } else if (left->is_single_fpu()) {
    __ fnegs(dest->as_float_reg(), left->as_float_reg());
  } else {
    __ fnegd(dest->as_double_reg(), left->as_double_reg());
  }
}

void LIR_Assembler::leal(LIR_Opr addr, LIR_Opr dest, LIR_PatchCode patch_code, CodeEmitInfo* info) {
  __ lea(dest->as_register_lo(), as_Address(addr->as_address_ptr()));
}

void LIR_Assembler::rt_call(LIR_Opr result, address dest, const LIR_OprList* args, LIR_Opr tmp, CodeEmitInfo* info) {
  CodeBlob *cb = CodeCache::find_blob(dest);
  if (cb) {
    __ far_call(RuntimeAddress(dest));
  } else {
    __ mov(rscratch1, RuntimeAddress(dest));
    int len = args->length();
    int type = 0;
    if (!result->is_illegal()) {
      switch (result->type()) {
      case T_VOID:
        type = 0;
        break;
      case T_INT:
      case T_LONG:
      case T_OBJECT:
        type = 1;
        break;
      case T_FLOAT:
        type = 2;
        break;
      case T_DOUBLE:
        type = 3;
        break;
      default:
        ShouldNotReachHere();
        break;
      }
    }
    int num_gpargs = 0;
    int num_fpargs = 0;
    for (int i = 0; i < args->length(); i++) {
      LIR_Opr arg = args->at(i);
      if (arg->type() == T_FLOAT || arg->type() == T_DOUBLE) {
        num_fpargs++;
      } else {
        num_gpargs++;
      }
    }
    __ blrt(rscratch1, num_gpargs, num_fpargs, type);
  }

  if (info != NULL) {
    add_call_info_here(info);
  }
  __ maybe_isb();
}

void LIR_Assembler::volatile_move_op(LIR_Opr src, LIR_Opr dest, BasicType type, CodeEmitInfo* info) {
  if (dest->is_address() || src->is_address()) {
    move_op(src, dest, type, lir_patch_none, info,
            /*pop_fpu_stack*/false, /*unaligned*/false, /*wide*/false);
  } else {
    ShouldNotReachHere();
  }
}

#define COMMENT(x)

void LIR_Assembler::membar() {
  COMMENT("membar");
  __ membar(MacroAssembler::AnyAny);
}

void LIR_Assembler::membar_acquire() {
  __ membar(Assembler::LoadLoad|Assembler::LoadStore);
}

void LIR_Assembler::membar_release() {
  __ membar(Assembler::LoadStore|Assembler::StoreStore);
}

void LIR_Assembler::membar_loadload() {
  __ membar(Assembler::LoadLoad);
}

void LIR_Assembler::membar_storestore() {
  __ membar(MacroAssembler::StoreStore);
}

void LIR_Assembler::membar_loadstore() { __ membar(MacroAssembler::LoadStore); }

void LIR_Assembler::membar_storeload() { __ membar(MacroAssembler::StoreLoad); }

void LIR_Assembler::on_spin_wait() {
  Unimplemented();
}

void LIR_Assembler::get_thread(LIR_Opr result_reg) {
  __ mov(result_reg->as_register(), rthread);
}

void LIR_Assembler::peephole(LIR_List *lir) { }

void LIR_Assembler::atomic_op(LIR_Code code, LIR_Opr src, LIR_Opr data, LIR_Opr dest, LIR_Opr tmp_op) {
  Address addr = as_Address(src->as_address_ptr());
  BasicType type = src->type();
  bool is_oop = type == T_OBJECT || type == T_ARRAY;

  void (MacroAssembler::* add)(Register prev, RegisterOrConstant incr, Register addr);
  void (MacroAssembler::* xchg)(Register prev, Register newv, Register addr);

  switch (type) {
  case T_INT:
    xchg = &MacroAssembler::atomic_xchgalw;
    add = &MacroAssembler::atomic_addalw;
    break;
  case T_LONG:
    xchg = &MacroAssembler::atomic_xchgal;
    add = &MacroAssembler::atomic_addal;
    break;
  case T_OBJECT:
  case T_ARRAY:
    if (UseCompressedOops) {
      xchg = &MacroAssembler::atomic_xchgalw;
      add = &MacroAssembler::atomic_addalw;
    } else {
      xchg = &MacroAssembler::atomic_xchgal;
      add = &MacroAssembler::atomic_addal;
    }
    break;
  default:
    ShouldNotReachHere();
    xchg = &MacroAssembler::atomic_xchgal;
    add = &MacroAssembler::atomic_addal; // unreachable
  }

  switch (code) {
  case lir_xadd:
    {
      RegisterOrConstant inc;
      Register tmp = as_reg(tmp_op);
      Register dst = as_reg(dest);
      if (data->is_constant()) {
        inc = RegisterOrConstant(as_long(data));
      } else {
        inc = RegisterOrConstant(as_reg(data));
      }
      __ lea(tmp, addr);
      (_masm->*add)(dst, inc, tmp);
      break;
    }
  case lir_xchg:
    {
      Register tmp = tmp_op->as_register();
      Register obj = as_reg(data);
      Register dst = as_reg(dest);
      if (is_oop && UseCompressedOops) {
        __ encode_heap_oop(rscratch2, obj);
        obj = rscratch2;
      }
      __ lea(tmp, addr);
      (_masm->*xchg)(dst, obj, tmp);
      if (is_oop && UseCompressedOops) {
        __ decode_heap_oop(dst);
      }
    }
    break;
  default:
    ShouldNotReachHere();
  }
  __ membar(__ AnyAny);
}

#undef __
