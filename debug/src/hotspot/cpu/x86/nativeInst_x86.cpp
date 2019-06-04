#include "precompiled.hpp"

#include "asm/macroAssembler.hpp"
#include "memory/resourceArea.hpp"
#include "nativeInst_x86.hpp"
#include "oops/oop.inline.hpp"
#include "runtime/handles.hpp"
#include "runtime/sharedRuntime.hpp"
#include "runtime/stubRoutines.hpp"
#include "utilities/ostream.hpp"
#include "c1/c1_Runtime1.hpp"

void NativeInstruction::wrote(int offset) {
  ICache::invalidate_word(addr_at(offset));
}

void NativeLoadGot::report_and_fail() const {
  tty->print_cr("Addr: " INTPTR_FORMAT, p2i(instruction_address()));
  fatal("not a indirect rip mov to rbx");
}

intptr_t NativeLoadGot::data() const {
  return *(intptr_t *) got_address();
}

address NativePltCall::destination() const {
  NativeGotJump* jump = nativeGotJump_at(plt_jump());
  return jump->destination();
}

address NativePltCall::plt_entry() const {
  return return_address() + displacement();
}

address NativePltCall::plt_jump() const {
  address entry = plt_entry();
  // Virtual PLT code has move instruction first
  if (((NativeGotJump*)entry)->is_GotJump()) {
    return entry;
  } else {
    return nativeLoadGot_at(entry)->next_instruction_address();
  }
}

address NativePltCall::plt_load_got() const {
  address entry = plt_entry();
  if (!((NativeGotJump*)entry)->is_GotJump()) {
    // Virtual PLT code has move instruction first
    return entry;
  } else {
    // Static PLT code has move instruction second (from c2i stub)
    return nativeGotJump_at(entry)->next_instruction_address();
  }
}

address NativePltCall::plt_c2i_stub() const {
  address entry = plt_load_got();
  // This method should be called only for static calls which has C2I stub.
  NativeLoadGot* load = nativeLoadGot_at(entry);
  return entry;
}

address NativePltCall::plt_resolve_call() const {
  NativeGotJump* jump = nativeGotJump_at(plt_jump());
  address entry = jump->next_instruction_address();
  if (((NativeGotJump*)entry)->is_GotJump()) {
    return entry;
  } else {
    // c2i stub 2 instructions
    entry = nativeLoadGot_at(entry)->next_instruction_address();
    return nativeGotJump_at(entry)->next_instruction_address();
  }
}

void NativePltCall::reset_to_plt_resolve_call() {
  set_destination_mt_safe(plt_resolve_call());
}

void NativePltCall::set_destination_mt_safe(address dest) {
  // rewriting the value in the GOT, it should always be aligned
  NativeGotJump* jump = nativeGotJump_at(plt_jump());
  address* got = (address *) jump->got_address();
  *got = dest;
}

void NativePltCall::set_stub_to_clean() {
  NativeLoadGot* method_loader = nativeLoadGot_at(plt_c2i_stub());
  NativeGotJump* jump          = nativeGotJump_at(method_loader->next_instruction_address());
  method_loader->set_data(0);
  jump->set_jump_destination((address)-1);
}

address NativeGotJump::destination() const {
  address *got_entry = (address *) got_address();
  return *got_entry;
}

address NativeCall::destination() const {
  // Getting the destination of a call isn't safe because that call can
  // be getting patched while you're calling this.  There's only special
  // places where this can be called but not automatically verifiable by
  // checking which locks are held.  The solution is true atomic patching
  // on x86, nyi.
  return return_address() + displacement();
}

void NativeCall::print() {
  tty->print_cr(PTR_FORMAT ": call " PTR_FORMAT, p2i(instruction_address()), p2i(destination()));
}

// Inserts a native call instruction at a given pc
void NativeCall::insert(address code_pos, address entry) {
  intptr_t disp = (intptr_t)entry - ((intptr_t)code_pos + 1 + 4);
#ifdef AMD64
  guarantee(disp == (intptr_t)(jint)disp, "must be 32-bit offset");
#endif
  *code_pos = instruction_code;
  *((int32_t *)(code_pos + 1)) = (int32_t) disp;
  ICache::invalidate_range(code_pos, instruction_size);
}

// MT-safe patching of a call instruction.
// First patches first word of instruction to two jmp's that jmps to them
// selfs (spinlock). Then patches the last byte, and then atomicly replaces
// the jmp's with the first 4 byte of the new instruction.
void NativeCall::replace_mt_safe(address instr_addr, address code_buffer) {
  NativeCall* n_call =  nativeCall_at (instr_addr); // checking that it is a call
  if (os::is_MP()) {
    guarantee((intptr_t)instr_addr % BytesPerWord == 0, "must be aligned");
  }

  // First patch dummy jmp in place
  unsigned char patch[4];
  patch[0] = 0xEB;       // jmp rel8
  patch[1] = 0xFE;       // jmp to self
  patch[2] = 0xEB;
  patch[3] = 0xFE;

  // First patch dummy jmp in place
  *(jint*)instr_addr = *(jint *)patch;

  // Invalidate.  Opteron requires a flush after every write.
  n_call->wrote(0);

  // Patch 4th byte
  instr_addr[4] = code_buffer[4];

  n_call->wrote(4);

  // Patch bytes 0-3
  *(jint*)instr_addr = *(jint *)code_buffer;

  n_call->wrote(0);
}

// Similar to replace_mt_safe, but just changes the destination.  The
// important thing is that free-running threads are able to execute this
// call instruction at all times.  If the displacement field is aligned
// we can simply rely on atomicity of 32-bit writes to make sure other threads
// will see no intermediate states.  Otherwise, the first two bytes of the
// call are guaranteed to be aligned, and can be atomically patched to a
// self-loop to guard the instruction while we change the other bytes.

// We cannot rely on locks here, since the free-running threads must run at
// full speed.
//
// Used in the runtime linkage of calls; see class CompiledIC.
// (Cf. 4506997 and 4479829, where threads witnessed garbage displacements.)
void NativeCall::set_destination_mt_safe(address dest) {
  // Both C1 and C2 should now be generating code which aligns the patched address
  // to be within a single cache line except that C1 does not do the alignment on
  // uniprocessor systems.
  bool is_aligned = ((uintptr_t)displacement_address() + 0) / cache_line_size ==
                    ((uintptr_t)displacement_address() + 3) / cache_line_size;

  guarantee(!os::is_MP() || is_aligned, "destination must be aligned");

  if (is_aligned) {
    // Simple case:  The destination lies within a single cache line.
    set_destination(dest);
  } else if ((uintptr_t)instruction_address() / cache_line_size ==
             ((uintptr_t)instruction_address() + 1) / cache_line_size) {
    // Tricky case:  The instruction prefix lies within a single cache line.
    intptr_t disp = dest - return_address();
#ifdef AMD64
    guarantee(disp == (intptr_t)(jint)disp, "must be 32-bit offset");
#endif

    int call_opcode = instruction_address()[0];

    // First patch dummy jump in place:
    {
      u_char patch_jump[2];
      patch_jump[0] = 0xEB;       // jmp rel8
      patch_jump[1] = 0xFE;       // jmp to self

      *(short*)instruction_address() = *(short*)patch_jump;
    }
    // Invalidate.  Opteron requires a flush after every write.
    wrote(0);

    // (Note: We assume any reader which has already started to read
    // the unpatched call will completely read the whole unpatched call
    // without seeing the next writes we are about to make.)

    // Next, patch the last three bytes:
    u_char patch_disp[5];
    patch_disp[0] = call_opcode;
    *(int32_t*)&patch_disp[1] = (int32_t)disp;
    for (int i = sizeof(short); i < instruction_size; i++)
      instruction_address()[i] = patch_disp[i];

    // Invalidate.  Opteron requires a flush after every write.
    wrote(sizeof(short));

    // (Note: We assume that any reader which reads the opcode we are
    // about to repatch will also read the writes we just made.)

    // Finally, overwrite the jump:
    *(short*)instruction_address() = *(short*)patch_disp;
    // Invalidate.  Opteron requires a flush after every write.
    wrote(0);

    guarantee(destination() == dest, "patch succeeded");
  } else {
    // Impossible:  One or the other must be atomically writable.
    ShouldNotReachHere();
  }
}

void NativeMovConstReg::print() {
  tty->print_cr(PTR_FORMAT ": mov reg, " INTPTR_FORMAT, p2i(instruction_address()), data());
}

//-------------------------------------------------------------------

int NativeMovRegMem::instruction_start() const {
  int off = 0;
  u_char instr_0 = ubyte_at(off);

  // See comment in Assembler::locate_operand() about VEX prefixes.
  if (instr_0 == instruction_VEX_prefix_2bytes) {
    return 2;
  }
  if (instr_0 == instruction_VEX_prefix_3bytes) {
    return 3;
  }
  if (instr_0 == instruction_EVEX_prefix_4bytes) {
    return 4;
  }

  // First check to see if we have a (prefixed or not) xor
  if (instr_0 >= instruction_prefix_wide_lo && // 0x40
      instr_0 <= instruction_prefix_wide_hi) { // 0x4f
    off++;
    instr_0 = ubyte_at(off);
  }

  if (instr_0 == instruction_code_xor) {
    off += 2;
    instr_0 = ubyte_at(off);
  }

  // Now look for the real instruction and the many prefix/size specifiers.

  if (instr_0 == instruction_operandsize_prefix ) {  // 0x66
    off++; // Not SSE instructions
    instr_0 = ubyte_at(off);
  }

  if (instr_0 == instruction_code_xmm_ss_prefix || // 0xf3
       instr_0 == instruction_code_xmm_sd_prefix) { // 0xf2
    off++;
    instr_0 = ubyte_at(off);
  }

  if (instr_0 >= instruction_prefix_wide_lo && // 0x40
       instr_0 <= instruction_prefix_wide_hi) { // 0x4f
    off++;
    instr_0 = ubyte_at(off);
  }

  if (instr_0 == instruction_extended_prefix ) {  // 0x0f
    off++;
  }

  return off;
}

address NativeMovRegMem::instruction_address() const {
  return addr_at(instruction_start());
}

address NativeMovRegMem::next_instruction_address() const {
  address ret = instruction_address() + instruction_size;
  u_char instr_0 =  *(u_char*) instruction_address();
  switch (instr_0) {
  case instruction_operandsize_prefix:

    fatal("should have skipped instruction_operandsize_prefix");
    break;

  case instruction_extended_prefix:
    fatal("should have skipped instruction_extended_prefix");
    break;

  case instruction_code_mem2reg_movslq: // 0x63
  case instruction_code_mem2reg_movzxb: // 0xB6
  case instruction_code_mem2reg_movsxb: // 0xBE
  case instruction_code_mem2reg_movzxw: // 0xB7
  case instruction_code_mem2reg_movsxw: // 0xBF
  case instruction_code_reg2mem:        // 0x89 (q/l)
  case instruction_code_mem2reg:        // 0x8B (q/l)
  case instruction_code_reg2memb:       // 0x88
  case instruction_code_mem2regb:       // 0x8a

  case instruction_code_lea:            // 0x8d

  case instruction_code_float_s:        // 0xd9 fld_s a
  case instruction_code_float_d:        // 0xdd fld_d a

  case instruction_code_xmm_load:       // 0x10
  case instruction_code_xmm_store:      // 0x11
  case instruction_code_xmm_lpd:        // 0x12
    {
      // If there is an SIB then instruction is longer than expected
      u_char mod_rm = *(u_char*)(instruction_address() + 1);
      if ((mod_rm & 7) == 0x4) {
        ret++;
      }
    }
  case instruction_code_xor:
    fatal("should have skipped xor lead in");
    break;

  default:
    fatal("not a NativeMovRegMem");
  }
  return ret;
}

int NativeMovRegMem::offset() const{
  int off = data_offset + instruction_start();
  u_char mod_rm = *(u_char*)(instruction_address() + 1);
  // nnnn(r12|rsp) isn't coded as simple mod/rm since that is
  // the encoding to use an SIB byte. Which will have the nnnn
  // field off by one byte
  if ((mod_rm & 7) == 0x4) {
    off++;
  }
  return int_at(off);
}

void NativeMovRegMem::set_offset(int x) {
  int off = data_offset + instruction_start();
  u_char mod_rm = *(u_char*)(instruction_address() + 1);
  // nnnn(r12|rsp) isn't coded as simple mod/rm since that is
  // the encoding to use an SIB byte. Which will have the nnnn
  // field off by one byte
  if ((mod_rm & 7) == 0x4) {
    off++;
  }
  set_int_at(off, x);
}

void NativeMovRegMem::print() {
  tty->print_cr(PTR_FORMAT ": mov reg, [reg + %x]", p2i(instruction_address()), offset());
}

void NativeLoadAddress::print() {
  tty->print_cr(PTR_FORMAT ": lea [reg + %x], reg", p2i(instruction_address()), offset());
}

//--------------------------------------------------------------------------------

void NativeJump::insert(address code_pos, address entry) {
  intptr_t disp = (intptr_t)entry - ((intptr_t)code_pos + 1 + 4);
#ifdef AMD64
  guarantee(disp == (intptr_t)(int32_t)disp, "must be 32-bit offset");
#endif

  *code_pos = instruction_code;
  *((int32_t*)(code_pos + 1)) = (int32_t)disp;

  ICache::invalidate_range(code_pos, instruction_size);
}

void NativeJump::check_verified_entry_alignment(address entry, address verified_entry) {
  // Patching to not_entrant can happen while activations of the method are
  // in use. The patching in that instance must happen only when certain
  // alignment restrictions are true. These guarantees check those
  // conditions.
#ifdef AMD64
  const int linesize = 64;
#else
  const int linesize = 32;
#endif

  // Must be wordSize aligned
  guarantee(((uintptr_t) verified_entry & (wordSize -1)) == 0, "illegal address for code patching 2");
  // First 5 bytes must be within the same cache line - 4827828
  guarantee((uintptr_t) verified_entry / linesize == ((uintptr_t) verified_entry + 4) / linesize, "illegal address for code patching 3");
}

// MT safe inserting of a jump over an unknown instruction sequence (used by nmethod::makeZombie)
// The problem: jmp <dest> is a 5-byte instruction. Atomical write can be only with 4 bytes.
// First patches the first word atomically to be a jump to itself.
// Then patches the last byte  and then atomically patches the first word (4-bytes),
// thus inserting the desired jump
// This code is mt-safe with the following conditions: entry point is 4 byte aligned,
// entry point is in same cache line as unverified entry point, and the instruction being
// patched is >= 5 byte (size of patch).
//
// In C2 the 5+ byte sized instruction is enforced by code in MachPrologNode::emit.
// In C1 the restriction is enforced by CodeEmitter::method_entry
// In JVMCI, the restriction is enforced by HotSpotFrameContext.enter(...)
//
void NativeJump::patch_verified_entry(address entry, address verified_entry, address dest) {
  // complete jump instruction (to be inserted) is in code_buffer;
  unsigned char code_buffer[5];
  code_buffer[0] = instruction_code;
  intptr_t disp = (intptr_t)dest - ((intptr_t)verified_entry + 1 + 4);
#ifdef AMD64
  guarantee(disp == (intptr_t)(int32_t)disp, "must be 32-bit offset");
#endif
  *(int32_t*)(code_buffer + 1) = (int32_t)disp;

  check_verified_entry_alignment(entry, verified_entry);

  // Can't call nativeJump_at() because it's asserts jump exists
  NativeJump* n_jump = (NativeJump*) verified_entry;

  //First patch dummy jmp in place

  unsigned char patch[4];
  patch[0] = 0xEB;       // jmp rel8
  patch[1] = 0xFE;       // jmp to self
  patch[2] = 0xEB;
  patch[3] = 0xFE;

  // First patch dummy jmp in place
  *(int32_t*)verified_entry = *(int32_t *)patch;

  n_jump->wrote(0);

  // Patch 5th byte (from jump instruction)
  verified_entry[4] = code_buffer[4];

  n_jump->wrote(4);

  // Patch bytes 0-3 (from jump instruction)
  *(int32_t*)verified_entry = *(int32_t *)code_buffer;
  // Invalidate.  Opteron requires a flush after every write.
  n_jump->wrote(0);
}

address NativeFarJump::jump_destination() const {
  NativeMovConstReg* mov = nativeMovConstReg_at(addr_at(0));
  return (address)mov->data();
}

void NativePopReg::insert(address code_pos, Register reg) {
  *code_pos = (u_char)(instruction_code | reg->encoding());
  ICache::invalidate_range(code_pos, instruction_size);
}

void NativeIllegalInstruction::insert(address code_pos) {
  *(short *)code_pos = instruction_code;
  ICache::invalidate_range(code_pos, instruction_size);
}

void NativeGeneralJump::insert_unconditional(address code_pos, address entry) {
  intptr_t disp = (intptr_t)entry - ((intptr_t)code_pos + 1 + 4);
#ifdef AMD64
  guarantee(disp == (intptr_t)(int32_t)disp, "must be 32-bit offset");
#endif

  *code_pos = unconditional_long_jump;
  *((int32_t *)(code_pos + 1)) = (int32_t) disp;
  ICache::invalidate_range(code_pos, instruction_size);
}

// MT-safe patching of a long jump instruction.
// First patches first word of instruction to two jmp's that jmps to them
// selfs (spinlock). Then patches the last byte, and then atomicly replaces
// the jmp's with the first 4 byte of the new instruction.
void NativeGeneralJump::replace_mt_safe(address instr_addr, address code_buffer) {
   NativeGeneralJump* n_jump =  nativeGeneralJump_at (instr_addr); // checking that it is a jump

   // Temporary code
   unsigned char patch[4];
   patch[0] = 0xEB;       // jmp rel8
   patch[1] = 0xFE;       // jmp to self
   patch[2] = 0xEB;
   patch[3] = 0xFE;

   // First patch dummy jmp in place
   *(int32_t*)instr_addr = *(int32_t *)patch;
    n_jump->wrote(0);

   // Patch 4th byte
   instr_addr[4] = code_buffer[4];

    n_jump->wrote(4);

   // Patch bytes 0-3
   *(jint*)instr_addr = *(jint *)code_buffer;

    n_jump->wrote(0);
}

address NativeGeneralJump::jump_destination() const {
  int op_code = ubyte_at(0);
  bool is_rel32off = (op_code == 0xE9 || op_code == 0x0F);
  int  offset  = (op_code == 0x0F)  ? 2 : 1;
  int  length  = offset + ((is_rel32off) ? 4 : 1);

  if (is_rel32off)
    return addr_at(0) + length + int_at(offset);
  else
    return addr_at(0) + length + sbyte_at(offset);
}
