#include "precompiled.hpp"

#include "asm/macroAssembler.inline.hpp"
#include "assembler_aarch64.inline.hpp"
#include "code/vtableStubs.hpp"
#include "interp_masm_aarch64.hpp"
#include "memory/resourceArea.hpp"
#include "oops/compiledICHolder.hpp"
#include "oops/instanceKlass.hpp"
#include "oops/klassVtable.hpp"
#include "runtime/sharedRuntime.hpp"
#include "vmreg_aarch64.inline.hpp"

// machine-dependent part of VtableStubs: create VtableStub of correct size and
// initialize its code

#define __ masm->

VtableStub* VtableStubs::create_vtable_stub(int vtable_index) {
  // Read "A word on VtableStub sizing" in share/code/vtableStubs.hpp for details on stub sizing.
  const int stub_code_length = code_size_limit(true);
  VtableStub* s = new(stub_code_length) VtableStub(true, vtable_index);
  // Can be NULL if there is no free space in the code cache.
  if (s == NULL) {
    return NULL;
  }

  // Count unused bytes in instruction sequences of variable size.
  // We add them to the computed buffer size in order to avoid
  // overflow in subsequently generated stubs.
  address   start_pc;
  int       slop_bytes = 0;
  int       slop_delta = 0;

  ResourceMark    rm;
  CodeBuffer      cb(s->entry_point(), stub_code_length);
  MacroAssembler* masm = new MacroAssembler(&cb);

  // get receiver klass
  address npe_addr = __ pc();
  __ load_klass(r16, j_rarg0);

  start_pc = __ pc();
  __ lookup_virtual_method(r16, vtable_index, rmethod);
  slop_delta  = 8 - (int)(__ pc() - start_pc);
  slop_bytes += slop_delta;

  // r0: receiver klass
  // rmethod: Method*
  // r2: receiver
  address ame_addr = __ pc();
  __ ldr(rscratch1, Address(rmethod, Method::from_compiled_offset()));
  __ br(rscratch1);

  masm->flush();
  bookkeeping(masm, tty, s, npe_addr, ame_addr, true, vtable_index, slop_bytes, 0);

  return s;
}

VtableStub* VtableStubs::create_itable_stub(int itable_index) {
  // Read "A word on VtableStub sizing" in share/code/vtableStubs.hpp for details on stub sizing.
  const int stub_code_length = code_size_limit(false);
  VtableStub* s = new(stub_code_length) VtableStub(false, itable_index);
  // Can be NULL if there is no free space in the code cache.
  if (s == NULL) {
    return NULL;
  }
  // Count unused bytes in instruction sequences of variable size.
  // We add them to the computed buffer size in order to avoid
  // overflow in subsequently generated stubs.
  address   start_pc;
  int       slop_bytes = 0;
  int       slop_delta = 0;

  ResourceMark    rm;
  CodeBuffer      cb(s->entry_point(), stub_code_length);
  MacroAssembler* masm = new MacroAssembler(&cb);

  // Entry arguments:
  //  rscratch2: CompiledICHolder
  //  j_rarg0: Receiver

  // Most registers are in use; we'll use r16, rmethod, r10, r11
  const Register recv_klass_reg     = r10;
  const Register holder_klass_reg   = r16; // declaring interface klass (DECC)
  const Register resolved_klass_reg = rmethod; // resolved interface klass (REFC)
  const Register temp_reg           = r11;
  const Register icholder_reg       = rscratch2;

  Label L_no_such_interface;

  __ ldr(resolved_klass_reg, Address(icholder_reg, CompiledICHolder::holder_klass_offset()));
  __ ldr(holder_klass_reg,   Address(icholder_reg, CompiledICHolder::holder_metadata_offset()));

  start_pc = __ pc();

  // get receiver klass (also an implicit null-check)
  address npe_addr = __ pc();
  __ load_klass(recv_klass_reg, j_rarg0);

  // Receiver subtype check against REFC.
  // Destroys recv_klass_reg value.
  __ lookup_interface_method(// inputs: rec. class, interface
                             recv_klass_reg, resolved_klass_reg, noreg,
                             // outputs:  scan temp. reg1, scan temp. reg2
                             recv_klass_reg, temp_reg,
                             L_no_such_interface,
                             /*return_method=*/false);

  const ptrdiff_t  typecheckSize = __ pc() - start_pc;
  start_pc = __ pc();

  // Get selected method from declaring class and itable index
  __ load_klass(recv_klass_reg, j_rarg0);   // restore recv_klass_reg
  __ lookup_interface_method(// inputs: rec. class, interface, itable index
                             recv_klass_reg, holder_klass_reg, itable_index,
                             // outputs: method, scan temp. reg
                             rmethod, temp_reg,
                             L_no_such_interface);

  const ptrdiff_t lookupSize = __ pc() - start_pc;

  // Reduce "estimate" such that "padding" does not drop below 8.
  const ptrdiff_t estimate = 152;
  const ptrdiff_t codesize = typecheckSize + lookupSize;
  slop_delta  = (int)(estimate - codesize);
  slop_bytes += slop_delta;

  // rmethod: Method*
  // j_rarg0: receiver
  address ame_addr = __ pc();
  __ ldr(rscratch1, Address(rmethod, Method::from_compiled_offset()));
  __ br(rscratch1);

  __ bind(L_no_such_interface);
  // Handle IncompatibleClassChangeError in itable stubs.
  // More detailed error message.
  // We force resolving of the call site by jumping to the "handle
  // wrong method" stub, and so let the interpreter runtime do all the
  // dirty work.
  __ far_jump(RuntimeAddress(SharedRuntime::get_handle_wrong_method_stub()));

  masm->flush();
  bookkeeping(masm, tty, s, npe_addr, ame_addr, false, itable_index, slop_bytes, 0);

  return s;
}

int VtableStub::pd_code_alignment() {
  // aarch64 cache line size is not an architected constant. We just align on 4 bytes (instruction size).
  const unsigned int icache_line_size = 4;
  return icache_line_size;
}
