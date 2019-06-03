#include "precompiled.hpp"

#include "interp_masm_x86.hpp"
#include "interpreter/interpreter.hpp"
#include "interpreter/interpreterRuntime.hpp"
#include "oops/arrayOop.hpp"
#include "oops/markOop.hpp"
#include "oops/methodData.hpp"
#include "oops/method.hpp"
#include "runtime/basicLock.hpp"
#include "runtime/biasedLocking.hpp"
#include "runtime/frame.inline.hpp"
#include "runtime/safepointMechanism.hpp"
#include "runtime/sharedRuntime.hpp"
#include "runtime/thread.inline.hpp"

// Implementation of InterpreterMacroAssembler

void InterpreterMacroAssembler::jump_to_entry(address entry) {
  jump(RuntimeAddress(entry));
}

void InterpreterMacroAssembler::profile_obj_type(Register obj, const Address& mdo_addr) {
  Label update, next, none;

  verify_oop(obj);

  testptr(obj, obj);
  jccb(Assembler::notZero, update);
  orptr(mdo_addr, TypeEntries::null_seen);
  jmpb(next);

  bind(update);
  load_klass(obj, obj);

  xorptr(obj, mdo_addr);
  testptr(obj, TypeEntries::type_klass_mask);
  jccb(Assembler::zero, next); // klass seen before, nothing to
                               // do. The unknown bit may have been
                               // set already but no need to check.

  testptr(obj, TypeEntries::type_unknown);
  jccb(Assembler::notZero, next); // already unknown. Nothing to do anymore.

  cmpptr(mdo_addr, 0);
  jccb(Assembler::equal, none);
  cmpptr(mdo_addr, TypeEntries::null_seen);
  jccb(Assembler::equal, none);
  // There is a chance that the checks above (re-reading profiling
  // data from memory) fail if another thread has just set the
  // profiling to this obj's klass
  xorptr(obj, mdo_addr);
  testptr(obj, TypeEntries::type_klass_mask);
  jccb(Assembler::zero, next);

  // different than before. Cannot keep accurate profile.
  orptr(mdo_addr, TypeEntries::type_unknown);
  jmpb(next);

  bind(none);
  // first time here. Set profile type.
  movptr(mdo_addr, obj);

  bind(next);
}

void InterpreterMacroAssembler::profile_arguments_type(Register mdp, Register callee, Register tmp, bool is_virtual) {
  if (!ProfileInterpreter) {
    return;
  }

  if (MethodData::profile_arguments() || MethodData::profile_return()) {
    Label profile_continue;

    test_method_data_pointer(mdp, profile_continue);

    int off_to_start = is_virtual ? in_bytes(VirtualCallData::virtual_call_data_size()) : in_bytes(CounterData::counter_data_size());

    cmpb(Address(mdp, in_bytes(DataLayout::tag_offset()) - off_to_start), is_virtual ? DataLayout::virtual_call_type_data_tag : DataLayout::call_type_data_tag);
    jcc(Assembler::notEqual, profile_continue);

    if (MethodData::profile_arguments()) {
      Label done;
      int off_to_args = in_bytes(TypeEntriesAtCall::args_data_offset());
      addptr(mdp, off_to_args);

      for (int i = 0; i < TypeProfileArgsLimit; i++) {
        if (i > 0 || MethodData::profile_return()) {
          // If return value type is profiled we may have no argument to profile
          movptr(tmp, Address(mdp, in_bytes(TypeEntriesAtCall::cell_count_offset())-off_to_args));
          subl(tmp, i*TypeStackSlotEntries::per_arg_count());
          cmpl(tmp, TypeStackSlotEntries::per_arg_count());
          jcc(Assembler::less, done);
        }
        movptr(tmp, Address(callee, Method::const_offset()));
        load_unsigned_short(tmp, Address(tmp, ConstMethod::size_of_parameters_offset()));
        // stack offset o (zero based) from the start of the argument
        // list, for n arguments translates into offset n - o - 1 from
        // the end of the argument list
        subptr(tmp, Address(mdp, in_bytes(TypeEntriesAtCall::stack_slot_offset(i))-off_to_args));
        subl(tmp, 1);
        Address arg_addr = argument_address(tmp);
        movptr(tmp, arg_addr);

        Address mdo_arg_addr(mdp, in_bytes(TypeEntriesAtCall::argument_type_offset(i))-off_to_args);
        profile_obj_type(tmp, mdo_arg_addr);

        int to_add = in_bytes(TypeStackSlotEntries::per_arg_size());
        addptr(mdp, to_add);
        off_to_args += to_add;
      }

      if (MethodData::profile_return()) {
        movptr(tmp, Address(mdp, in_bytes(TypeEntriesAtCall::cell_count_offset())-off_to_args));
        subl(tmp, TypeProfileArgsLimit*TypeStackSlotEntries::per_arg_count());
      }

      bind(done);

      if (MethodData::profile_return()) {
        // We're right after the type profile for the last
        // argument. tmp is the number of cells left in the
        // CallTypeData/VirtualCallTypeData to reach its end. Non null
        // if there's a return to profile.
        shll(tmp, exact_log2(DataLayout::cell_size));
        addptr(mdp, tmp);
      }
      movptr(Address(rbp, frame::interpreter_frame_mdp_offset * wordSize), mdp);
    } else {
      update_mdp_by_constant(mdp, in_bytes(TypeEntriesAtCall::return_only_size()));
    }

    // mdp points right after the end of the
    // CallTypeData/VirtualCallTypeData, right after the cells for the
    // return value type if there's one

    bind(profile_continue);
  }
}

void InterpreterMacroAssembler::profile_return_type(Register mdp, Register ret, Register tmp) {
  if (ProfileInterpreter && MethodData::profile_return()) {
    Label profile_continue, done;

    test_method_data_pointer(mdp, profile_continue);

    if (MethodData::profile_return_jsr292_only()) {
      // If we don't profile all invoke bytecodes we must make sure
      // it's a bytecode we indeed profile. We can't go back to the
      // begining of the ProfileData we intend to update to check its
      // type because we're right after it and we don't known its
      // length
      Label do_profile;
      cmpb(Address(_bcp_register, 0), Bytecodes::_invokedynamic);
      jcc(Assembler::equal, do_profile);
      cmpb(Address(_bcp_register, 0), Bytecodes::_invokehandle);
      jcc(Assembler::equal, do_profile);
      get_method(tmp);
      cmpw(Address(tmp, Method::intrinsic_id_offset_in_bytes()), vmIntrinsics::_compiledLambdaForm);
      jcc(Assembler::notEqual, profile_continue);

      bind(do_profile);
    }

    Address mdo_ret_addr(mdp, -in_bytes(ReturnTypeEntry::size()));
    mov(tmp, ret);
    profile_obj_type(tmp, mdo_ret_addr);

    bind(profile_continue);
  }
}

void InterpreterMacroAssembler::profile_parameters_type(Register mdp, Register tmp1, Register tmp2) {
  if (ProfileInterpreter && MethodData::profile_parameters()) {
    Label profile_continue, done;

    test_method_data_pointer(mdp, profile_continue);

    // Load the offset of the area within the MDO used for
    // parameters. If it's negative we're not profiling any parameters
    movl(tmp1, Address(mdp, in_bytes(MethodData::parameters_type_data_di_offset()) - in_bytes(MethodData::data_offset())));
    testl(tmp1, tmp1);
    jcc(Assembler::negative, profile_continue);

    // Compute a pointer to the area for parameters from the offset
    // and move the pointer to the slot for the last
    // parameters. Collect profiling from last parameter down.
    // mdo start + parameters offset + array length - 1
    addptr(mdp, tmp1);
    movptr(tmp1, Address(mdp, ArrayData::array_len_offset()));
    decrement(tmp1, TypeStackSlotEntries::per_arg_count());

    Label loop;
    bind(loop);

    int off_base = in_bytes(ParametersTypeData::stack_slot_offset(0));
    int type_base = in_bytes(ParametersTypeData::type_offset(0));
    Address::ScaleFactor per_arg_scale = Address::times(DataLayout::cell_size);
    Address arg_off(mdp, tmp1, per_arg_scale, off_base);
    Address arg_type(mdp, tmp1, per_arg_scale, type_base);

    // load offset on the stack from the slot for this parameter
    movptr(tmp2, arg_off);
    negptr(tmp2);
    // read the parameter from the local area
    movptr(tmp2, Address(_locals_register, tmp2, Interpreter::stackElementScale()));

    // profile the parameter
    profile_obj_type(tmp2, arg_type);

    // go to next parameter
    decrement(tmp1, TypeStackSlotEntries::per_arg_count());
    jcc(Assembler::positive, loop);

    bind(profile_continue);
  }
}

void InterpreterMacroAssembler::call_VM_leaf_base(address entry_point, int number_of_arguments) {
  // interpreter specific
  //
  // Note: No need to save/restore bcp & locals registers
  //       since these are callee saved registers and no blocking/
  //       GC can happen in leaf calls.
  // Further Note: DO NOT save/restore bcp/locals. If a caller has
  // already saved them so that it can use rsi/rdi as temporaries
  // then a save/restore here will DESTROY the copy the caller
  // saved! There used to be a save_bcp() that only happened in
  // the ASSERT path (no restore_bcp). Which caused bizarre failures
  // when jvm built with ASSERTs.
  // super call
  MacroAssembler::call_VM_leaf_base(entry_point, number_of_arguments);
  // interpreter specific
  // LP64: Used to ASSERT that r13/r14 were equal to frame's bcp/locals
  // but since they may not have been saved (and we don't want to
  // save them here (see note above) the assert is invalid.
}

void InterpreterMacroAssembler::call_VM_base(Register oop_result, Register java_thread, Register last_java_sp, address entry_point, int number_of_arguments, bool check_exceptions) {
  // interpreter specific
  //
  // Note: Could avoid restoring locals ptr (callee saved) - however doesn't
  //       really make a difference for these runtime calls, since they are
  //       slow anyway. Btw., bcp must be saved/restored since it may change
  //       due to GC.
  save_bcp();
  // super call
  MacroAssembler::call_VM_base(oop_result, noreg, last_java_sp, entry_point, number_of_arguments, check_exceptions);
  // interpreter specific
  restore_bcp();
  restore_locals();
}

void InterpreterMacroAssembler::get_unsigned_2_byte_index_at_bcp(Register reg, int bcp_offset) {
  load_unsigned_short(reg, Address(_bcp_register, bcp_offset));
  bswapl(reg);
  shrl(reg, 16);
}

void InterpreterMacroAssembler::get_cache_index_at_bcp(Register index, int bcp_offset, size_t index_size) {
  if (index_size == sizeof(u2)) {
    load_unsigned_short(index, Address(_bcp_register, bcp_offset));
  } else if (index_size == sizeof(u4)) {
    movl(index, Address(_bcp_register, bcp_offset));
    notl(index);  // convert to plain index
  } else if (index_size == sizeof(u1)) {
    load_unsigned_byte(index, Address(_bcp_register, bcp_offset));
  } else {
    ShouldNotReachHere();
  }
}

void InterpreterMacroAssembler::get_cache_and_index_at_bcp(Register cache, Register index, int bcp_offset, size_t index_size) {
  get_cache_index_at_bcp(index, bcp_offset, index_size);
  movptr(cache, Address(rbp, frame::interpreter_frame_cache_offset * wordSize));
  shll(index, 2);
}

void InterpreterMacroAssembler::get_cache_and_index_and_bytecode_at_bcp(Register cache, Register index, Register bytecode, int byte_no, int bcp_offset, size_t index_size) {
  get_cache_and_index_at_bcp(cache, index, bcp_offset, index_size);
  // We use a 32-bit load here since the layout of 64-bit words on
  // little-endian machines allow us that.
  movl(bytecode, Address(cache, index, Address::times_ptr, ConstantPoolCache::base_offset() + ConstantPoolCacheEntry::indices_offset()));
  const int shift_count = (1 + byte_no) * BitsPerByte;
  shrl(bytecode, shift_count);
  andl(bytecode, ConstantPoolCacheEntry::bytecode_1_mask);
}

void InterpreterMacroAssembler::get_cache_entry_pointer_at_bcp(Register cache, Register tmp, int bcp_offset, size_t index_size) {
  get_cache_index_at_bcp(tmp, bcp_offset, index_size);
  shll(tmp, 2 + LogBytesPerWord);
  movptr(cache, Address(rbp, frame::interpreter_frame_cache_offset * wordSize));
  // skip past the header
  addptr(cache, in_bytes(ConstantPoolCache::base_offset()));
  addptr(cache, tmp);  // construct pointer to cache entry
}

// Load object from cpool->resolved_references(index)
void InterpreterMacroAssembler::load_resolved_reference_at_index(Register result, Register index, Register tmp) {
  get_constant_pool(result);
  // load pointer for resolved_references[] objArray
  movptr(result, Address(result, ConstantPool::cache_offset_in_bytes()));
  movptr(result, Address(result, ConstantPoolCache::resolved_references_offset_in_bytes()));
  resolve_oop_handle(result, tmp);
  load_heap_oop(result, Address(result, index, UseCompressedOops ? Address::times_4 : Address::times_ptr, arrayOopDesc::base_offset_in_bytes(T_OBJECT)), tmp);
}

// load cpool->resolved_klass_at(index)
void InterpreterMacroAssembler::load_resolved_klass_at_index(Register cpool, Register index, Register klass) {
  movw(index, Address(cpool, index, Address::times_ptr, sizeof(ConstantPool)));
  Register resolved_klasses = cpool;
  movptr(resolved_klasses, Address(cpool, ConstantPool::resolved_klasses_offset_in_bytes()));
  movptr(klass, Address(resolved_klasses, index, Address::times_ptr, Array<Klass*>::base_offset_in_bytes()));
}

// Generate a subtype check: branch to ok_is_subtype if sub_klass is a
// subtype of super_klass.
//
// Args:
//      rax: superklass
//      Rsub_klass: subklass
//
// Kills:
//      rcx, rdi
void InterpreterMacroAssembler::gen_subtype_check(Register Rsub_klass, Label& ok_is_subtype) {
  // Profile the not-null value's klass.
  profile_typecheck(rcx, Rsub_klass, rdi); // blows rcx, reloads rdi

  // Do the check.
  check_klass_subtype(Rsub_klass, rax, rcx, ok_is_subtype); // blows rcx

  // Profile the failure of the check.
  profile_typecheck_failed(rcx); // blows rcx
}

// Java Expression Stack

void InterpreterMacroAssembler::pop_ptr(Register r) {
  pop(r);
}

void InterpreterMacroAssembler::push_ptr(Register r) {
  push(r);
}

void InterpreterMacroAssembler::push_i(Register r) {
  push(r);
}

void InterpreterMacroAssembler::push_f(XMMRegister r) {
  subptr(rsp, wordSize);
  movflt(Address(rsp, 0), r);
}

void InterpreterMacroAssembler::pop_f(XMMRegister r) {
  movflt(r, Address(rsp, 0));
  addptr(rsp, wordSize);
}

void InterpreterMacroAssembler::push_d(XMMRegister r) {
  subptr(rsp, 2 * wordSize);
  movdbl(Address(rsp, 0), r);
}

void InterpreterMacroAssembler::pop_d(XMMRegister r) {
  movdbl(r, Address(rsp, 0));
  addptr(rsp, 2 * Interpreter::stackElementSize);
}

void InterpreterMacroAssembler::pop_i(Register r) {
  // XXX can't use pop currently, upper half non clean
  movl(r, Address(rsp, 0));
  addptr(rsp, wordSize);
}

void InterpreterMacroAssembler::pop_l(Register r) {
  movq(r, Address(rsp, 0));
  addptr(rsp, 2 * Interpreter::stackElementSize);
}

void InterpreterMacroAssembler::push_l(Register r) {
  subptr(rsp, 2 * wordSize);
  movptr(Address(rsp, Interpreter::expr_offset_in_bytes(0)), r         );
  movptr(Address(rsp, Interpreter::expr_offset_in_bytes(1)), NULL_WORD );
}

void InterpreterMacroAssembler::pop(TosState state) {
  switch (state) {
  case atos: pop_ptr();                 break;
  case btos:
  case ztos:
  case ctos:
  case stos:
  case itos: pop_i();                   break;
  case ltos: pop_l();                   break;
  case ftos: pop_f(xmm0);               break;
  case dtos: pop_d(xmm0);               break;
  case vtos: /* nothing to do */        break;
  default:   ShouldNotReachHere();
  }
  verify_oop(rax, state);
}

void InterpreterMacroAssembler::push(TosState state) {
  verify_oop(rax, state);
  switch (state) {
  case atos: push_ptr();                break;
  case btos:
  case ztos:
  case ctos:
  case stos:
  case itos: push_i();                  break;
  case ltos: push_l();                  break;
  case ftos: push_f(xmm0);              break;
  case dtos: push_d(xmm0);              break;
  case vtos: /* nothing to do */        break;
  default  : ShouldNotReachHere();
  }
}

// Helpers for swap and dup
void InterpreterMacroAssembler::load_ptr(int n, Register val) {
  movptr(val, Address(rsp, Interpreter::expr_offset_in_bytes(n)));
}

void InterpreterMacroAssembler::store_ptr(int n, Register val) {
  movptr(Address(rsp, Interpreter::expr_offset_in_bytes(n)), val);
}

void InterpreterMacroAssembler::prepare_to_jump_from_interpreted() {
  // set sender sp
  lea(_bcp_register, Address(rsp, wordSize));
  // record last_sp
  movptr(Address(rbp, frame::interpreter_frame_last_sp_offset * wordSize), _bcp_register);
}

// Jump to from_interpreted entry of a call unless single stepping is possible
// in this thread in which case we must call the i2i entry
void InterpreterMacroAssembler::jump_from_interpreted(Register method, Register temp) {
  prepare_to_jump_from_interpreted();

  jmp(Address(method, Method::from_interpreted_offset()));
}

// The following two routines provide a hook so that an implementation
// can schedule the dispatch in two parts.  x86 does not do this.
void InterpreterMacroAssembler::dispatch_prolog(TosState state, int step) {
  // Nothing x86 specific to be done here
}

void InterpreterMacroAssembler::dispatch_epilog(TosState state, int step) {
  dispatch_next(state, step);
}

void InterpreterMacroAssembler::dispatch_base(TosState state, address* table, bool verifyoop, bool generate_poll) {
  verify_FPU(1, state);
  if (VerifyActivationFrameSize) {
    Label L;
    mov(rcx, rbp);
    subptr(rcx, rsp);
    int32_t min_frame_size = (frame::link_offset - frame::interpreter_frame_initial_sp_offset) * wordSize;
    cmpptr(rcx, (int32_t)min_frame_size);
    jcc(Assembler::greaterEqual, L);
    stop("broken stack frame");
    bind(L);
  }
  if (verifyoop) {
    verify_oop(rax, state);
  }

  address* const safepoint_table = Interpreter::safept_table(state);
  Label no_safepoint, dispatch;
  if (SafepointMechanism::uses_thread_local_poll() && table != safepoint_table && generate_poll) {
    testb(Address(r15_thread, Thread::polling_page_offset()), SafepointMechanism::poll_bit());

    jccb(Assembler::zero, no_safepoint);
    lea(rscratch1, ExternalAddress((address)safepoint_table));
    jmpb(dispatch);
  }

  bind(no_safepoint);
  lea(rscratch1, ExternalAddress((address)table));
  bind(dispatch);
  jmp(Address(rscratch1, rbx, Address::times_8));
}

void InterpreterMacroAssembler::dispatch_only(TosState state, bool generate_poll) {
  dispatch_base(state, Interpreter::dispatch_table(state), true, generate_poll);
}

void InterpreterMacroAssembler::dispatch_only_normal(TosState state) {
  dispatch_base(state, Interpreter::normal_table(state));
}

void InterpreterMacroAssembler::dispatch_only_noverify(TosState state) {
  dispatch_base(state, Interpreter::normal_table(state), false);
}

void InterpreterMacroAssembler::dispatch_next(TosState state, int step, bool generate_poll) {
  // load next bytecode (load before advancing _bcp_register to prevent AGI)
  load_unsigned_byte(rbx, Address(_bcp_register, step));
  // advance _bcp_register
  increment(_bcp_register, step);
  dispatch_base(state, Interpreter::dispatch_table(state), true, generate_poll);
}

void InterpreterMacroAssembler::dispatch_via(TosState state, address* table) {
  // load current bytecode
  load_unsigned_byte(rbx, Address(_bcp_register, 0));
  dispatch_base(state, table);
}

void InterpreterMacroAssembler::narrow(Register result) {
  // Get method->_constMethod->_result_type
  movptr(rcx, Address(rbp, frame::interpreter_frame_method_offset * wordSize));
  movptr(rcx, Address(rcx, Method::const_offset()));
  load_unsigned_byte(rcx, Address(rcx, ConstMethod::result_type_offset()));

  Label done, notBool, notByte, notChar;

  // common case first
  cmpl(rcx, T_INT);
  jcc(Assembler::equal, done);

  // mask integer result to narrower return type.
  cmpl(rcx, T_BOOLEAN);
  jcc(Assembler::notEqual, notBool);
  andl(result, 0x1);
  jmp(done);

  bind(notBool);
  cmpl(rcx, T_BYTE);
  jcc(Assembler::notEqual, notByte);
  movsbl(result, result);
  jmp(done);

  bind(notByte);
  cmpl(rcx, T_CHAR);
  jcc(Assembler::notEqual, notChar);
  movzwl(result, result);
  jmp(done);

  bind(notChar);
  // cmpl(rcx, T_SHORT);  // all that's left
  // jcc(Assembler::notEqual, done);
  movswl(result, result);

  // Nothing to do for T_INT
  bind(done);
}

// remove activation
//
// Unlock the receiver if this is a synchronized method.
// Unlock any Java monitors from syncronized blocks.
// Remove the activation from the stack.
//
// If there are locked Java monitors
//    If throw_monitor_exception
//       throws IllegalMonitorStateException
//    Else if install_monitor_exception
//       installs IllegalMonitorStateException
//    Else
//       no error processing
void InterpreterMacroAssembler::remove_activation(TosState state, Register ret_addr, bool throw_monitor_exception, bool install_monitor_exception, bool notify_jvmdi) {
  // Note: Registers rdx xmm0 may be in use for the
  // result check if synchronized method
  Label unlocked, unlock, no_unlock;

  const Register rthread = r15_thread;
  const Register robj    = c_rarg1;
  const Register rmon    = c_rarg1;
                              // monitor pointers need different register
                              // because rdx may have the result in it

  // get the value of _do_not_unlock_if_synchronized into rdx
  const Address do_not_unlock_if_synchronized(rthread,
    in_bytes(JavaThread::do_not_unlock_if_synchronized_offset()));
  movbool(rbx, do_not_unlock_if_synchronized);
  movbool(do_not_unlock_if_synchronized, false); // reset the flag

 // get method access flags
  movptr(rcx, Address(rbp, frame::interpreter_frame_method_offset * wordSize));
  movl(rcx, Address(rcx, Method::access_flags_offset()));
  testl(rcx, JVM_ACC_SYNCHRONIZED);
  jcc(Assembler::zero, unlocked);

  // Don't unlock anything if the _do_not_unlock_if_synchronized flag
  // is set.
  testbool(rbx);
  jcc(Assembler::notZero, no_unlock);

  // unlock monitor
  push(state); // save result

  // BasicObjectLock will be first in list, since this is a
  // synchronized method. However, need to check that the object has
  // not been unlocked by an explicit monitorexit bytecode.
  const Address monitor(rbp, frame::interpreter_frame_initial_sp_offset * wordSize - (int) sizeof(BasicObjectLock));
  // We use c_rarg1/rdx so that if we go slow path it will be the correct
  // register for unlock_object to pass to VM directly
  lea(robj, monitor); // address of first monitor

  movptr(rax, Address(robj, BasicObjectLock::obj_offset_in_bytes()));
  testptr(rax, rax);
  jcc(Assembler::notZero, unlock);

  pop(state);
  if (throw_monitor_exception) {
    // Entry already unlocked, need to throw exception
    call_VM(noreg, CAST_FROM_FN_PTR(address, InterpreterRuntime::throw_illegal_monitor_state_exception));
    should_not_reach_here();
  } else {
    // Monitor already unlocked during a stack unroll. If requested,
    // install an illegal_monitor_state_exception.  Continue with
    // stack unrolling.
    if (install_monitor_exception) {
      call_VM(noreg, CAST_FROM_FN_PTR(address, InterpreterRuntime::new_illegal_monitor_state_exception));
    }
    jmp(unlocked);
  }

  bind(unlock);
  unlock_object(robj);
  pop(state);

  // Check that for block-structured locking (i.e., that all locked
  // objects has been unlocked)
  bind(unlocked);

  // rax, rdx: Might contain return value

  // Check that all monitors are unlocked
  {
    Label loop, exception, entry, restart;
    const int entry_size = frame::interpreter_frame_monitor_size() * wordSize;
    const Address monitor_block_top(rbp, frame::interpreter_frame_monitor_block_top_offset * wordSize);
    const Address monitor_block_bot(rbp, frame::interpreter_frame_initial_sp_offset * wordSize);

    bind(restart);
    // We use c_rarg1 so that if we go slow path it will be the correct
    // register for unlock_object to pass to VM directly
    movptr(rmon, monitor_block_top); // points to current entry, starting
                                  // with top-most entry
    lea(rbx, monitor_block_bot);  // points to word before bottom of
                                  // monitor block
    jmp(entry);

    // Entry already locked, need to throw exception
    bind(exception);

    if (throw_monitor_exception) {
      // Throw exception
      MacroAssembler::call_VM(noreg, CAST_FROM_FN_PTR(address, InterpreterRuntime::throw_illegal_monitor_state_exception));
      should_not_reach_here();
    } else {
      // Stack unrolling. Unlock object and install illegal_monitor_exception.
      // Unlock does not block, so don't have to worry about the frame.
      // We don't have to preserve c_rarg1 since we are going to throw an exception.

      push(state);
      mov(robj, rmon);   // nop if robj and rmon are the same
      unlock_object(robj);
      pop(state);

      if (install_monitor_exception) {
        call_VM(noreg, CAST_FROM_FN_PTR(address, InterpreterRuntime::new_illegal_monitor_state_exception));
      }

      jmp(restart);
    }

    bind(loop);
    // check if current entry is used
    cmpptr(Address(rmon, BasicObjectLock::obj_offset_in_bytes()), (int32_t) NULL);
    jcc(Assembler::notEqual, exception);

    addptr(rmon, entry_size); // otherwise advance to next entry
    bind(entry);
    cmpptr(rmon, rbx); // check if bottom reached
    jcc(Assembler::notEqual, loop); // if not at bottom then check this entry
  }

  bind(no_unlock);

  // remove activation
  // get sender sp
  movptr(rbx,
         Address(rbp, frame::interpreter_frame_sender_sp_offset * wordSize));
  if (StackReservedPages > 0) {
    // testing if reserved zone needs to be re-enabled
    Register rthread = r15_thread;
    Label no_reserved_zone_enabling;

    cmpl(Address(rthread, JavaThread::stack_guard_state_offset()), JavaThread::stack_guard_enabled);
    jcc(Assembler::equal, no_reserved_zone_enabling);

    cmpptr(rbx, Address(rthread, JavaThread::reserved_stack_activation_offset()));
    jcc(Assembler::lessEqual, no_reserved_zone_enabling);

    call_VM_leaf(CAST_FROM_FN_PTR(address, SharedRuntime::enable_stack_reserved_zone), rthread);
    call_VM(noreg, CAST_FROM_FN_PTR(address, InterpreterRuntime::throw_delayed_StackOverflowError));
    should_not_reach_here();

    bind(no_reserved_zone_enabling);
  }
  leave();                           // remove frame anchor
  pop(ret_addr);                     // get return address
  mov(rsp, rbx);                     // set sp to sender sp
}

void InterpreterMacroAssembler::get_method_counters(Register method, Register mcs, Label& skip) {
  Label has_counters;
  movptr(mcs, Address(method, Method::method_counters_offset()));
  testptr(mcs, mcs);
  jcc(Assembler::notZero, has_counters);
  call_VM(noreg, CAST_FROM_FN_PTR(address, InterpreterRuntime::build_method_counters), method);
  movptr(mcs, Address(method,Method::method_counters_offset()));
  testptr(mcs, mcs);
  jcc(Assembler::zero, skip); // No MethodCounters allocated, OutOfMemory
  bind(has_counters);
}

// Lock object
//
// Args:
//      rdx, c_rarg1: BasicObjectLock to be used for locking
//
// Kills:
//      rax, rbx
void InterpreterMacroAssembler::lock_object(Register lock_reg) {
  if (UseHeavyMonitors) {
    call_VM(noreg, CAST_FROM_FN_PTR(address, InterpreterRuntime::monitorenter), lock_reg);
  } else {
    Label done;

    const Register swap_reg = rax; // Must use rax for cmpxchg instruction
    const Register tmp_reg = rbx; // Will be passed to biased_locking_enter to avoid a
                                  // problematic case where tmp_reg = no_reg.
    const Register obj_reg = c_rarg3; // Will contain the oop

    const int obj_offset = BasicObjectLock::obj_offset_in_bytes();
    const int lock_offset = BasicObjectLock::lock_offset_in_bytes ();
    const int mark_offset = lock_offset + BasicLock::displaced_header_offset_in_bytes();

    Label slow_case;

    // Load object pointer into obj_reg
    movptr(obj_reg, Address(lock_reg, obj_offset));

    if (UseBiasedLocking) {
      biased_locking_enter(lock_reg, obj_reg, swap_reg, tmp_reg, false, done, &slow_case);
    }

    // Load immediate 1 into swap_reg %rax
    movl(swap_reg, (int32_t)1);

    // Load (object->mark() | 1) into swap_reg %rax
    orptr(swap_reg, Address(obj_reg, oopDesc::mark_offset_in_bytes()));

    // Save (object->mark() | 1) into BasicLock's displaced header
    movptr(Address(lock_reg, mark_offset), swap_reg);

    if (os::is_MP()) lock();
    cmpxchgptr(lock_reg, Address(obj_reg, oopDesc::mark_offset_in_bytes()));
    if (PrintBiasedLockingStatistics) {
      cond_inc32(Assembler::zero,
                 ExternalAddress((address) BiasedLocking::fast_path_entry_count_addr()));
    }
    jcc(Assembler::zero, done);

    const int zero_bits = 7;

    // Test if the oopMark is an obvious stack pointer, i.e.,
    //  1) (mark & zero_bits) == 0, and
    //  2) rsp <= mark < mark + os::pagesize()
    //
    // These 3 tests can be done by evaluating the following
    // expression: ((mark - rsp) & (zero_bits - os::vm_page_size())),
    // assuming both stack pointer and pagesize have their
    // least significant bits clear.
    // NOTE: the oopMark is in swap_reg %rax as the result of cmpxchg
    subptr(swap_reg, rsp);
    andptr(swap_reg, zero_bits - os::vm_page_size());

    // Save the test result, for recursive case, the result is zero
    movptr(Address(lock_reg, mark_offset), swap_reg);

    if (PrintBiasedLockingStatistics) {
      cond_inc32(Assembler::zero,
                 ExternalAddress((address) BiasedLocking::fast_path_entry_count_addr()));
    }
    jcc(Assembler::zero, done);

    bind(slow_case);

    // Call the runtime routine for slow case
    call_VM(noreg, CAST_FROM_FN_PTR(address, InterpreterRuntime::monitorenter), lock_reg);

    bind(done);
  }
}

// Unlocks an object. Used in monitorexit bytecode and
// remove_activation.  Throws an IllegalMonitorException if object is
// not locked by current thread.
//
// Args:
//      rdx, c_rarg1: BasicObjectLock for lock
//
// Kills:
//      rax
//      c_rarg0, c_rarg1, c_rarg2, c_rarg3, ... (param regs)
//      rscratch1 (scratch reg)
// rax, rbx, rcx, rdx
void InterpreterMacroAssembler::unlock_object(Register lock_reg) {
  if (UseHeavyMonitors) {
    call_VM(noreg, CAST_FROM_FN_PTR(address, InterpreterRuntime::monitorexit), lock_reg);
  } else {
    Label done;

    const Register swap_reg   = rax;  // Must use rax for cmpxchg instruction
    const Register header_reg = c_rarg2;  // Will contain the old oopMark
    const Register obj_reg    = c_rarg3;  // Will contain the oop

    save_bcp(); // Save in case of exception

    // Convert from BasicObjectLock structure to object and BasicLock
    // structure Store the BasicLock address into %rax
    lea(swap_reg, Address(lock_reg, BasicObjectLock::lock_offset_in_bytes()));

    // Load oop into obj_reg(%c_rarg3)
    movptr(obj_reg, Address(lock_reg, BasicObjectLock::obj_offset_in_bytes()));

    // Free entry
    movptr(Address(lock_reg, BasicObjectLock::obj_offset_in_bytes()), (int32_t)NULL_WORD);

    if (UseBiasedLocking) {
      biased_locking_exit(obj_reg, header_reg, done);
    }

    // Load the old header from BasicLock structure
    movptr(header_reg, Address(swap_reg,
                               BasicLock::displaced_header_offset_in_bytes()));

    // Test for recursion
    testptr(header_reg, header_reg);

    // zero for recursive case
    jcc(Assembler::zero, done);

    // Atomic swap back the old header
    if (os::is_MP()) lock();
    cmpxchgptr(header_reg, Address(obj_reg, oopDesc::mark_offset_in_bytes()));

    // zero for simple unlock of a stack-lock case
    jcc(Assembler::zero, done);

    // Call the runtime routine for slow case.
    movptr(Address(lock_reg, BasicObjectLock::obj_offset_in_bytes()), obj_reg); // restore obj
    call_VM(noreg, CAST_FROM_FN_PTR(address, InterpreterRuntime::monitorexit), lock_reg);

    bind(done);

    restore_bcp();
  }
}

void InterpreterMacroAssembler::test_method_data_pointer(Register mdp, Label& zero_continue) {
  movptr(mdp, Address(rbp, frame::interpreter_frame_mdp_offset * wordSize));
  testptr(mdp, mdp);
  jcc(Assembler::zero, zero_continue);
}

// Set the method data pointer for the current bcp.
void InterpreterMacroAssembler::set_method_data_pointer_for_bcp() {
  Label set_mdp;
  push(rax);
  push(rbx);

  get_method(rbx);
  // Test MDO to avoid the call if it is NULL.
  movptr(rax, Address(rbx, in_bytes(Method::method_data_offset())));
  testptr(rax, rax);
  jcc(Assembler::zero, set_mdp);
  // rbx: method
  // _bcp_register: bcp
  call_VM_leaf(CAST_FROM_FN_PTR(address, InterpreterRuntime::bcp_to_di), rbx, _bcp_register);
  // rax: mdi
  // mdo is guaranteed to be non-zero here, we checked for it before the call.
  movptr(rbx, Address(rbx, in_bytes(Method::method_data_offset())));
  addptr(rbx, in_bytes(MethodData::data_offset()));
  addptr(rax, rbx);
  bind(set_mdp);
  movptr(Address(rbp, frame::interpreter_frame_mdp_offset * wordSize), rax);
  pop(rbx);
  pop(rax);
}

void InterpreterMacroAssembler::verify_method_data_pointer() { }

void InterpreterMacroAssembler::set_mdp_data_at(Register mdp_in, int constant, Register value) {
  Address data(mdp_in, constant);
  movptr(data, value);
}

void InterpreterMacroAssembler::increment_mdp_data_at(Register mdp_in, int constant, bool decrement) {
  // Counter address
  Address data(mdp_in, constant);

  increment_mdp_data_at(data, decrement);
}

void InterpreterMacroAssembler::increment_mdp_data_at(Address data, bool decrement) {
  // %%% this does 64bit counters at best it is wasting space
  // at worst it is a rare bug when counters overflow

  if (decrement) {
    // Decrement the register.  Set condition codes.
    addptr(data, (int32_t) -DataLayout::counter_increment);
    // If the decrement causes the counter to overflow, stay negative
    Label L;
    jcc(Assembler::negative, L);
    addptr(data, (int32_t) DataLayout::counter_increment);
    bind(L);
  } else {
    // Increment the register.  Set carry flag.
    addptr(data, DataLayout::counter_increment);
    // If the increment causes the counter to overflow, pull back by 1.
    sbbptr(data, (int32_t)0);
  }
}

void InterpreterMacroAssembler::increment_mdp_data_at(Register mdp_in, Register reg, int constant, bool decrement) {
  Address data(mdp_in, reg, Address::times_1, constant);

  increment_mdp_data_at(data, decrement);
}

void InterpreterMacroAssembler::set_mdp_flag_at(Register mdp_in, int flag_byte_constant) {
  int header_offset = in_bytes(DataLayout::flags_offset());
  int header_bits = flag_byte_constant;
  // Set the flag
  orb(Address(mdp_in, header_offset), header_bits);
}

void InterpreterMacroAssembler::test_mdp_data_at(Register mdp_in, int offset, Register value, Register test_value_out, Label& not_equal_continue) {
  if (test_value_out == noreg) {
    cmpptr(value, Address(mdp_in, offset));
  } else {
    // Put the test value into a register, so caller can use it:
    movptr(test_value_out, Address(mdp_in, offset));
    cmpptr(test_value_out, value);
  }
  jcc(Assembler::notEqual, not_equal_continue);
}

void InterpreterMacroAssembler::update_mdp_by_offset(Register mdp_in, int offset_of_disp) {
  Address disp_address(mdp_in, offset_of_disp);
  addptr(mdp_in, disp_address);
  movptr(Address(rbp, frame::interpreter_frame_mdp_offset * wordSize), mdp_in);
}

void InterpreterMacroAssembler::update_mdp_by_offset(Register mdp_in, Register reg, int offset_of_disp) {
  Address disp_address(mdp_in, reg, Address::times_1, offset_of_disp);
  addptr(mdp_in, disp_address);
  movptr(Address(rbp, frame::interpreter_frame_mdp_offset * wordSize), mdp_in);
}

void InterpreterMacroAssembler::update_mdp_by_constant(Register mdp_in, int constant) {
  addptr(mdp_in, constant);
  movptr(Address(rbp, frame::interpreter_frame_mdp_offset * wordSize), mdp_in);
}

void InterpreterMacroAssembler::update_mdp_for_ret(Register return_bci) {
  push(return_bci); // save/restore across call_VM
  call_VM(noreg, CAST_FROM_FN_PTR(address, InterpreterRuntime::update_mdp_for_ret), return_bci);
  pop(return_bci);
}

void InterpreterMacroAssembler::profile_taken_branch(Register mdp, Register bumped_count) {
  if (ProfileInterpreter) {
    Label profile_continue;

    // If no method data exists, go to profile_continue.
    // Otherwise, assign to mdp
    test_method_data_pointer(mdp, profile_continue);

    // We are taking a branch.  Increment the taken count.
    // We inline increment_mdp_data_at to return bumped_count in a register
    //increment_mdp_data_at(mdp, in_bytes(JumpData::taken_offset()));
    Address data(mdp, in_bytes(JumpData::taken_offset()));
    movptr(bumped_count, data);
    addptr(bumped_count, DataLayout::counter_increment);
    sbbptr(bumped_count, 0);
    movptr(data, bumped_count); // Store back out

    // The method data pointer needs to be updated to reflect the new target.
    update_mdp_by_offset(mdp, in_bytes(JumpData::displacement_offset()));
    bind(profile_continue);
  }
}

void InterpreterMacroAssembler::profile_not_taken_branch(Register mdp) {
  if (ProfileInterpreter) {
    Label profile_continue;

    // If no method data exists, go to profile_continue.
    test_method_data_pointer(mdp, profile_continue);

    // We are taking a branch.  Increment the not taken count.
    increment_mdp_data_at(mdp, in_bytes(BranchData::not_taken_offset()));

    // The method data pointer needs to be updated to correspond to
    // the next bytecode
    update_mdp_by_constant(mdp, in_bytes(BranchData::branch_data_size()));
    bind(profile_continue);
  }
}

void InterpreterMacroAssembler::profile_call(Register mdp) {
  if (ProfileInterpreter) {
    Label profile_continue;

    // If no method data exists, go to profile_continue.
    test_method_data_pointer(mdp, profile_continue);

    // We are making a call.  Increment the count.
    increment_mdp_data_at(mdp, in_bytes(CounterData::count_offset()));

    // The method data pointer needs to be updated to reflect the new target.
    update_mdp_by_constant(mdp, in_bytes(CounterData::counter_data_size()));
    bind(profile_continue);
  }
}

void InterpreterMacroAssembler::profile_final_call(Register mdp) {
  if (ProfileInterpreter) {
    Label profile_continue;

    // If no method data exists, go to profile_continue.
    test_method_data_pointer(mdp, profile_continue);

    // We are making a call.  Increment the count.
    increment_mdp_data_at(mdp, in_bytes(CounterData::count_offset()));

    // The method data pointer needs to be updated to reflect the new target.
    update_mdp_by_constant(mdp, in_bytes(VirtualCallData::virtual_call_data_size()));
    bind(profile_continue);
  }
}

void InterpreterMacroAssembler::profile_virtual_call(Register receiver, Register mdp, Register reg2, bool receiver_can_be_null) {
  if (ProfileInterpreter) {
    Label profile_continue;

    // If no method data exists, go to profile_continue.
    test_method_data_pointer(mdp, profile_continue);

    Label skip_receiver_profile;
    if (receiver_can_be_null) {
      Label not_null;
      testptr(receiver, receiver);
      jccb(Assembler::notZero, not_null);
      // We are making a call.  Increment the count for null receiver.
      increment_mdp_data_at(mdp, in_bytes(CounterData::count_offset()));
      jmp(skip_receiver_profile);
      bind(not_null);
    }

    // Record the receiver type.
    record_klass_in_profile(receiver, mdp, reg2, true);
    bind(skip_receiver_profile);

    // The method data pointer needs to be updated to reflect the new target.
    if (MethodProfileWidth == 0) {
      update_mdp_by_constant(mdp, in_bytes(VirtualCallData::virtual_call_data_size()));
    }
    bind(profile_continue);
  }
}

void InterpreterMacroAssembler::profile_called_method(Register method, Register mdp, Register reg2) {
  if (ProfileInterpreter && MethodProfileWidth > 0) {
    Label profile_continue;

    // If no method data exists, go to profile_continue.
    test_method_data_pointer(mdp, profile_continue);

    Label done;
    record_item_in_profile_helper(method, mdp, reg2, 0, done, MethodProfileWidth,
      &VirtualCallData::method_offset, &VirtualCallData::method_count_offset, in_bytes(VirtualCallData::nonprofiled_receiver_count_offset()));
    bind(done);

    update_mdp_by_constant(mdp, in_bytes(VirtualCallData::virtual_call_data_size()));
    bind(profile_continue);
  }
}

// This routine creates a state machine for updating the multi-row
// type profile at a virtual call site (or other type-sensitive bytecode).
// The machine visits each row (of receiver/count) until the receiver type
// is found, or until it runs out of rows.  At the same time, it remembers
// the location of the first empty row.  (An empty row records null for its
// receiver, and can be allocated for a newly-observed receiver type.)
// Because there are two degrees of freedom in the state, a simple linear
// search will not work; it must be a decision tree.  Hence this helper
// function is recursive, to generate the required tree structured code.
// It's the interpreter, so we are trading off code space for speed.
// See below for example code.
void InterpreterMacroAssembler::record_klass_in_profile_helper(Register receiver, Register mdp, Register reg2, int start_row, Label& done, bool is_virtual_call) {
  if (TypeProfileWidth == 0) {
    if (is_virtual_call) {
      increment_mdp_data_at(mdp, in_bytes(CounterData::count_offset()));
    } else if (EnableJVMCI) {
      increment_mdp_data_at(mdp, in_bytes(ReceiverTypeData::nonprofiled_receiver_count_offset()));
    }
  } else {
    int non_profiled_offset = -1;
    if (is_virtual_call) {
      non_profiled_offset = in_bytes(CounterData::count_offset());
    } else if (EnableJVMCI) {
      non_profiled_offset = in_bytes(ReceiverTypeData::nonprofiled_receiver_count_offset());
    }

    record_item_in_profile_helper(receiver, mdp, reg2, 0, done, TypeProfileWidth,
        &VirtualCallData::receiver_offset, &VirtualCallData::receiver_count_offset, non_profiled_offset);
  }
}

void InterpreterMacroAssembler::record_item_in_profile_helper(Register item, Register mdp, Register reg2, int start_row, Label& done, int total_rows, OffsetFunction item_offset_fn, OffsetFunction item_count_offset_fn, int non_profiled_offset) {
  int last_row = total_rows - 1;
  // Test this row for both the item and for null.
  // Take any of three different outcomes:
  //   1. found item => increment count and goto done
  //   2. found null => keep looking for case 1, maybe allocate this cell
  //   3. found something else => keep looking for cases 1 and 2
  // Case 3 is handled by a recursive call.
  for (int row = start_row; row <= last_row; row++) {
    Label next_test;
    bool test_for_null_also = (row == start_row);

    // See if the item is item[n].
    int item_offset = in_bytes(item_offset_fn(row));
    test_mdp_data_at(mdp, item_offset, item,
                     (test_for_null_also ? reg2 : noreg),
                     next_test);
    // (Reg2 now contains the item from the CallData.)

    // The item is item[n].  Increment count[n].
    int count_offset = in_bytes(item_count_offset_fn(row));
    increment_mdp_data_at(mdp, count_offset);
    jmp(done);
    bind(next_test);

    if (test_for_null_also) {
      Label found_null;
      // Failed the equality check on item[n]...  Test for null.
      testptr(reg2, reg2);
      if (start_row == last_row) {
        // The only thing left to do is handle the null case.
        if (non_profiled_offset >= 0) {
          jccb(Assembler::zero, found_null);
          // Item did not match any saved item and there is no empty row for it.
          // Increment total counter to indicate polymorphic case.
          increment_mdp_data_at(mdp, non_profiled_offset);
          jmp(done);
          bind(found_null);
        } else {
          jcc(Assembler::notZero, done);
        }
        break;
      }
      // Since null is rare, make it be the branch-taken case.
      jcc(Assembler::zero, found_null);

      // Put all the "Case 3" tests here.
      record_item_in_profile_helper(item, mdp, reg2, start_row + 1, done, total_rows,
        item_offset_fn, item_count_offset_fn, non_profiled_offset);

      // Found a null.  Keep searching for a matching item,
      // but remember that this is an empty (unused) slot.
      bind(found_null);
    }
  }

  // In the fall-through case, we found no matching item, but we
  // observed the item[start_row] is NULL.

  // Fill in the item field and increment the count.
  int item_offset = in_bytes(item_offset_fn(start_row));
  set_mdp_data_at(mdp, item_offset, item);
  int count_offset = in_bytes(item_count_offset_fn(start_row));
  movl(reg2, DataLayout::counter_increment);
  set_mdp_data_at(mdp, count_offset, reg2);
  if (start_row > 0) {
    jmp(done);
  }
}

void InterpreterMacroAssembler::record_klass_in_profile(Register receiver, Register mdp, Register reg2, bool is_virtual_call) {
  Label done;

  record_klass_in_profile_helper(receiver, mdp, reg2, 0, done, is_virtual_call);

  bind (done);
}

void InterpreterMacroAssembler::profile_ret(Register return_bci, Register mdp) {
  if (ProfileInterpreter) {
    Label profile_continue;
    uint row;

    // If no method data exists, go to profile_continue.
    test_method_data_pointer(mdp, profile_continue);

    // Update the total ret count.
    increment_mdp_data_at(mdp, in_bytes(CounterData::count_offset()));

    for (row = 0; row < RetData::row_limit(); row++) {
      Label next_test;

      // See if return_bci is equal to bci[n]:
      test_mdp_data_at(mdp,
                       in_bytes(RetData::bci_offset(row)),
                       return_bci, noreg,
                       next_test);

      // return_bci is equal to bci[n].  Increment the count.
      increment_mdp_data_at(mdp, in_bytes(RetData::bci_count_offset(row)));

      // The method data pointer needs to be updated to reflect the new target.
      update_mdp_by_offset(mdp,
                           in_bytes(RetData::bci_displacement_offset(row)));
      jmp(profile_continue);
      bind(next_test);
    }

    update_mdp_for_ret(return_bci);

    bind(profile_continue);
  }
}

void InterpreterMacroAssembler::profile_null_seen(Register mdp) {
  if (ProfileInterpreter) {
    Label profile_continue;

    // If no method data exists, go to profile_continue.
    test_method_data_pointer(mdp, profile_continue);

    set_mdp_flag_at(mdp, BitData::null_seen_byte_constant());

    // The method data pointer needs to be updated.
    int mdp_delta = in_bytes(BitData::bit_data_size());
    if (TypeProfileCasts) {
      mdp_delta = in_bytes(VirtualCallData::virtual_call_data_size());
    }
    update_mdp_by_constant(mdp, mdp_delta);

    bind(profile_continue);
  }
}

void InterpreterMacroAssembler::profile_typecheck_failed(Register mdp) {
  if (ProfileInterpreter && TypeProfileCasts) {
    Label profile_continue;

    // If no method data exists, go to profile_continue.
    test_method_data_pointer(mdp, profile_continue);

    int count_offset = in_bytes(CounterData::count_offset());
    // Back up the address, since we have already bumped the mdp.
    count_offset -= in_bytes(VirtualCallData::virtual_call_data_size());

    // *Decrement* the counter.  We expect to see zero or small negatives.
    increment_mdp_data_at(mdp, count_offset, true);

    bind (profile_continue);
  }
}

void InterpreterMacroAssembler::profile_typecheck(Register mdp, Register klass, Register reg2) {
  if (ProfileInterpreter) {
    Label profile_continue;

    // If no method data exists, go to profile_continue.
    test_method_data_pointer(mdp, profile_continue);

    // The method data pointer needs to be updated.
    int mdp_delta = in_bytes(BitData::bit_data_size());
    if (TypeProfileCasts) {
      mdp_delta = in_bytes(VirtualCallData::virtual_call_data_size());

      // Record the object type.
      record_klass_in_profile(klass, mdp, reg2, false);
    }
    update_mdp_by_constant(mdp, mdp_delta);

    bind(profile_continue);
  }
}

void InterpreterMacroAssembler::profile_switch_default(Register mdp) {
  if (ProfileInterpreter) {
    Label profile_continue;

    // If no method data exists, go to profile_continue.
    test_method_data_pointer(mdp, profile_continue);

    // Update the default case count
    increment_mdp_data_at(mdp, in_bytes(MultiBranchData::default_count_offset()));

    // The method data pointer needs to be updated.
    update_mdp_by_offset(mdp, in_bytes(MultiBranchData::default_displacement_offset()));

    bind(profile_continue);
  }
}

void InterpreterMacroAssembler::profile_switch_case(Register index, Register mdp, Register reg2) {
  if (ProfileInterpreter) {
    Label profile_continue;

    // If no method data exists, go to profile_continue.
    test_method_data_pointer(mdp, profile_continue);

    // Build the base (index * per_case_size_in_bytes()) +
    // case_array_offset_in_bytes()
    movl(reg2, in_bytes(MultiBranchData::per_case_size()));
    imulptr(index, reg2); // XXX l ?
    addptr(index, in_bytes(MultiBranchData::case_array_offset())); // XXX l ?

    // Update the case count
    increment_mdp_data_at(mdp, index, in_bytes(MultiBranchData::relative_count_offset()));

    // The method data pointer needs to be updated.
    update_mdp_by_offset(mdp, index, in_bytes(MultiBranchData::relative_displacement_offset()));

    bind(profile_continue);
  }
}

void InterpreterMacroAssembler::verify_oop(Register reg, TosState state) {
  if (state == atos) {
    MacroAssembler::verify_oop(reg);
  }
}

void InterpreterMacroAssembler::verify_FPU(int stack_depth, TosState state) { }

// Jump if ((*counter_addr += increment) & mask) satisfies the condition.
void InterpreterMacroAssembler::increment_mask_and_jump(Address counter_addr, int increment, Address mask, Register scratch, bool preloaded, Condition cond, Label* where) {
  if (!preloaded) {
    movl(scratch, counter_addr);
  }
  incrementl(scratch, increment);
  movl(counter_addr, scratch);
  andl(scratch, mask);
  if (where != NULL) {
    jcc(cond, *where);
  }
}
