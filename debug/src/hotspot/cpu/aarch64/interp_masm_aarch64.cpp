#include "precompiled.hpp"

#include "asm/macroAssembler.inline.hpp"
#include "gc/shared/barrierSet.hpp"
#include "gc/shared/barrierSetAssembler.hpp"
#include "interp_masm_aarch64.hpp"
#include "interpreter/interpreter.hpp"
#include "interpreter/interpreterRuntime.hpp"
#include "logging/log.hpp"
#include "oops/arrayOop.hpp"
#include "oops/markOop.hpp"
#include "oops/method.hpp"
#include "oops/methodData.hpp"
#include "runtime/basicLock.hpp"
#include "runtime/biasedLocking.hpp"
#include "runtime/frame.inline.hpp"
#include "runtime/safepointMechanism.hpp"
#include "runtime/sharedRuntime.hpp"
#include "runtime/thread.inline.hpp"

void InterpreterMacroAssembler::narrow(Register result) {

  // Get method->_constMethod->_result_type
  ldr(rscratch1, Address(rfp, frame::interpreter_frame_method_offset * wordSize));
  ldr(rscratch1, Address(rscratch1, Method::const_offset()));
  ldrb(rscratch1, Address(rscratch1, ConstMethod::result_type_offset()));

  Label done, notBool, notByte, notChar;

  // common case first
  cmpw(rscratch1, T_INT);
  br(Assembler::EQ, done);

  // mask integer result to narrower return type.
  cmpw(rscratch1, T_BOOLEAN);
  br(Assembler::NE, notBool);
  andw(result, result, 0x1);
  b(done);

  bind(notBool);
  cmpw(rscratch1, T_BYTE);
  br(Assembler::NE, notByte);
  sbfx(result, result, 0, 8);
  b(done);

  bind(notByte);
  cmpw(rscratch1, T_CHAR);
  br(Assembler::NE, notChar);
  ubfx(result, result, 0, 16);  // truncate upper 16 bits
  b(done);

  bind(notChar);
  sbfx(result, result, 0, 16);     // sign-extend short

  // Nothing to do for T_INT
  bind(done);
}

void InterpreterMacroAssembler::jump_to_entry(address entry) {
  b(entry);
}

void InterpreterMacroAssembler::get_unsigned_2_byte_index_at_bcp(Register reg, int bcp_offset) {
  ldrh(reg, Address(rbcp, bcp_offset));
  rev16(reg, reg);
}

void InterpreterMacroAssembler::get_dispatch() {
  unsigned long offset;
  adrp(rdispatch, ExternalAddress((address)Interpreter::dispatch_table()), offset);
  lea(rdispatch, Address(rdispatch, offset));
}

void InterpreterMacroAssembler::get_cache_index_at_bcp(Register index, int bcp_offset, size_t index_size) {
  if (index_size == sizeof(u2)) {
    load_unsigned_short(index, Address(rbcp, bcp_offset));
  } else if (index_size == sizeof(u4)) {
    ldrw(index, Address(rbcp, bcp_offset));
    eonw(index, index, zr);  // convert to plain index
  } else if (index_size == sizeof(u1)) {
    load_unsigned_byte(index, Address(rbcp, bcp_offset));
  } else {
    ShouldNotReachHere();
  }
}

// Return
// Rindex: index into constant pool
// Rcache: address of cache entry - ConstantPoolCache::base_offset()
//
// A caller must add ConstantPoolCache::base_offset() to Rcache to get
// the true address of the cache entry.
//
void InterpreterMacroAssembler::get_cache_and_index_at_bcp(Register cache, Register index, int bcp_offset, size_t index_size) {
  get_cache_index_at_bcp(index, bcp_offset, index_size);
  // convert from field index to ConstantPoolCacheEntry
  // aarch64 already has the cache in rcpool so there is no need to
  // install it in cache. instead we pre-add the indexed offset to
  // rcpool and return it in cache. All clients of this method need to
  // be modified accordingly.
  add(cache, rcpool, index, Assembler::LSL, 5);
}

void InterpreterMacroAssembler::get_cache_and_index_and_bytecode_at_bcp(Register cache, Register index, Register bytecode, int byte_no, int bcp_offset, size_t index_size) {
  get_cache_and_index_at_bcp(cache, index, bcp_offset, index_size);
  // We use a 32-bit load here since the layout of 64-bit words on
  // little-endian machines allow us that.
  // n.b. unlike x86 cache already includes the index offset
  lea(bytecode, Address(cache,
                         ConstantPoolCache::base_offset()
                         + ConstantPoolCacheEntry::indices_offset()));
  ldarw(bytecode, bytecode);
  const int shift_count = (1 + byte_no) * BitsPerByte;
  ubfx(bytecode, bytecode, shift_count, BitsPerByte);
}

void InterpreterMacroAssembler::get_cache_entry_pointer_at_bcp(Register cache, Register tmp, int bcp_offset, size_t index_size) {
  get_cache_index_at_bcp(tmp, bcp_offset, index_size);
  // convert from field index to ConstantPoolCacheEntry index
  // and from word offset to byte offset
  ldr(cache, Address(rfp, frame::interpreter_frame_cache_offset * wordSize));
  // skip past the header
  add(cache, cache, in_bytes(ConstantPoolCache::base_offset()));
  add(cache, cache, tmp, Assembler::LSL, 2 + LogBytesPerWord);  // construct pointer to cache entry
}

void InterpreterMacroAssembler::get_method_counters(Register method, Register mcs, Label& skip) {
  Label has_counters;
  ldr(mcs, Address(method, Method::method_counters_offset()));
  cbnz(mcs, has_counters);
  call_VM(noreg, CAST_FROM_FN_PTR(address, InterpreterRuntime::build_method_counters), method);
  ldr(mcs, Address(method, Method::method_counters_offset()));
  cbz(mcs, skip); // No MethodCounters allocated, OutOfMemory
  bind(has_counters);
}

// Load object from cpool->resolved_references(index)
void InterpreterMacroAssembler::load_resolved_reference_at_index(Register result, Register index, Register tmp) {
  get_constant_pool(result);
  // load pointer for resolved_references[] objArray
  ldr(result, Address(result, ConstantPool::cache_offset_in_bytes()));
  ldr(result, Address(result, ConstantPoolCache::resolved_references_offset_in_bytes()));
  resolve_oop_handle(result, tmp);
  // Add in the index
  add(index, index, arrayOopDesc::base_offset_in_bytes(T_OBJECT) >> LogBytesPerHeapOop);
  load_heap_oop(result, Address(result, index, Address::uxtw(LogBytesPerHeapOop)));
}

void InterpreterMacroAssembler::load_resolved_klass_at_offset(Register cpool, Register index, Register klass, Register temp) {
  add(temp, cpool, index, LSL, LogBytesPerWord);
  ldrh(temp, Address(temp, sizeof(ConstantPool))); // temp = resolved_klass_index
  ldr(klass, Address(cpool,  ConstantPool::resolved_klasses_offset_in_bytes())); // klass = cpool->_resolved_klasses
  add(klass, klass, temp, LSL, LogBytesPerWord);
  ldr(klass, Address(klass, Array<Klass*>::base_offset_in_bytes()));
}

// Generate a subtype check: branch to ok_is_subtype if sub_klass is a
// subtype of super_klass.
//
// Args:
//      r0: superklass
//      Rsub_klass: subklass
//
// Kills:
//      r2, r5
void InterpreterMacroAssembler::gen_subtype_check(Register Rsub_klass, Label& ok_is_subtype) {

  // Profile the not-null value's klass.
  profile_typecheck(r2, Rsub_klass, r5); // blows r2, reloads r5

  // Do the check.
  check_klass_subtype(Rsub_klass, r0, r2, ok_is_subtype); // blows r2

  // Profile the failure of the check.
  profile_typecheck_failed(r2); // blows r2
}

// Java Expression Stack

void InterpreterMacroAssembler::pop_ptr(Register r) {
  ldr(r, post(esp, wordSize));
}

void InterpreterMacroAssembler::pop_i(Register r) {
  ldrw(r, post(esp, wordSize));
}

void InterpreterMacroAssembler::pop_l(Register r) {
  ldr(r, post(esp, 2 * Interpreter::stackElementSize));
}

void InterpreterMacroAssembler::push_ptr(Register r) {
  str(r, pre(esp, -wordSize));
 }

void InterpreterMacroAssembler::push_i(Register r) {
  str(r, pre(esp, -wordSize));
}

void InterpreterMacroAssembler::push_l(Register r) {
  str(zr, pre(esp, -wordSize));
  str(r, pre(esp, - wordSize));
}

void InterpreterMacroAssembler::pop_f(FloatRegister r) {
  ldrs(r, post(esp, wordSize));
}

void InterpreterMacroAssembler::pop_d(FloatRegister r) {
  ldrd(r, post(esp, 2 * Interpreter::stackElementSize));
}

void InterpreterMacroAssembler::push_f(FloatRegister r) {
  strs(r, pre(esp, -wordSize));
}

void InterpreterMacroAssembler::push_d(FloatRegister r) {
  strd(r, pre(esp, 2* -wordSize));
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
  case ftos: pop_f();                   break;
  case dtos: pop_d();                   break;
  case vtos: /* nothing to do */        break;
  default:   ShouldNotReachHere();
  }
  verify_oop(r0, state);
}

void InterpreterMacroAssembler::push(TosState state) {
  verify_oop(r0, state);
  switch (state) {
  case atos: push_ptr();                break;
  case btos:
  case ztos:
  case ctos:
  case stos:
  case itos: push_i();                  break;
  case ltos: push_l();                  break;
  case ftos: push_f();                  break;
  case dtos: push_d();                  break;
  case vtos: /* nothing to do */        break;
  default  : ShouldNotReachHere();
  }
}

// Helpers for swap and dup
void InterpreterMacroAssembler::load_ptr(int n, Register val) {
  ldr(val, Address(esp, Interpreter::expr_offset_in_bytes(n)));
}

void InterpreterMacroAssembler::store_ptr(int n, Register val) {
  str(val, Address(esp, Interpreter::expr_offset_in_bytes(n)));
}

void InterpreterMacroAssembler::load_float(Address src) {
  ldrs(v0, src);
}

void InterpreterMacroAssembler::load_double(Address src) {
  ldrd(v0, src);
}

void InterpreterMacroAssembler::prepare_to_jump_from_interpreted() {
  // set sender sp
  mov(r13, sp);
  // record last_sp
  str(esp, Address(rfp, frame::interpreter_frame_last_sp_offset * wordSize));
}

// Jump to from_interpreted entry of a call unless single stepping is possible
// in this thread in which case we must call the i2i entry
void InterpreterMacroAssembler::jump_from_interpreted(Register method, Register temp) {
  prepare_to_jump_from_interpreted();

  ldr(rscratch1, Address(method, Method::from_interpreted_offset()));
  br(rscratch1);
}

// The following two routines provide a hook so that an implementation
// can schedule the dispatch in two parts.  amd64 does not do this.
void InterpreterMacroAssembler::dispatch_prolog(TosState state, int step) { }

void InterpreterMacroAssembler::dispatch_epilog(TosState state, int step) {
    dispatch_next(state, step);
}

void InterpreterMacroAssembler::dispatch_base(TosState state, address* table, bool verifyoop, bool generate_poll) {
  if (VerifyActivationFrameSize) {
    Unimplemented();
  }
  if (verifyoop) {
    verify_oop(r0, state);
  }

  Label safepoint;
  address* const safepoint_table = Interpreter::safept_table(state);
  bool needs_thread_local_poll = generate_poll && SafepointMechanism::uses_thread_local_poll() && table != safepoint_table;

  if (needs_thread_local_poll) {
    ldr(rscratch2, Address(rthread, Thread::polling_page_offset()));
    tbnz(rscratch2, exact_log2(SafepointMechanism::poll_bit()), safepoint);
  }

  if (table == Interpreter::dispatch_table(state)) {
    addw(rscratch2, rscratch1, Interpreter::distance_from_dispatch_table(state));
    ldr(rscratch2, Address(rdispatch, rscratch2, Address::uxtw(3)));
  } else {
    mov(rscratch2, (address)table);
    ldr(rscratch2, Address(rscratch2, rscratch1, Address::uxtw(3)));
  }
  br(rscratch2);

  if (needs_thread_local_poll) {
    bind(safepoint);
    lea(rscratch2, ExternalAddress((address)safepoint_table));
    ldr(rscratch2, Address(rscratch2, rscratch1, Address::uxtw(3)));
    br(rscratch2);
  }
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
  // load next bytecode
  ldrb(rscratch1, Address(pre(rbcp, step)));
  dispatch_base(state, Interpreter::dispatch_table(state), generate_poll);
}

void InterpreterMacroAssembler::dispatch_via(TosState state, address* table) {
  // load current bytecode
  ldrb(rscratch1, Address(rbcp, 0));
  dispatch_base(state, table);
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
void InterpreterMacroAssembler::remove_activation(TosState state, bool throw_monitor_exception, bool install_monitor_exception, bool notify_jvmdi) {
  // Note: Registers r3 xmm0 may be in use for the
  // result check if synchronized method
  Label unlocked, unlock, no_unlock;

  // get the value of _do_not_unlock_if_synchronized into r3
  const Address do_not_unlock_if_synchronized(rthread,
    in_bytes(JavaThread::do_not_unlock_if_synchronized_offset()));
  ldrb(r3, do_not_unlock_if_synchronized);
  strb(zr, do_not_unlock_if_synchronized); // reset the flag

 // get method access flags
  ldr(r1, Address(rfp, frame::interpreter_frame_method_offset * wordSize));
  ldr(r2, Address(r1, Method::access_flags_offset()));
  tbz(r2, exact_log2(JVM_ACC_SYNCHRONIZED), unlocked);

  // Don't unlock anything if the _do_not_unlock_if_synchronized flag
  // is set.
  cbnz(r3, no_unlock);

  // unlock monitor
  push(state); // save result

  // BasicObjectLock will be first in list, since this is a
  // synchronized method. However, need to check that the object has
  // not been unlocked by an explicit monitorexit bytecode.
  const Address monitor(rfp, frame::interpreter_frame_initial_sp_offset *
                        wordSize - (int) sizeof(BasicObjectLock));
  // We use c_rarg1 so that if we go slow path it will be the correct
  // register for unlock_object to pass to VM directly
  lea(c_rarg1, monitor); // address of first monitor

  ldr(r0, Address(c_rarg1, BasicObjectLock::obj_offset_in_bytes()));
  cbnz(r0, unlock);

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
    b(unlocked);
  }

  bind(unlock);
  unlock_object(c_rarg1);
  pop(state);

  // Check that for block-structured locking (i.e., that all locked
  // objects has been unlocked)
  bind(unlocked);

  // r0: Might contain return value

  // Check that all monitors are unlocked
  {
    Label loop, exception, entry, restart;
    const int entry_size = frame::interpreter_frame_monitor_size() * wordSize;
    const Address monitor_block_top(rfp, frame::interpreter_frame_monitor_block_top_offset * wordSize);
    const Address monitor_block_bot(rfp, frame::interpreter_frame_initial_sp_offset * wordSize);

    bind(restart);
    // We use c_rarg1 so that if we go slow path it will be the correct
    // register for unlock_object to pass to VM directly
    ldr(c_rarg1, monitor_block_top); // points to current entry, starting
                                     // with top-most entry
    lea(r19, monitor_block_bot);  // points to word before bottom of
                                  // monitor block
    b(entry);

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
      unlock_object(c_rarg1);
      pop(state);

      if (install_monitor_exception) {
        call_VM(noreg, CAST_FROM_FN_PTR(address, InterpreterRuntime::new_illegal_monitor_state_exception));
      }

      b(restart);
    }

    bind(loop);
    // check if current entry is used
    ldr(rscratch1, Address(c_rarg1, BasicObjectLock::obj_offset_in_bytes()));
    cbnz(rscratch1, exception);

    add(c_rarg1, c_rarg1, entry_size); // otherwise advance to next entry
    bind(entry);
    cmp(c_rarg1, r19); // check if bottom reached
    br(Assembler::NE, loop); // if not at bottom then check this entry
  }

  bind(no_unlock);

  // jvmti support
  if (notify_jvmdi) {
    notify_method_exit(state, NotifyJVMTI);    // preserve TOSCA
  } else {
    notify_method_exit(state, SkipNotifyJVMTI); // preserve TOSCA
  }

  // remove activation
  // get sender esp
  ldr(esp,
      Address(rfp, frame::interpreter_frame_sender_sp_offset * wordSize));
  if (StackReservedPages > 0) {
    // testing if reserved zone needs to be re-enabled
    Label no_reserved_zone_enabling;

    ldr(rscratch1, Address(rthread, JavaThread::reserved_stack_activation_offset()));
    cmp(esp, rscratch1);
    br(Assembler::LS, no_reserved_zone_enabling);

    call_VM_leaf(CAST_FROM_FN_PTR(address, SharedRuntime::enable_stack_reserved_zone), rthread);
    call_VM(noreg, CAST_FROM_FN_PTR(address, InterpreterRuntime::throw_delayed_StackOverflowError));
    should_not_reach_here();

    bind(no_reserved_zone_enabling);
  }
  // remove frame anchor
  leave();
  // If we're returning to interpreted code we will shortly be
  // adjusting SP to allow some space for ESP.  If we're returning to
  // compiled code the saved sender SP was saved in sender_sp, so this
  // restores it.
  andr(sp, esp, -16);
}

// Lock object
//
// Args:
//      c_rarg1: BasicObjectLock to be used for locking
//
// Kills:
//      r0
//      c_rarg0, c_rarg1, c_rarg2, c_rarg3, .. (param regs)
//      rscratch1, rscratch2 (scratch regs)
void InterpreterMacroAssembler::lock_object(Register lock_reg)
{
  if (UseHeavyMonitors) {
    call_VM(noreg, CAST_FROM_FN_PTR(address, InterpreterRuntime::monitorenter), lock_reg);
  } else {
    Label done;

    const Register swap_reg = r0;
    const Register tmp = c_rarg2;
    const Register obj_reg = c_rarg3; // Will contain the oop

    const int obj_offset = BasicObjectLock::obj_offset_in_bytes();
    const int lock_offset = BasicObjectLock::lock_offset_in_bytes ();
    const int mark_offset = lock_offset + BasicLock::displaced_header_offset_in_bytes();

    Label slow_case;

    // Load object pointer into obj_reg %c_rarg3
    ldr(obj_reg, Address(lock_reg, obj_offset));

    if (UseBiasedLocking) {
      biased_locking_enter(lock_reg, obj_reg, swap_reg, tmp, false, done, &slow_case);
    }

    // Load (object->mark() | 1) into swap_reg
    ldr(rscratch1, Address(obj_reg, oopDesc::mark_offset_in_bytes()));
    orr(swap_reg, rscratch1, 1);

    // Save (object->mark() | 1) into BasicLock's displaced header
    str(swap_reg, Address(lock_reg, mark_offset));

    Label fail;
    if (PrintBiasedLockingStatistics) {
      Label fast;
      cmpxchg_obj_header(swap_reg, lock_reg, obj_reg, rscratch1, fast, &fail);
      bind(fast);
      atomic_incw(Address((address)BiasedLocking::fast_path_entry_count_addr()),
                  rscratch2, rscratch1, tmp);
      b(done);
      bind(fail);
    } else {
      cmpxchg_obj_header(swap_reg, lock_reg, obj_reg, rscratch1, done, /*fallthrough*/NULL);
    }

    // Test if the oopMark is an obvious stack pointer, i.e.,
    //  1) (mark & 7) == 0, and
    //  2) rsp <= mark < mark + os::pagesize()
    //
    // These 3 tests can be done by evaluating the following
    // expression: ((mark - rsp) & (7 - os::vm_page_size())),
    // assuming both stack pointer and pagesize have their
    // least significant 3 bits clear.
    // NOTE: the oopMark is in swap_reg %r0 as the result of cmpxchg
    // NOTE2: aarch64 does not like to subtract sp from rn so take a
    // copy
    mov(rscratch1, sp);
    sub(swap_reg, swap_reg, rscratch1);
    ands(swap_reg, swap_reg, (unsigned long)(7 - os::vm_page_size()));

    // Save the test result, for recursive case, the result is zero
    str(swap_reg, Address(lock_reg, mark_offset));

    if (PrintBiasedLockingStatistics) {
      br(Assembler::NE, slow_case);
      atomic_incw(Address((address)BiasedLocking::fast_path_entry_count_addr()),
                  rscratch2, rscratch1, tmp);
    }
    br(Assembler::EQ, done);

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
//      c_rarg1: BasicObjectLock for lock
//
// Kills:
//      r0
//      c_rarg0, c_rarg1, c_rarg2, c_rarg3, ... (param regs)
//      rscratch1, rscratch2 (scratch regs)
void InterpreterMacroAssembler::unlock_object(Register lock_reg)
{
  if (UseHeavyMonitors) {
    call_VM(noreg, CAST_FROM_FN_PTR(address, InterpreterRuntime::monitorexit), lock_reg);
  } else {
    Label done;

    const Register swap_reg   = r0;
    const Register header_reg = c_rarg2;  // Will contain the old oopMark
    const Register obj_reg    = c_rarg3;  // Will contain the oop

    save_bcp(); // Save in case of exception

    // Convert from BasicObjectLock structure to object and BasicLock
    // structure Store the BasicLock address into %r0
    lea(swap_reg, Address(lock_reg, BasicObjectLock::lock_offset_in_bytes()));

    // Load oop into obj_reg(%c_rarg3)
    ldr(obj_reg, Address(lock_reg, BasicObjectLock::obj_offset_in_bytes()));

    // Free entry
    str(zr, Address(lock_reg, BasicObjectLock::obj_offset_in_bytes()));

    if (UseBiasedLocking) {
      biased_locking_exit(obj_reg, header_reg, done);
    }

    // Load the old header from BasicLock structure
    ldr(header_reg, Address(swap_reg,
                            BasicLock::displaced_header_offset_in_bytes()));

    // Test for recursion
    cbz(header_reg, done);

    // Atomic swap back the old header
    cmpxchg_obj_header(swap_reg, header_reg, obj_reg, rscratch1, done, /*fallthrough*/NULL);

    // Call the runtime routine for slow case.
    str(obj_reg, Address(lock_reg, BasicObjectLock::obj_offset_in_bytes())); // restore obj
    call_VM(noreg, CAST_FROM_FN_PTR(address, InterpreterRuntime::monitorexit), lock_reg);

    bind(done);

    restore_bcp();
  }
}

void InterpreterMacroAssembler::test_method_data_pointer(Register mdp, Label& zero_continue) {
  ldr(mdp, Address(rfp, frame::interpreter_frame_mdp_offset * wordSize));
  cbz(mdp, zero_continue);
}

// Set the method data pointer for the current bcp.
void InterpreterMacroAssembler::set_method_data_pointer_for_bcp() {
  Label set_mdp;
  stp(r0, r1, Address(pre(sp, -2 * wordSize)));

  // Test MDO to avoid the call if it is NULL.
  ldr(r0, Address(rmethod, in_bytes(Method::method_data_offset())));
  cbz(r0, set_mdp);
  call_VM_leaf(CAST_FROM_FN_PTR(address, InterpreterRuntime::bcp_to_di), rmethod, rbcp);
  // r0: mdi
  // mdo is guaranteed to be non-zero here, we checked for it before the call.
  ldr(r1, Address(rmethod, in_bytes(Method::method_data_offset())));
  lea(r1, Address(r1, in_bytes(MethodData::data_offset())));
  add(r0, r1, r0);
  str(r0, Address(rfp, frame::interpreter_frame_mdp_offset * wordSize));
  bind(set_mdp);
  ldp(r0, r1, Address(post(sp, 2 * wordSize)));
}

void InterpreterMacroAssembler::verify_method_data_pointer() { }

void InterpreterMacroAssembler::set_mdp_data_at(Register mdp_in, int constant, Register value) {
  Address data(mdp_in, constant);
  str(value, data);
}

void InterpreterMacroAssembler::increment_mdp_data_at(Register mdp_in, int constant, bool decrement) {
  increment_mdp_data_at(mdp_in, noreg, constant, decrement);
}

void InterpreterMacroAssembler::increment_mdp_data_at(Register mdp_in, Register reg, int constant, bool decrement) {
  Address addr1(mdp_in, constant);
  Address addr2(rscratch2, reg, Address::lsl(0));
  Address &addr = addr1;
  if (reg != noreg) {
    lea(rscratch2, addr1);
    addr = addr2;
  }

  if (decrement) {
    // Decrement the register.  Set condition codes.
    // Intel does this
    // addptr(data, (int32_t) -DataLayout::counter_increment);
    // If the decrement causes the counter to overflow, stay negative
    // Label L;
    // jcc(Assembler::negative, L);
    // addptr(data, (int32_t) DataLayout::counter_increment);
    // so we do this
    ldr(rscratch1, addr);
    subs(rscratch1, rscratch1, (unsigned)DataLayout::counter_increment);
    Label L;
    br(Assembler::LO, L);       // skip store if counter underflow
    str(rscratch1, addr);
    bind(L);
  } else {
    // Intel does this
    // Increment the register.  Set carry flag.
    // addptr(data, DataLayout::counter_increment);
    // If the increment causes the counter to overflow, pull back by 1.
    // sbbptr(data, (int32_t)0);
    // so we do this
    ldr(rscratch1, addr);
    adds(rscratch1, rscratch1, DataLayout::counter_increment);
    Label L;
    br(Assembler::CS, L);       // skip store if counter overflow
    str(rscratch1, addr);
    bind(L);
  }
}

void InterpreterMacroAssembler::set_mdp_flag_at(Register mdp_in, int flag_byte_constant) {
  int flags_offset = in_bytes(DataLayout::flags_offset());
  // Set the flag
  ldrb(rscratch1, Address(mdp_in, flags_offset));
  orr(rscratch1, rscratch1, flag_byte_constant);
  strb(rscratch1, Address(mdp_in, flags_offset));
}

void InterpreterMacroAssembler::test_mdp_data_at(Register mdp_in, int offset, Register value, Register test_value_out, Label& not_equal_continue) {
  if (test_value_out == noreg) {
    ldr(rscratch1, Address(mdp_in, offset));
    cmp(value, rscratch1);
  } else {
    // Put the test value into a register, so caller can use it:
    ldr(test_value_out, Address(mdp_in, offset));
    cmp(value, test_value_out);
  }
  br(Assembler::NE, not_equal_continue);
}

void InterpreterMacroAssembler::update_mdp_by_offset(Register mdp_in, int offset_of_disp) {
  ldr(rscratch1, Address(mdp_in, offset_of_disp));
  add(mdp_in, mdp_in, rscratch1, LSL);
  str(mdp_in, Address(rfp, frame::interpreter_frame_mdp_offset * wordSize));
}

void InterpreterMacroAssembler::update_mdp_by_offset(Register mdp_in, Register reg, int offset_of_disp) {
  lea(rscratch1, Address(mdp_in, offset_of_disp));
  ldr(rscratch1, Address(rscratch1, reg, Address::lsl(0)));
  add(mdp_in, mdp_in, rscratch1, LSL);
  str(mdp_in, Address(rfp, frame::interpreter_frame_mdp_offset * wordSize));
}

void InterpreterMacroAssembler::update_mdp_by_constant(Register mdp_in, int constant) {
  add(mdp_in, mdp_in, (unsigned)constant);
  str(mdp_in, Address(rfp, frame::interpreter_frame_mdp_offset * wordSize));
}

void InterpreterMacroAssembler::update_mdp_for_ret(Register return_bci) {
  // save/restore across call_VM
  stp(zr, return_bci, Address(pre(sp, -2 * wordSize)));
  call_VM(noreg, CAST_FROM_FN_PTR(address, InterpreterRuntime::update_mdp_for_ret), return_bci);
  ldp(zr, return_bci, Address(post(sp, 2 * wordSize)));
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
    ldr(bumped_count, data);
    // Intel does this to catch overflow
    // addptr(bumped_count, DataLayout::counter_increment);
    // sbbptr(bumped_count, 0);
    // so we do this
    adds(bumped_count, bumped_count, DataLayout::counter_increment);
    Label L;
    br(Assembler::CS, L);       // skip store if counter overflow
    str(bumped_count, data);
    bind(L);
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
      // We are making a call.  Increment the count for null receiver.
      increment_mdp_data_at(mdp, in_bytes(CounterData::count_offset()));
      b(skip_receiver_profile);
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
    b(done);
    bind(next_test);

    if (test_for_null_also) {
      Label found_null;
      // Failed the equality check on item[n]...  Test for null.
      if (start_row == last_row) {
        // The only thing left to do is handle the null case.
        if (non_profiled_offset >= 0) {
          cbz(reg2, found_null);
          // Item did not match any saved item and there is no empty row for it.
          // Increment total counter to indicate polymorphic case.
          increment_mdp_data_at(mdp, non_profiled_offset);
          b(done);
          bind(found_null);
        } else {
          cbnz(reg2, done);
        }
        break;
      }
      // Since null is rare, make it be the branch-taken case.
      cbz(reg2, found_null);

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
  mov(reg2, DataLayout::counter_increment);
  set_mdp_data_at(mdp, count_offset, reg2);
  if (start_row > 0) {
    b(done);
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
      b(profile_continue);
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
    movw(reg2, in_bytes(MultiBranchData::per_case_size()));
    movw(rscratch1, in_bytes(MultiBranchData::case_array_offset()));
    Assembler::maddw(index, index, reg2, rscratch1);

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

void InterpreterMacroAssembler::notify_method_entry() { }

void InterpreterMacroAssembler::notify_method_exit(TosState state, NotifyMethodExitMode mode) { }

// Jump if ((*counter_addr += increment) & mask) satisfies the condition.
void InterpreterMacroAssembler::increment_mask_and_jump(Address counter_addr, int increment, Address mask, Register scratch, Register scratch2, bool preloaded, Condition cond, Label* where) {
  if (!preloaded) {
    ldrw(scratch, counter_addr);
  }
  add(scratch, scratch, increment);
  strw(scratch, counter_addr);
  ldrw(scratch2, mask);
  ands(scratch, scratch, scratch2);
  br(cond, *where);
}

void InterpreterMacroAssembler::call_VM_leaf_base(address entry_point, int number_of_arguments) {
  // interpreter specific
  //
  // Note: No need to save/restore rbcp & rlocals pointer since these
  //       are callee saved registers and no blocking/ GC can happen
  //       in leaf calls.
  // super call
  MacroAssembler::call_VM_leaf_base(entry_point, number_of_arguments);
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

void InterpreterMacroAssembler::profile_obj_type(Register obj, const Address& mdo_addr) {
  Label update, next, none;

  verify_oop(obj);

  cbnz(obj, update);
  orptr(mdo_addr, TypeEntries::null_seen);
  b(next);

  bind(update);
  load_klass(obj, obj);

  ldr(rscratch1, mdo_addr);
  eor(obj, obj, rscratch1);
  tst(obj, TypeEntries::type_klass_mask);
  br(Assembler::EQ, next); // klass seen before, nothing to
                           // do. The unknown bit may have been
                           // set already but no need to check.

  tbnz(obj, exact_log2(TypeEntries::type_unknown), next);
  // already unknown. Nothing to do anymore.

  ldr(rscratch1, mdo_addr);
  cbz(rscratch1, none);
  cmp(rscratch1, TypeEntries::null_seen);
  br(Assembler::EQ, none);
  // There is a chance that the checks above (re-reading profiling
  // data from memory) fail if another thread has just set the
  // profiling to this obj's klass
  ldr(rscratch1, mdo_addr);
  eor(obj, obj, rscratch1);
  tst(obj, TypeEntries::type_klass_mask);
  br(Assembler::EQ, next);

  // different than before. Cannot keep accurate profile.
  orptr(mdo_addr, TypeEntries::type_unknown);
  b(next);

  bind(none);
  // first time here. Set profile type.
  str(obj, mdo_addr);

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

    ldrb(rscratch1, Address(mdp, in_bytes(DataLayout::tag_offset()) - off_to_start));
    cmp(rscratch1, is_virtual ? DataLayout::virtual_call_type_data_tag : DataLayout::call_type_data_tag);
    br(Assembler::NE, profile_continue);

    if (MethodData::profile_arguments()) {
      Label done;
      int off_to_args = in_bytes(TypeEntriesAtCall::args_data_offset());

      for (int i = 0; i < TypeProfileArgsLimit; i++) {
        if (i > 0 || MethodData::profile_return()) {
          // If return value type is profiled we may have no argument to profile
          ldr(tmp, Address(mdp, in_bytes(TypeEntriesAtCall::cell_count_offset())));
          sub(tmp, tmp, i*TypeStackSlotEntries::per_arg_count());
          cmp(tmp, TypeStackSlotEntries::per_arg_count());
          add(rscratch1, mdp, off_to_args);
          br(Assembler::LT, done);
        }
        ldr(tmp, Address(callee, Method::const_offset()));
        load_unsigned_short(tmp, Address(tmp, ConstMethod::size_of_parameters_offset()));
        // stack offset o (zero based) from the start of the argument
        // list, for n arguments translates into offset n - o - 1 from
        // the end of the argument list
        ldr(rscratch1, Address(mdp, in_bytes(TypeEntriesAtCall::stack_slot_offset(i))));
        sub(tmp, tmp, rscratch1);
        sub(tmp, tmp, 1);
        Address arg_addr = argument_address(tmp);
        ldr(tmp, arg_addr);

        Address mdo_arg_addr(mdp, in_bytes(TypeEntriesAtCall::argument_type_offset(i)));
        profile_obj_type(tmp, mdo_arg_addr);

        int to_add = in_bytes(TypeStackSlotEntries::per_arg_size());
        off_to_args += to_add;
      }

      if (MethodData::profile_return()) {
        ldr(tmp, Address(mdp, in_bytes(TypeEntriesAtCall::cell_count_offset())));
        sub(tmp, tmp, TypeProfileArgsLimit*TypeStackSlotEntries::per_arg_count());
      }

      add(rscratch1, mdp, off_to_args);
      bind(done);
      mov(mdp, rscratch1);

      if (MethodData::profile_return()) {
        // We're right after the type profile for the last
        // argument. tmp is the number of cells left in the
        // CallTypeData/VirtualCallTypeData to reach its end. Non null
        // if there's a return to profile.
        add(mdp, mdp, tmp, LSL, exact_log2(DataLayout::cell_size));
      }
      str(mdp, Address(rfp, frame::interpreter_frame_mdp_offset * wordSize));
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
      ldrb(rscratch1, Address(rbcp, 0));
      cmp(rscratch1, Bytecodes::_invokedynamic);
      br(Assembler::EQ, do_profile);
      cmp(rscratch1, Bytecodes::_invokehandle);
      br(Assembler::EQ, do_profile);
      get_method(tmp);
      ldrh(rscratch1, Address(tmp, Method::intrinsic_id_offset_in_bytes()));
      cmp(rscratch1, vmIntrinsics::_compiledLambdaForm);
      br(Assembler::NE, profile_continue);

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
    ldrw(tmp1, Address(mdp, in_bytes(MethodData::parameters_type_data_di_offset()) - in_bytes(MethodData::data_offset())));
    tbnz(tmp1, 31, profile_continue);  // i.e. sign bit set

    // Compute a pointer to the area for parameters from the offset
    // and move the pointer to the slot for the last
    // parameters. Collect profiling from last parameter down.
    // mdo start + parameters offset + array length - 1
    add(mdp, mdp, tmp1);
    ldr(tmp1, Address(mdp, ArrayData::array_len_offset()));
    sub(tmp1, tmp1, TypeStackSlotEntries::per_arg_count());

    Label loop;
    bind(loop);

    int off_base = in_bytes(ParametersTypeData::stack_slot_offset(0));
    int type_base = in_bytes(ParametersTypeData::type_offset(0));
    int per_arg_scale = exact_log2(DataLayout::cell_size);
    add(rscratch1, mdp, off_base);
    add(rscratch2, mdp, type_base);

    Address arg_off(rscratch1, tmp1, Address::lsl(per_arg_scale));
    Address arg_type(rscratch2, tmp1, Address::lsl(per_arg_scale));

    // load offset on the stack from the slot for this parameter
    ldr(tmp2, arg_off);
    neg(tmp2, tmp2);
    // read the parameter from the local area
    ldr(tmp2, Address(rlocals, tmp2, Address::lsl(Interpreter::logStackElementSize)));

    // profile the parameter
    profile_obj_type(tmp2, arg_type);

    // go to next parameter
    subs(tmp1, tmp1, TypeStackSlotEntries::per_arg_count());
    br(Assembler::GE, loop);

    bind(profile_continue);
  }
}
