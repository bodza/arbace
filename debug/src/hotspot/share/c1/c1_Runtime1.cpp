#include "precompiled.hpp"

#include "asm/codeBuffer.hpp"
#include "c1/c1_CodeStubs.hpp"
#include "c1/c1_Defs.hpp"
#include "c1/c1_FrameMap.hpp"
#include "c1/c1_LIRAssembler.hpp"
#include "c1/c1_MacroAssembler.hpp"
#include "c1/c1_Runtime1.hpp"
#include "classfile/systemDictionary.hpp"
#include "classfile/vmSymbols.hpp"
#include "code/codeBlob.hpp"
#include "code/compiledIC.hpp"
#include "code/pcDesc.hpp"
#include "code/scopeDesc.hpp"
#include "code/vtableStubs.hpp"
#include "compiler/disassembler.hpp"
#include "gc/shared/barrierSet.hpp"
#include "gc/shared/c1/barrierSetC1.hpp"
#include "gc/shared/collectedHeap.hpp"
#include "interpreter/bytecode.hpp"
#include "memory/allocation.inline.hpp"
#include "memory/oopFactory.hpp"
#include "memory/resourceArea.hpp"
#include "oops/access.inline.hpp"
#include "oops/objArrayOop.inline.hpp"
#include "oops/objArrayKlass.hpp"
#include "oops/oop.inline.hpp"
#include "runtime/atomic.hpp"
#include "runtime/biasedLocking.hpp"
#include "runtime/compilationPolicy.hpp"
#include "runtime/interfaceSupport.inline.hpp"
#include "runtime/frame.inline.hpp"
#include "runtime/javaCalls.hpp"
#include "runtime/sharedRuntime.hpp"
#include "runtime/threadCritical.hpp"
#include "runtime/vframe.inline.hpp"
#include "runtime/vframeArray.hpp"
#include "runtime/vm_version.hpp"
#include "utilities/copy.hpp"

// Implementation of StubAssembler

StubAssembler::StubAssembler(CodeBuffer* code, const char * name, int stub_id) : C1_MacroAssembler(code) {
  _name = name;
  _must_gc_arguments = false;
  _frame_size = no_frame_size;
  _num_rt_args = 0;
  _stub_id = stub_id;
}

void StubAssembler::set_info(const char* name, bool must_gc_arguments) {
  _name = name;
  _must_gc_arguments = must_gc_arguments;
}

void StubAssembler::set_frame_size(int size) {
  if (_frame_size == no_frame_size) {
    _frame_size = size;
  }
}

void StubAssembler::set_num_rt_args(int args) {
  if (_num_rt_args == 0) {
    _num_rt_args = args;
  }
}

// Implementation of Runtime1

CodeBlob* Runtime1::_blobs[Runtime1::number_of_ids];
const char *Runtime1::_blob_names[] = {
  RUNTIME1_STUBS(STUB_NAME, LAST_STUB_NAME)
};

class StubIDStubAssemblerCodeGenClosure: public StubAssemblerCodeGenClosure {
 private:
  Runtime1::StubID _id;
 public:
  StubIDStubAssemblerCodeGenClosure(Runtime1::StubID id) : _id(id) { }
  virtual OopMapSet* generate_code(StubAssembler* sasm) {
    return Runtime1::generate_code_for(_id, sasm);
  }
};

CodeBlob* Runtime1::generate_blob(BufferBlob* buffer_blob, int stub_id, const char* name, bool expect_oop_map, StubAssemblerCodeGenClosure* cl) {
  ResourceMark rm;
  // create code buffer for code storage
  CodeBuffer code(buffer_blob);

  OopMapSet* oop_maps;
  int frame_size;
  bool must_gc_arguments;

  Compilation::setup_code_buffer(&code, 0);

  // create assembler for code generation
  StubAssembler* sasm = new StubAssembler(&code, name, stub_id);
  // generate code for runtime stub
  oop_maps = cl->generate_code(sasm);

  // align so printing shows nop's instead of random code at the end (SimpleStubs are aligned)
  sasm->align(BytesPerWord);
  // make sure all code is in code buffer
  sasm->flush();

  frame_size = sasm->frame_size();
  must_gc_arguments = sasm->must_gc_arguments();
  // create blob - distinguish a few special cases
  CodeBlob* blob = RuntimeStub::new_runtime_stub(name, &code, CodeOffsets::frame_never_safe, frame_size, oop_maps, must_gc_arguments);
  return blob;
}

void Runtime1::generate_blob_for(BufferBlob* buffer_blob, StubID id) {
  bool expect_oop_map = true;
  StubIDStubAssemblerCodeGenClosure cl(id);
  CodeBlob* blob = generate_blob(buffer_blob, id, name_for(id), expect_oop_map, &cl);
  // install blob
  _blobs[id] = blob;
}

void Runtime1::initialize(BufferBlob* blob) {
  // platform-dependent initialization
  initialize_pd();
  // generate stubs
  for (int id = 0; id < number_of_ids; id++) generate_blob_for(blob, (StubID)id);
  // printing
  BarrierSetC1* bs = BarrierSet::barrier_set()->barrier_set_c1();
  bs->generate_c1_runtime_stubs(blob);
}

CodeBlob* Runtime1::blob_for(StubID id) {
  return _blobs[id];
}

const char* Runtime1::name_for(StubID id) {
  return _blob_names[id];
}

const char* Runtime1::name_for_address(address entry) {
  for (int id = 0; id < number_of_ids; id++) {
    if (entry == entry_for((StubID)id)) return name_for((StubID)id);
  }

#define FUNCTION_CASE(a, f) \
  if ((intptr_t)a == CAST_FROM_FN_PTR(intptr_t, f))  return #f

  FUNCTION_CASE(entry, os::javaTimeMillis);
  FUNCTION_CASE(entry, os::javaTimeNanos);
  FUNCTION_CASE(entry, SharedRuntime::d2f);
  FUNCTION_CASE(entry, SharedRuntime::d2i);
  FUNCTION_CASE(entry, SharedRuntime::d2l);
  FUNCTION_CASE(entry, SharedRuntime::dcos);
  FUNCTION_CASE(entry, SharedRuntime::dexp);
  FUNCTION_CASE(entry, SharedRuntime::dlog);
  FUNCTION_CASE(entry, SharedRuntime::dlog10);
  FUNCTION_CASE(entry, SharedRuntime::dpow);
  FUNCTION_CASE(entry, SharedRuntime::drem);
  FUNCTION_CASE(entry, SharedRuntime::dsin);
  FUNCTION_CASE(entry, SharedRuntime::dtan);
  FUNCTION_CASE(entry, SharedRuntime::f2i);
  FUNCTION_CASE(entry, SharedRuntime::f2l);
  FUNCTION_CASE(entry, SharedRuntime::frem);
  FUNCTION_CASE(entry, SharedRuntime::l2d);
  FUNCTION_CASE(entry, SharedRuntime::l2f);
  FUNCTION_CASE(entry, SharedRuntime::ldiv);
  FUNCTION_CASE(entry, SharedRuntime::lmul);
  FUNCTION_CASE(entry, SharedRuntime::lrem);
  FUNCTION_CASE(entry, SharedRuntime::lrem);
  FUNCTION_CASE(entry, is_instance_of);
  FUNCTION_CASE(entry, trace_block_entry);
  FUNCTION_CASE(entry, StubRoutines::updateBytesCRC32());
  FUNCTION_CASE(entry, StubRoutines::updateBytesCRC32C());
  FUNCTION_CASE(entry, StubRoutines::vectorizedMismatch());
  FUNCTION_CASE(entry, StubRoutines::dexp());
  FUNCTION_CASE(entry, StubRoutines::dlog());
  FUNCTION_CASE(entry, StubRoutines::dlog10());
  FUNCTION_CASE(entry, StubRoutines::dpow());
  FUNCTION_CASE(entry, StubRoutines::dsin());
  FUNCTION_CASE(entry, StubRoutines::dcos());
  FUNCTION_CASE(entry, StubRoutines::dtan());

#undef FUNCTION_CASE

  // Soft float adds more runtime names.
  return pd_name_for_address(entry);
}

JRT_ENTRY(void, Runtime1::new_instance(JavaThread* thread, Klass* klass))

  Handle holder(THREAD, klass->klass_holder()); // keep the klass alive
  InstanceKlass* h = InstanceKlass::cast(klass);
  h->check_valid_for_instantiation(true, CHECK);
  // make sure klass is initialized
  h->initialize(CHECK);
  // allocate instance and return via TLS
  oop obj = h->allocate_instance(CHECK);
  thread->set_vm_result(obj);
JRT_END

JRT_ENTRY(void, Runtime1::new_type_array(JavaThread* thread, Klass* klass, jint length))
  // Note: no handle for klass needed since they are not used
  //       anymore after new_typeArray() and no GC can happen before.
  //       (This may have to change if this code changes!)
  BasicType elt_type = TypeArrayKlass::cast(klass)->element_type();
  oop obj = oopFactory::new_typeArray(elt_type, length, CHECK);
  thread->set_vm_result(obj);
JRT_END

JRT_ENTRY(void, Runtime1::new_object_array(JavaThread* thread, Klass* array_klass, jint length))

  // Note: no handle for klass needed since they are not used
  //       anymore after new_objArray() and no GC can happen before.
  //       (This may have to change if this code changes!)
  Handle holder(THREAD, array_klass->klass_holder()); // keep the klass alive
  Klass* elem_klass = ObjArrayKlass::cast(array_klass)->element_klass();
  objArrayOop obj = oopFactory::new_objArray(elem_klass, length, CHECK);
  thread->set_vm_result(obj);
JRT_END

JRT_ENTRY(void, Runtime1::new_multi_array(JavaThread* thread, Klass* klass, int rank, jint* dims))

  Handle holder(THREAD, klass->klass_holder()); // keep the klass alive
  oop obj = ArrayKlass::cast(klass)->multi_allocate(rank, dims, CHECK);
  thread->set_vm_result(obj);
JRT_END

JRT_ENTRY(void, Runtime1::unimplemented_entry(JavaThread* thread, StubID id))
  tty->print_cr("Runtime1::entry_for(%d) returned unimplemented entry point", id);
JRT_END

JRT_ENTRY(void, Runtime1::throw_array_store_exception(JavaThread* thread, oopDesc* obj))
  ResourceMark rm(thread);
  const char* klass_name = obj->klass()->external_name();
  SharedRuntime::throw_and_post_jvmti_exception(thread, vmSymbols::java_lang_ArrayStoreException(), klass_name);
JRT_END

extern void vm_exit(int code);

// Enter this method from compiled code handler below. This is where we transition
// to VM mode. This is done as a helper routine so that the method called directly
// from compiled code does not have to transition to VM. This allows the entry
// method to see if the nmethod that we have just looked up a handler for has
// been deoptimized while we were in the vm. This simplifies the assembly code
// cpu directories.
//
// We are entering here from exception stub (via the entry method below)
// If there is a compiled exception handler in this method, we will continue there;
// otherwise we will unwind the stack and continue at the caller of top frame method
// Note: we enter in Java using a special JRT wrapper. This wrapper allows us to
// control the area where we can allow a safepoint. After we exit the safepoint area we can
// check to see if the handler we are going to return is now in a nmethod that has
// been deoptimized. If that is the case we return the deopt blob
// unpack_with_exception entry instead. This makes life for the exception blob easier
// because making that same check and diverting is painful from assembly language.
JRT_ENTRY_NO_ASYNC(static address, exception_handler_for_pc_helper(JavaThread* thread, oopDesc* ex, address pc, nmethod*& nm))
  Handle exception(thread, ex);
  nm = CodeCache::find_nmethod(pc);

  // Check the stack guard pages and reenable them if necessary and there is
  // enough space on the stack to do so.  Use fast exceptions only if the guard
  // pages are enabled.
  bool guard_pages_enabled = thread->stack_guards_enabled();
  if (!guard_pages_enabled)
    guard_pages_enabled = thread->reguard_stack();

  // ExceptionCache is used only for exceptions at call sites and not for implicit exceptions
  if (guard_pages_enabled) {
    address fast_continuation = nm->handler_for_exception_and_pc(exception, pc);
    if (fast_continuation != NULL) {
      return fast_continuation;
    }
  }

  // If the stack guard pages are enabled, check whether there is a handler in
  // the current method.  Otherwise (guard pages disabled), force an unwind and
  // skip the exception cache update (i.e., just leave continuation==NULL).
  address continuation = NULL;
  if (guard_pages_enabled) {
    // New exception handling mechanism can support inlined methods
    // with exception handlers since the mappings are from PC to PC

    // for AbortVMOnException flag
    Exceptions::debug_check_abort(exception);

    // Clear out the exception oop and pc since looking up an
    // exception handler can cause class loading, which might throw an
    // exception and those fields are expected to be clear during
    // normal bytecode execution.
    thread->clear_exception_oop_and_pc();

    bool recursive_exception = false;
    continuation = SharedRuntime::compute_compiled_exc_handler(nm, pc, exception, false, false, recursive_exception);
    // If an exception was thrown during exception dispatch, the exception oop may have changed
    thread->set_exception_oop(exception());
    thread->set_exception_pc(pc);

    // the exception cache is used only by non-implicit exceptions
    // Update the exception cache only when there didn't happen
    // another exception during the computation of the compiled
    // exception handler. Checking for exception oop equality is not
    // sufficient because some exceptions are pre-allocated and reused.
    if (continuation != NULL && !recursive_exception) {
      nm->add_handler_for_exception_and_pc(exception, pc, continuation);
    }
  }

  thread->set_vm_result(exception());

  return continuation;
JRT_END

// Enter this method from compiled code only if there is a Java exception handler
// in the method handling the exception.
// We are entering here from exception stub. We don't do a normal VM transition here.
// We do it in a helper. This is so we can check to see if the nmethod we have just
// searched for an exception handler has been deoptimized in the meantime.
address Runtime1::exception_handler_for_pc(JavaThread* thread) {
  oop exception = thread->exception_oop();
  address pc = thread->exception_pc();
  // Still in Java mode
  nmethod* nm = NULL;
  address continuation = NULL;
  {
    // Enter VM mode by calling the helper
    ResetNoHandleMark rnhm;
    continuation = exception_handler_for_pc_helper(thread, exception, pc, nm);
  }
  // Back in JAVA, use no oops DON'T safepoint

  return continuation;
}

JRT_ENTRY(void, Runtime1::throw_range_check_exception(JavaThread* thread, int index, arrayOopDesc* a))
  const int len = 35;
  char message[2 * jintAsStringSize + len];
  sprintf(message, "Index %d out of bounds for length %d", index, a->length());
  SharedRuntime::throw_and_post_jvmti_exception(thread, vmSymbols::java_lang_ArrayIndexOutOfBoundsException(), message);
JRT_END

JRT_ENTRY(void, Runtime1::throw_index_exception(JavaThread* thread, int index))
  char message[16];
  sprintf(message, "%d", index);
  SharedRuntime::throw_and_post_jvmti_exception(thread, vmSymbols::java_lang_IndexOutOfBoundsException(), message);
JRT_END

JRT_ENTRY(void, Runtime1::throw_div0_exception(JavaThread* thread))
  SharedRuntime::throw_and_post_jvmti_exception(thread, vmSymbols::java_lang_ArithmeticException(), "/ by zero");
JRT_END

JRT_ENTRY(void, Runtime1::throw_null_pointer_exception(JavaThread* thread))
  SharedRuntime::throw_and_post_jvmti_exception(thread, vmSymbols::java_lang_NullPointerException());
JRT_END

JRT_ENTRY(void, Runtime1::throw_class_cast_exception(JavaThread* thread, oopDesc* object))
  ResourceMark rm(thread);
  char* message = SharedRuntime::generate_class_cast_message(thread, object->klass());
  SharedRuntime::throw_and_post_jvmti_exception(thread, vmSymbols::java_lang_ClassCastException(), message);
JRT_END

JRT_ENTRY(void, Runtime1::throw_incompatible_class_change_error(JavaThread* thread))
  ResourceMark rm(thread);
  SharedRuntime::throw_and_post_jvmti_exception(thread, vmSymbols::java_lang_IncompatibleClassChangeError());
JRT_END

JRT_ENTRY_NO_ASYNC(void, Runtime1::monitorenter(JavaThread* thread, oopDesc* obj, BasicObjectLock* lock))
  if (PrintBiasedLockingStatistics) {
    Atomic::inc(BiasedLocking::slow_path_entry_count_addr());
  }
  Handle h_obj(thread, obj);
  if (UseBiasedLocking) {
    // Retry fast entry if bias is revoked to avoid unnecessary inflation
    ObjectSynchronizer::fast_enter(h_obj, lock->lock(), true, CHECK);
  } else {
    if (UseFastLocking) {
      // When using fast locking, the compiled code has already tried the fast case
      ObjectSynchronizer::slow_enter(h_obj, lock->lock(), THREAD);
    } else {
      lock->set_obj(obj);
      ObjectSynchronizer::fast_enter(h_obj, lock->lock(), false, THREAD);
    }
  }
JRT_END

JRT_LEAF(void, Runtime1::monitorexit(JavaThread* thread, BasicObjectLock* lock))
  // monitorexit is non-blocking (leaf routine) => no exceptions can be thrown
  EXCEPTION_MARK;

  oop obj = lock->obj();
  if (UseFastLocking) {
    // When using fast locking, the compiled code has already tried the fast case
    ObjectSynchronizer::slow_exit(obj, lock->lock(), THREAD);
  } else {
    ObjectSynchronizer::fast_exit(obj, lock->lock(), THREAD);
  }
JRT_END

#ifndef DEOPTIMIZE_WHEN_PATCHING

static Klass* resolve_field_return_klass(const methodHandle& caller, int bci, TRAPS) {
  Bytecode_field field_access(caller, bci);
  // This can be static or non-static field access
  Bytecodes::Code code = field_access.code();

  // We must load class, initialize class and resolve the field
  fieldDescriptor result; // initialize class if needed
  constantPoolHandle constants(THREAD, caller->constants());
  LinkResolver::resolve_field_access(result, constants, field_access.index(), caller, Bytecodes::java_code(code), CHECK_NULL);
  return result.field_holder();
}

//
// This routine patches sites where a class wasn't loaded or
// initialized at the time the code was generated.  It handles
// references to classes, fields and forcing of initialization.  Most
// of the cases are straightforward and involving simply forcing
// resolution of a class, rewriting the instruction stream with the
// needed constant and replacing the call in this function with the
// patched code.  The case for static field is more complicated since
// the thread which is in the process of initializing a class can
// access it's static fields but other threads can't so the code
// either has to deoptimize when this case is detected or execute a
// check that the current thread is the initializing thread.  The
// current
//
// Patches basically look like this:
//
//
// patch_site: jmp patch stub     ;; will be patched
// continue:   ...
//             ...
//             ...
//             ...
//
// They have a stub which looks like this:
//
//             ;; patch body
//             movl <const>, reg           (for class constants)
//        <or> movl [reg1 + <const>], reg  (for field offsets)
//        <or> movl reg, [reg1 + <const>]  (for field offsets)
//             <being_init offset> <bytes to copy> <bytes to skip>
// patch_stub: call Runtime1::patch_code (through a runtime stub)
//             jmp patch_site
//
//
// A normal patch is done by rewriting the patch body, usually a move,
// and then copying it into place over top of the jmp instruction
// being careful to flush caches and doing it in an MP-safe way.  The
// constants following the patch body are used to find various pieces
// of the patch relative to the call site for Runtime1::patch_code.
// The case for getstatic and putstatic is more complicated because
// getstatic and putstatic have special semantics when executing while
// the class is being initialized.  getstatic/putstatic on a class
// which is being_initialized may be executed by the initializing
// thread but other threads have to block when they execute it.  This
// is accomplished in compiled code by executing a test of the current
// thread against the initializing thread of the class.  It's emitted
// as boilerplate in their stub which allows the patched code to be
// executed before it's copied back into the main body of the nmethod.
//
// being_init: get_thread(<tmp reg>
//             cmpl [reg1 + <init_thread_offset>], <tmp reg>
//             jne patch_stub
//             movl [reg1 + <const>], reg  (for field offsets)  <or>
//             movl reg, [reg1 + <const>]  (for field offsets)
//             jmp continue
//             <being_init offset> <bytes to copy> <bytes to skip>
// patch_stub: jmp Runtim1::patch_code (through a runtime stub)
//             jmp patch_site
//
// If the class is being initialized the patch body is rewritten and
// the patch site is rewritten to jump to being_init, instead of
// patch_stub.  Whenever this code is executed it checks the current
// thread against the intializing thread so other threads will enter
// the runtime and end up blocked waiting the class to finish
// initializing inside the calls to resolve_field below.  The
// initializing class will continue on it's way.  Once the class is
// fully_initialized, the intializing_thread of the class becomes
// NULL, so the next thread to execute this code will fail the test,
// call into patch_code and complete the patching process by copying
// the patch body back into the main part of the nmethod and resume
// executing.
//
//

JRT_ENTRY(void, Runtime1::patch_code(JavaThread* thread, Runtime1::StubID stub_id ))

  ResourceMark rm(thread);
  RegisterMap reg_map(thread, false);
  frame runtime_frame = thread->last_frame();
  frame caller_frame = runtime_frame.sender(&reg_map);

  // last java frame on stack
  vframeStream vfst(thread, true);

  methodHandle caller_method(THREAD, vfst.method());
  // Note also that in the presence of inlining it is not guaranteed
  // that caller_method() == caller_code->method()

  int bci = vfst.bci();
  Bytecodes::Code code = caller_method()->java_code_at(bci);

  // this is used by assertions in the access_field_patching_id
  BasicType patch_field_type = T_ILLEGAL;
  int patch_field_offset = -1;
  Klass* init_klass = NULL;      // klass needed by load_klass_patching code
  Klass* load_klass = NULL;      // klass needed by load_klass_patching code
  Handle mirror(THREAD, NULL);   // oop needed by load_mirror_patching code
  Handle appendix(THREAD, NULL); // oop needed by appendix_patching code
  bool load_klass_or_mirror_patch_id = (stub_id == Runtime1::load_klass_patching_id || stub_id == Runtime1::load_mirror_patching_id);

  if (stub_id == Runtime1::access_field_patching_id) {
    Bytecode_field field_access(caller_method, bci);
    fieldDescriptor result; // initialize class if needed
    Bytecodes::Code code = field_access.code();
    constantPoolHandle constants(THREAD, caller_method->constants());
    LinkResolver::resolve_field_access(result, constants, field_access.index(), caller_method, Bytecodes::java_code(code), CHECK);
    patch_field_offset = result.offset();

    // If we're patching a field which is volatile then at compile it
    // must not have been know to be volatile, so the generated code
    // isn't correct for a volatile reference.  The nmethod has to be
    // deoptimized so that the code can be regenerated correctly.
    // This check is only needed for access_field_patching since this
    // is the path for patching field offsets.  load_klass is only
    // used for patching references to oops which don't need special
    // handling in the volatile case.

    false = result.access_flags().is_volatile();

    // If we are patching a field which should be atomic, then
    // the generated code is not correct either, force deoptimizing.
    // We need to only cover T_LONG and T_DOUBLE fields, as we can
    // break access atomicity only for them.

    // Strictly speaking, the deoptimizaation on 64-bit platforms
    // is unnecessary, and T_LONG stores on 32-bit platforms need
    // to be handled by special patching code when AlwaysAtomicAccesses
    // becomes product feature. At this point, we are still going
    // for the deoptimization for consistency against volatile
    // accesses.

    patch_field_type = result.field_type();
    false = (AlwaysAtomicAccesses && (patch_field_type == T_DOUBLE || patch_field_type == T_LONG));
  } else if (load_klass_or_mirror_patch_id) {
    Klass* k = NULL;
    switch (code) {
      case Bytecodes::_putstatic:
      case Bytecodes::_getstatic:
        { Klass* klass = resolve_field_return_klass(caller_method, bci, CHECK);
          init_klass = klass;
          mirror = Handle(THREAD, klass->java_mirror());
        }
        break;
      case Bytecodes::_new:
        { Bytecode_new bnew(caller_method(), caller_method->bcp_from(bci));
          k = caller_method->constants()->klass_at(bnew.index(), CHECK);
        }
        break;
      case Bytecodes::_multianewarray:
        { Bytecode_multianewarray mna(caller_method(), caller_method->bcp_from(bci));
          k = caller_method->constants()->klass_at(mna.index(), CHECK);
        }
        break;
      case Bytecodes::_instanceof:
        { Bytecode_instanceof io(caller_method(), caller_method->bcp_from(bci));
          k = caller_method->constants()->klass_at(io.index(), CHECK);
        }
        break;
      case Bytecodes::_checkcast:
        { Bytecode_checkcast cc(caller_method(), caller_method->bcp_from(bci));
          k = caller_method->constants()->klass_at(cc.index(), CHECK);
        }
        break;
      case Bytecodes::_anewarray:
        { Bytecode_anewarray anew(caller_method(), caller_method->bcp_from(bci));
          Klass* ek = caller_method->constants()->klass_at(anew.index(), CHECK);
          k = ek->array_klass(CHECK);
        }
        break;
      case Bytecodes::_ldc:
      case Bytecodes::_ldc_w:
        {
          Bytecode_loadconstant cc(caller_method, bci);
          oop m = cc.resolve_constant(CHECK);
          mirror = Handle(THREAD, m);
        }
        break;
      default: fatal("unexpected bytecode for load_klass_or_mirror_patch_id");
    }
    load_klass = k;
  } else {
    ShouldNotReachHere();
  }

  // Now copy code back

  { MutexLockerEx ml_patch(Patching_lock, Mutex::_no_safepoint_check_flag);
    // NULL may have happened while we waited for the lock.
    // In that case we don't bother to do any patching we just return
    // and let the deopt happen
    //
    NativeGeneralJump* jump = nativeGeneralJump_at(caller_frame.pc());
    address instr_pc = jump->jump_destination();
    NativeInstruction* ni = nativeInstruction_at(instr_pc);
    if (ni->is_jump()) {
      // the jump has not been patched yet
      // The jump destination is slow case and therefore not part of the stubs
      // (stubs are only for StaticCalls)

      // format of buffer
      //    ....
      //    instr byte 0     <-- copy_buff
      //    instr byte 1
      //    ..
      //    instr byte n-1
      //      n
      //    ....             <-- call destination

      address stub_location = caller_frame.pc() + PatchingStub::patch_info_offset();
      unsigned char* byte_count = (unsigned char*) (stub_location - 1);
      unsigned char* byte_skip = (unsigned char*) (stub_location - 2);
      unsigned char* being_initialized_entry_offset = (unsigned char*) (stub_location - 3);
      address copy_buff = stub_location - *byte_skip - *byte_count;
      address being_initialized_entry = stub_location - *being_initialized_entry_offset;
      // depending on the code below, do_patch says whether to copy the patch body back into the nmethod
      bool do_patch = true;
      if (stub_id == Runtime1::access_field_patching_id) {
        // The offset may not be correct if the class was not loaded at code generation time.
        // Set it now.
        NativeMovRegMem* n_move = nativeMovRegMem_at(copy_buff);
        n_move->add_offset_in_bytes(patch_field_offset);
      } else if (load_klass_or_mirror_patch_id) {
        // If a getstatic or putstatic is referencing a klass which
        // isn't fully initialized, the patch body isn't copied into
        // place until initialization is complete.  In this case the
        // patch site is setup so that any threads besides the
        // initializing thread are forced to come into the VM and
        // block.
        do_patch = (code != Bytecodes::_getstatic && code != Bytecodes::_putstatic) || InstanceKlass::cast(init_klass)->is_initialized();
        NativeGeneralJump* jump = nativeGeneralJump_at(instr_pc);
        if (jump->jump_destination() == being_initialized_entry) {
        } else {
          // patch the instruction <move reg, klass>
          NativeMovConstReg* n_copy = nativeMovConstReg_at(copy_buff);

          if (stub_id == Runtime1::load_klass_patching_id) {
            n_copy->set_data((intx) (load_klass));
          } else {
            // Don't need a G1 pre-barrier here since we assert above that data isn't an oop.
            n_copy->set_data(cast_from_oop<intx>(mirror()));
          }
        }
      } else {
        ShouldNotReachHere();
      }

      if (do_patch) {
        // replace instructions
        // first replace the tail, then the call
#ifdef ARM
        if (load_klass_or_mirror_patch_id && nativeMovConstReg_at(copy_buff)->is_pc_relative()) {
          nmethod* nm = CodeCache::find_nmethod(instr_pc);
          address addr = NULL;
          RelocIterator mds(nm, copy_buff, copy_buff + 1);
          while (mds.next()) {
            if (mds.type() == relocInfo::oop_type) {
              oop_Relocation* r = mds.oop_reloc();
              addr = (address)r->oop_addr();
              break;
            } else if (mds.type() == relocInfo::metadata_type) {
              metadata_Relocation* r = mds.metadata_reloc();
              addr = (address)r->metadata_addr();
              break;
            }
          }
          copy_buff -= *byte_count;
          NativeMovConstReg* n_copy2 = nativeMovConstReg_at(copy_buff);
          n_copy2->set_pc_relative_offset(addr, instr_pc);
        }
#endif

        for (int i = NativeGeneralJump::instruction_size; i < *byte_count; i++) {
          address ptr = copy_buff + i;
          int a_byte = (*ptr) & 0xFF;
          address dst = instr_pc + i;
          *(unsigned char*)dst = (unsigned char) a_byte;
        }
        ICache::invalidate_range(instr_pc, *byte_count);
        NativeGeneralJump::replace_mt_safe(instr_pc, copy_buff);

        if (load_klass_or_mirror_patch_id) {
          relocInfo::relocType rtype = (stub_id == Runtime1::load_klass_patching_id) ? relocInfo::metadata_type : relocInfo::oop_type;
          // update relocInfo to metadata
          nmethod* nm = CodeCache::find_nmethod(instr_pc);

          // The old patch site is now a move instruction so update
          // the reloc info so that it will get updated during
          // future GCs.
          RelocIterator iter(nm, (address)instr_pc, (address)(instr_pc + 1));
          relocInfo::change_reloc_info_for_address(&iter, (address) instr_pc, relocInfo::none, rtype);
        }
      } else {
        ICache::invalidate_range(copy_buff, *byte_count);
        NativeGeneralJump::insert_unconditional(instr_pc, being_initialized_entry);
      }
    }
  }

  // If we are patching in a non-perm oop, make sure the nmethod
  // is on the right list.
  if (ScavengeRootsInCode) {
    MutexLockerEx ml_code (CodeCache_lock, Mutex::_no_safepoint_check_flag);
    nmethod* nm = CodeCache::find_nmethod(caller_frame.pc());
    guarantee(nm != NULL, "only nmethods can contain non-perm oops");

    // Since we've patched some oops in the nmethod,
    // (re)register it with the heap.
    Universe::heap()->register_nmethod(nm);
  }
JRT_END

#else

JRT_ENTRY(void, Runtime1::patch_code(JavaThread* thread, Runtime1::StubID stub_id ))
  RegisterMap reg_map(thread, false);

  frame runtime_frame = thread->last_frame();
  frame caller_frame = runtime_frame.sender(&reg_map);

  // It's possible the nmethod was invalidated in the last
  // safepoint, but if it's still alive then make it not_entrant.
  nmethod* nm = CodeCache::find_nmethod(caller_frame.pc());
  if (nm != NULL) {
    nm->make_not_entrant();
  }

  NULL(thread, caller_frame.id());

  // Return to the now deoptimized frame.
JRT_END

#endif

//
// Entry point for compiled code. We want to patch a nmethod.
// We don't do a normal VM transition here because we want to
// know after the patching is complete and any safepoint(s) are taken
// if the calling nmethod was deoptimized. We do this by calling a
// helper method which does the normal VM transition and when it
// completes we can check for deoptimization. This simplifies the
// assembly code in the cpu directories.
//
int Runtime1::move_klass_patching(JavaThread* thread) {
//
// NOTE: we are still in Java
//
  Thread* THREAD = thread;
  {
    // Enter VM mode

    ResetNoHandleMark rnhm;
    patch_code(thread, load_klass_patching_id);
  }
  // Back in JAVA, use no oops DON'T safepoint

  // Return true if calling code is deoptimized

  return false;
}

int Runtime1::move_mirror_patching(JavaThread* thread) {
//
// NOTE: we are still in Java
//
  Thread* THREAD = thread;
  {
    // Enter VM mode

    ResetNoHandleMark rnhm;
    patch_code(thread, load_mirror_patching_id);
  }
  // Back in JAVA, use no oops DON'T safepoint

  // Return true if calling code is deoptimized

  return false;
}

//
// Entry point for compiled code. We want to patch a nmethod.
// We don't do a normal VM transition here because we want to
// know after the patching is complete and any safepoint(s) are taken
// if the calling nmethod was deoptimized. We do this by calling a
// helper method which does the normal VM transition and when it
// completes we can check for deoptimization. This simplifies the
// assembly code in the cpu directories.
//

int Runtime1::access_field_patching(JavaThread* thread) {
//
// NOTE: we are still in Java
//
  Thread* THREAD = thread;
  {
    // Enter VM mode

    ResetNoHandleMark rnhm;
    patch_code(thread, access_field_patching_id);
  }
  // Back in JAVA, use no oops DON'T safepoint

  // Return true if calling code is deoptimized

  return false;
JRT_END

JRT_LEAF(void, Runtime1::trace_block_entry(jint block_id))
  // for now we just print out the block id
  tty->print("%d ", block_id);
JRT_END

JRT_LEAF(int, Runtime1::is_instance_of(oopDesc* mirror, oopDesc* obj))
  // had to return int instead of bool, otherwise there may be a mismatch
  // between the C calling convention and the Java one.
  // e.g., on x86, GCC may clear only %al when returning a bool false, but
  // JVM takes the whole %eax as the return value, which may misinterpret
  // the return value as a boolean true.

  Klass* k = java_lang_Class::as_Klass(mirror);
  return (k != NULL && obj != NULL && obj->is_a(k)) ? 1 : 0;
JRT_END
