#ifndef SHARE_VM_INTERPRETER_INTERPRETERRUNTIME_HPP
#define SHARE_VM_INTERPRETER_INTERPRETERRUNTIME_HPP

#include "interpreter/bytecode.hpp"
#include "interpreter/linkResolver.hpp"
#include "memory/universe.hpp"
#include "oops/method.hpp"
#include "runtime/frame.hpp"
#include "runtime/signature.hpp"
#include "runtime/thread.hpp"
#include "utilities/macros.hpp"

// The InterpreterRuntime is called by the interpreter for everything
// that cannot/should not be dealt with in assembly and needs C support.

class InterpreterRuntime: AllStatic {
  friend class BytecodeClosure; // for method and bcp
  friend class PrintingClosure; // for method and bcp

 private:

  static void      set_bcp_and_mdp(address bcp, JavaThread*thread);
  static void      note_trap_inner(JavaThread* thread, int reason,
                                   const methodHandle& trap_method, int trap_bci, TRAPS);
  static void      note_trap(JavaThread *thread, int reason, TRAPS);

  // Inner work method for Interpreter's frequency counter overflow.
  static nmethod* frequency_counter_overflow_inner(JavaThread* thread, address branch_bcp);

 public:
  // Constants
  static void    ldc           (JavaThread* thread, bool wide);
  static void    resolve_ldc   (JavaThread* thread, Bytecodes::Code bytecode);

  // Allocation
  static void    _new          (JavaThread* thread, ConstantPool* pool, int index);
  static void    newarray      (JavaThread* thread, BasicType type, jint size);
  static void    anewarray     (JavaThread* thread, ConstantPool* pool, int index, jint size);
  static void    multianewarray(JavaThread* thread, jint* first_size_address);
  static void    register_finalizer(JavaThread* thread, oopDesc* obj);

  // Quicken instance-of and check-cast bytecodes
  static void    quicken_io_cc(JavaThread* thread);

  // Exceptions thrown by the interpreter
  static void    throw_AbstractMethodError(JavaThread* thread);
  static void    throw_AbstractMethodErrorWithMethod(JavaThread* thread, Method* oop);
  static void    throw_AbstractMethodErrorVerbose(JavaThread* thread,
                                                  Klass* recvKlass,
                                                  Method* missingMethod);

  static void    throw_IncompatibleClassChangeError(JavaThread* thread);
  static void    throw_IncompatibleClassChangeErrorVerbose(JavaThread* thread,
                                                           Klass* resc,
                                                           Klass* interfaceKlass);
  static void    throw_StackOverflowError(JavaThread* thread);
  static void    throw_delayed_StackOverflowError(JavaThread* thread);
  static void    throw_ArrayIndexOutOfBoundsException(JavaThread* thread, arrayOopDesc* a, jint index);
  static void    throw_ClassCastException(JavaThread* thread, oopDesc* obj);
  static void    create_exception(JavaThread* thread, char* name, char* message);
  static void    create_klass_exception(JavaThread* thread, char* name, oopDesc* obj);
  static address exception_handler_for_exception(JavaThread* thread, oopDesc* exception);
  static void    throw_pending_exception(JavaThread* thread);

  static void resolve_from_cache(JavaThread* thread, Bytecodes::Code bytecode);
 private:
  // Statics & fields
  static void resolve_get_put(JavaThread* thread, Bytecodes::Code bytecode);

  // Calls
  static void resolve_invoke(JavaThread* thread, Bytecodes::Code bytecode);
  static void resolve_invokehandle (JavaThread* thread);
  static void resolve_invokedynamic(JavaThread* thread);

 public:
  // Synchronization
  static void    monitorenter(JavaThread* thread, BasicObjectLock* elem);
  static void    monitorexit (JavaThread* thread, BasicObjectLock* elem);

  static void    throw_illegal_monitor_state_exception(JavaThread* thread);
  static void    new_illegal_monitor_state_exception(JavaThread* thread);

  // Breakpoints
  static void _breakpoint(JavaThread* thread, Method* method, address bcp);
  static Bytecodes::Code get_original_bytecode_at(JavaThread* thread, Method* method, address bcp);
  static void            set_original_bytecode_at(JavaThread* thread, Method* method, address bcp, Bytecodes::Code new_code);
  static bool is_breakpoint(JavaThread *thread);

  // Safepoints
  static void    at_safepoint(JavaThread* thread);

  // Debugger support
  static int  interpreter_contains(address pc);

  // Native signature handlers
  static void prepare_native_call(JavaThread* thread, Method* method);
  static address slow_signature_handler(JavaThread* thread, Method* method, intptr_t* from, intptr_t* to);

#if defined(IA32) || defined(AMD64) || defined(ARM)
  // Popframe support (only needed on x86, AMD64 and ARM)
  static void popframe_move_outgoing_args(JavaThread* thread, void* src_address, void* dest_address);
#endif

  // bytecode tracing is only used by the TraceBytecodes
  static intptr_t trace_bytecode(JavaThread* thread, intptr_t preserve_this_value, intptr_t tos, intptr_t tos2) { return 0; };

  // Platform dependent stuff
#include CPU_HEADER(interpreterRT)

  // optional normalization of fingerprints to reduce the number of adapters
  static uint64_t normalize_fast_native_fingerprint(uint64_t fingerprint);

  // Interpreter's frequency counter overflow
  static nmethod* frequency_counter_overflow(JavaThread* thread, address branch_bcp);

  // Interpreter profiling support
  static jint    bcp_to_di(Method* method, address cur_bcp);
  static void    profile_method(JavaThread* thread);
  static void    update_mdp_for_ret(JavaThread* thread, int bci);
  static MethodCounters* build_method_counters(JavaThread* thread, Method* m);
};

class SignatureHandlerLibrary: public AllStatic {
 public:
  enum { buffer_size =  1*K }; // the size of the temporary code buffer
  enum { blob_size   = 32*K }; // the size of a handler code blob.

 private:
  static BufferBlob*              _handler_blob; // the current buffer blob containing the generated handlers
  static address                  _handler;      // next available address within _handler_blob;
  static GrowableArray<uint64_t>* _fingerprints; // the fingerprint collection
  static GrowableArray<address>*  _handlers;     // the corresponding handlers
  static address                  _buffer;       // the temporary code buffer

  static address set_handler_blob();
  static void initialize();
  static address set_handler(CodeBuffer* buffer);
  static void pd_set_handler(address handler);

 public:
  static void add(const methodHandle& method);
  static void add(uint64_t fingerprint, address handler);
};

#endif
