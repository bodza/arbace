#ifndef SHARE_VM_RUNTIME_SHAREDRUNTIME_HPP
#define SHARE_VM_RUNTIME_SHAREDRUNTIME_HPP

#include "interpreter/linkResolver.hpp"
#include "memory/allocation.hpp"
#include "memory/resourceArea.hpp"
#include "utilities/hashtable.hpp"
#include "utilities/macros.hpp"

class AdapterHandlerEntry;
class AdapterHandlerTable;
class AdapterFingerPrint;
class vframeStream;

// Runtime is the base class for various runtime interfaces
// (CompilerRuntime, etc.). It provides shared functionality
// such as exception forwarding (C++ to Java exceptions),
// locking/unlocking mechanisms, statistical information, etc.

class SharedRuntime: AllStatic {
  friend class VMStructs;

 private:
  static methodHandle resolve_sub_helper(JavaThread *thread, bool is_virtual, bool is_optimized, TRAPS);

  // Shared stub locations

  static RuntimeStub*        _wrong_method_blob;
  static RuntimeStub*        _wrong_method_abstract_blob;
  static RuntimeStub*        _ic_miss_blob;
  static RuntimeStub*        _resolve_opt_virtual_call_blob;
  static RuntimeStub*        _resolve_virtual_call_blob;
  static RuntimeStub*        _resolve_static_call_blob;
  static address             _resolve_static_call_entry;

  static SafepointBlob*      _polling_page_vectors_safepoint_handler_blob;
  static SafepointBlob*      _polling_page_safepoint_handler_blob;
  static SafepointBlob*      _polling_page_return_handler_blob;

 private:
  enum { POLL_AT_RETURN,  POLL_AT_LOOP, POLL_AT_VECTOR_LOOP };
  static SafepointBlob* generate_handler_blob(address call_ptr, int poll_type);
  static RuntimeStub*   generate_resolve_blob(address destination, const char* name);

 public:
  static void generate_stubs(void);

  // The following arithmetic routines are used on platforms that do
  // not have machine instructions to implement their functionality.
  // Do not remove these.

  // long arithmetics
  static jlong   lmul(jlong y, jlong x);
  static jlong   ldiv(jlong y, jlong x);
  static jlong   lrem(jlong y, jlong x);

  // float and double remainder
  static jfloat  frem(jfloat  x, jfloat  y);
  static jdouble drem(jdouble x, jdouble y);

#ifdef __SOFTFP__
  static jfloat  fadd(jfloat x, jfloat y);
  static jfloat  fsub(jfloat x, jfloat y);
  static jfloat  fmul(jfloat x, jfloat y);
  static jfloat  fdiv(jfloat x, jfloat y);

  static jdouble dadd(jdouble x, jdouble y);
  static jdouble dsub(jdouble x, jdouble y);
  static jdouble dmul(jdouble x, jdouble y);
  static jdouble ddiv(jdouble x, jdouble y);
#endif

  // float conversion (needs to set appropriate rounding mode)
  static jint    f2i (jfloat  x);
  static jlong   f2l (jfloat  x);
  static jint    d2i (jdouble x);
  static jlong   d2l (jdouble x);
  static jfloat  d2f (jdouble x);
  static jfloat  l2f (jlong   x);
  static jdouble l2d (jlong   x);

#ifdef __SOFTFP__
  static jfloat  i2f (jint    x);
  static jdouble i2d (jint    x);
  static jdouble f2d (jfloat  x);
#endif

  // double trigonometrics and transcendentals
  static jdouble dsin(jdouble x);
  static jdouble dcos(jdouble x);
  static jdouble dtan(jdouble x);
  static jdouble dlog(jdouble x);
  static jdouble dlog10(jdouble x);
  static jdouble dexp(jdouble x);
  static jdouble dpow(jdouble x, jdouble y);

#if defined(__SOFTFP__) || defined(E500V2)
  static double dabs(double f);
#endif

#if defined(__SOFTFP__)
  static double dsqrt(double f);
#endif

  // Montgomery multiplication
  static void montgomery_multiply(jint *a_ints, jint *b_ints, jint *n_ints, jint len, jlong inv, jint *m_ints);
  static void montgomery_square(jint *a_ints, jint *n_ints, jint len, jlong inv, jint *m_ints);

#ifdef __SOFTFP__
  // C++ compiler generates soft float instructions as well as passing
  // float and double in registers.
  static int  fcmpl(float x, float y);
  static int  fcmpg(float x, float y);
  static int  dcmpl(double x, double y);
  static int  dcmpg(double x, double y);

  static int unordered_fcmplt(float x, float y);
  static int unordered_dcmplt(double x, double y);
  static int unordered_fcmple(float x, float y);
  static int unordered_dcmple(double x, double y);
  static int unordered_fcmpge(float x, float y);
  static int unordered_dcmpge(double x, double y);
  static int unordered_fcmpgt(float x, float y);
  static int unordered_dcmpgt(double x, double y);

  static float  fneg(float f);
  static double dneg(double f);
#endif

  // exception handling across interpreter/compiler boundaries
  static address raw_exception_handler_for_return_address(JavaThread* thread, address return_address);
  static address exception_handler_for_return_address(JavaThread* thread, address return_address);

  // exception handling and implicit exceptions
  static address compute_compiled_exc_handler(CompiledMethod* nm, address ret_pc, Handle& exception, bool force_unwind, bool top_frame_only, bool& recursive_exception_occurred);
  enum ImplicitExceptionKind {
    IMPLICIT_NULL,
    IMPLICIT_DIVIDE_BY_ZERO,
    STACK_OVERFLOW
  };
  static void    throw_AbstractMethodError(JavaThread* thread);
  static void    throw_IncompatibleClassChangeError(JavaThread* thread);
  static void    throw_ArithmeticException(JavaThread* thread);
  static void    throw_NullPointerException(JavaThread* thread);
  static void    throw_NullPointerException_at_call(JavaThread* thread);
  static void    throw_StackOverflowError(JavaThread* thread);
  static address continuation_for_implicit_exception(JavaThread* thread, address faulting_pc, ImplicitExceptionKind exception_kind);

  // Post-slow-path-allocation, pre-initializing-stores step for
  // implementing e.g. ReduceInitialCardMarks
  static void on_slowpath_allocation_exit(JavaThread* thread);

  static void enable_stack_reserved_zone(JavaThread* thread);

  // Shared stub locations
  static address get_poll_stub(address pc);

  static address get_ic_miss_stub() {
    return _ic_miss_blob->entry_point();
  }

  static address get_handle_wrong_method_stub()          { return _wrong_method_blob->entry_point(); }
  static address get_handle_wrong_method_abstract_stub() { return _wrong_method_abstract_blob->entry_point(); }
  static address get_resolve_opt_virtual_call_stub()     { return _resolve_opt_virtual_call_blob->entry_point(); }
  static address get_resolve_virtual_call_stub()         { return _resolve_virtual_call_blob->entry_point(); }
  static address get_resolve_static_call_stub()          { return _resolve_static_call_blob->entry_point(); }

  static SafepointBlob* polling_page_return_handler_blob()            { return _polling_page_return_handler_blob; }
  static SafepointBlob* polling_page_safepoint_handler_blob()         { return _polling_page_safepoint_handler_blob; }
  static SafepointBlob* polling_page_vectors_safepoint_handler_blob() { return _polling_page_vectors_safepoint_handler_blob; }

  // Helper routine for full-speed JVMTI exception throwing support
  static void throw_and_post_jvmti_exception(JavaThread *thread, Handle h_exception);
  static void throw_and_post_jvmti_exception(JavaThread *thread, Symbol* name, const char *message = NULL);

  // RedefineClasses() tracing support for obsolete method entry
  static int rc_trace_method_entry(JavaThread* thread, Method* m);

  // To be used as the entry point for unresolved native methods.
  static address native_method_throw_unsatisfied_link_error_entry();
  static address native_method_throw_unsupported_operation_exception_entry();

  static void register_finalizer(JavaThread* thread, oopDesc* obj);

  // Utility method for retrieving the Java thread id, returns 0 if the
  // thread is not a well formed Java thread.
  static jlong get_java_tid(Thread* thread);

  // used by native wrappers to reenable yellow if overflow happened in native code
  static void reguard_yellow_pages();

  // Fill in the "X cannot be cast to a Y" message for ClassCastException
  //
  // @param thr the current thread
  // @param caster_klass the class of the object we are casting
  // @return the dynamically allocated exception message (must be freed
  // by the caller using a resource mark)
  //
  // BCP must refer to the current 'checkcast' opcode for the frame
  // on top of the stack.
  // The caller (or one of its callers) must use a ResourceMark
  // in order to correctly free the result.
  //
  static char* generate_class_cast_message(JavaThread* thr, Klass* caster_klass);

  // Fill in the "X cannot be cast to a Y" message for ClassCastException
  //
  // @param caster_klass the class of the object we are casting
  // @param target_klass the target klass attempt
  // @return the dynamically allocated exception message (must be freed
  // by the caller using a resource mark)
  //
  // This version does not require access the frame, so it can be called
  // from interpreted code
  // The caller (or one of it's callers) must use a ResourceMark
  // in order to correctly free the result.
  //
  static char* generate_class_cast_message(Klass* caster_klass, Klass* target_klass, Symbol* target_klass_name = NULL);

  // Resolves a call site- may patch in the destination of the call into the
  // compiled code.
  static methodHandle resolve_helper(JavaThread *thread, bool is_virtual, bool is_optimized, TRAPS);

 public:
  // Resets a call-site in compiled code so it will get resolved again.
  static methodHandle reresolve_call_site(JavaThread *thread, TRAPS);

  // In the code prolog, if the klass comparison fails, the inline cache
  // misses and the call site is patched to megamorphic
  static methodHandle handle_ic_miss_helper(JavaThread* thread, TRAPS);

  // Find the method that called us.
  static methodHandle find_callee_method(JavaThread* thread, TRAPS);

 private:
  static Handle find_callee_info(JavaThread* thread, Bytecodes::Code& bc, CallInfo& callinfo, TRAPS);
  static Handle find_callee_info_helper(JavaThread* thread, vframeStream& vfst, Bytecodes::Code& bc, CallInfo& callinfo, TRAPS);

  static methodHandle extract_attached_method(vframeStream& vfst);

  static address clean_virtual_call_entry();
  static address clean_opt_virtual_call_entry();
  static address clean_static_call_entry();

#if defined(X86)
  // For Object.hashCode, System.identityHashCode try to pull hashCode from object header if available.
  static void inline_check_hashcode_from_object_header(MacroAssembler* masm, const methodHandle& method, Register obj_reg, Register result);
#endif

 public:
  // Read the array of BasicTypes from a Java signature, and compute where
  // compiled Java code would like to put the results.  Values in reg_lo and
  // reg_hi refer to 4-byte quantities.  Values less than SharedInfo::stack0 are
  // registers, those above refer to 4-byte stack slots.  All stack slots are
  // based off of the window top.  SharedInfo::stack0 refers to the first usable
  // slot in the bottom of the frame. SharedInfo::stack0+1 refers to the memory word
  // 4-bytes higher. So for sparc because the register window save area is at
  // the bottom of the frame the first 16 words will be skipped and SharedInfo::stack0
  // will be just above it. (
  // return value is the maximum number of VMReg stack slots the convention will use.
  static int java_calling_convention(const BasicType* sig_bt, VMRegPair* regs, int total_args_passed, int is_outgoing);

  static void check_member_name_argument_is_last_argument(const methodHandle& method, const BasicType* sig_bt, const VMRegPair* regs) { };

  // Ditto except for calling C
  //
  // C argument in register AND stack slot.
  // Some architectures require that an argument must be passed in a register
  // AND in a stack slot. These architectures provide a second VMRegPair array
  // to be filled by the c_calling_convention method. On other architectures,
  // NULL is being passed as the second VMRegPair array, so arguments are either
  // passed in a register OR in a stack slot.
  static int c_calling_convention(const BasicType *sig_bt, VMRegPair *regs, VMRegPair *regs2, int total_args_passed);

  static size_t trampoline_size();

  static void generate_trampoline(MacroAssembler *masm, address destination);

  // Convert a sig into a calling convention register layout
  // and find interesting things about it.
  static VMRegPair* find_callee_arguments(Symbol* sig, bool has_receiver, bool has_appendix, int *arg_size);
  static VMReg name_for_receiver();

  // "Top of Stack" slots that may be unused by the calling convention but must
  // otherwise be preserved.
  // On Intel these are not necessary and the value can be zero.
  // On Sparc this describes the words reserved for storing a register window
  // when an interrupt occurs.
  static uint out_preserve_stack_slots();

  // Is vector's size (in bytes) bigger than a size saved by default?
  // For example, on x86 16 bytes XMM registers are saved by default.
  static bool is_wide_vector(int size);

  // Save and restore a native result
  static void    save_native_result(MacroAssembler *_masm, BasicType ret_type, int frame_slots);
  static void restore_native_result(MacroAssembler *_masm, BasicType ret_type, int frame_slots);

  // Generate a native wrapper for a given method.  The method takes arguments
  // in the Java compiled code convention, marshals them to the native
  // convention (handlizes oops, etc), transitions to native, makes the call,
  // returns to java state (possibly blocking), unhandlizes any result and
  // returns.
  //
  // The wrapper may contain special-case code if the given method
  // is a JNI critical method, or a compiled method handle adapter.
  static nmethod* generate_native_wrapper(MacroAssembler* masm, const methodHandle& method, int compile_id, BasicType* sig_bt, VMRegPair* regs, BasicType ret_type);

  // Block before entering a JNI critical method
  static void block_for_jni_critical(JavaThread* thread);

  // Slow-path Locking and Unlocking
  static void complete_monitor_locking_C(oopDesc* obj, BasicLock* lock, JavaThread* thread);
  static void complete_monitor_unlocking_C(oopDesc* obj, BasicLock* lock, JavaThread* thread);

  // Resolving of calls
  static address resolve_static_call_C     (JavaThread *thread);
  static address resolve_virtual_call_C    (JavaThread *thread);
  static address resolve_opt_virtual_call_C(JavaThread *thread);

  // arraycopy, the non-leaf version.  (See StubRoutines for all the leaf calls.)
  static void slow_arraycopy_C(oopDesc* src,  jint src_pos, oopDesc* dest, jint dest_pos, jint length, JavaThread* thread);

  // handle ic miss with caller being compiled code
  // wrong method handling (inline cache misses, zombie methods)
  static address handle_wrong_method(JavaThread* thread);
  static address handle_wrong_method_abstract(JavaThread* thread);
  static address handle_wrong_method_ic_miss(JavaThread* thread);

  static address handle_unsafe_access(JavaThread* thread, address next_pc);
};

// ---------------------------------------------------------------------------
// Implementation of AdapterHandlerLibrary
//
// This library manages argument marshaling adapters and native wrappers.
// There are 2 flavors of adapters: I2C and C2I.
//
// The I2C flavor takes a stock interpreted call setup, marshals the
// arguments for a Java-compiled call, and jumps to Rmethod-> code()->
// code_begin().  It is broken to call it without an nmethod assigned.
// The usual behavior is to lift any register arguments up out of the
// stack and possibly re-pack the extra arguments to be contiguous.
// I2C adapters will save what the interpreter's stack pointer will be
// after arguments are popped, then adjust the interpreter's frame
// size to force alignment and possibly to repack the arguments.
// After re-packing, it jumps to the compiled code start.  There are
// no safepoints in this adapter code and a GC cannot happen while
// marshaling is in progress.
//
// The C2I flavor takes a stock compiled call setup plus the target method in
// Rmethod, marshals the arguments for an interpreted call and jumps to
// Rmethod->NULL.  On entry, the interpreted frame has not yet been
// setup.  Compiled frames are fixed-size and the args are likely not in the
// right place.  Hence all the args will likely be copied into the
// interpreter's frame, forcing that frame to grow.  The compiled frame's
// outgoing stack args will be dead after the copy.
//
// Native wrappers, like adapters, marshal arguments.  Unlike adapters they
// also perform an official frame push & pop.  They have a call to the native
// routine in their middles and end in a return (instead of ending in a jump).
// The native wrappers are stored in real nmethods instead of the BufferBlobs
// used by the adapters.  The code generation happens here because it's very
// similar to what the adapters have to do.

class AdapterHandlerEntry : public BasicHashtableEntry<mtCode> {
  friend class AdapterHandlerTable;

 private:
  AdapterFingerPrint* _fingerprint;
  address _i2c_entry;
  address _c2i_entry;
  address _c2i_unverified_entry;

  void init(AdapterFingerPrint* fingerprint, address i2c_entry, address c2i_entry, address c2i_unverified_entry) {
    _fingerprint = fingerprint;
    _i2c_entry = i2c_entry;
    _c2i_entry = c2i_entry;
    _c2i_unverified_entry = c2i_unverified_entry;
  }

  void deallocate();

  // should never be used
  AdapterHandlerEntry();

 public:
  address get_i2c_entry()            const { return _i2c_entry; }
  address get_c2i_entry()            const { return _c2i_entry; }
  address get_c2i_unverified_entry() const { return _c2i_unverified_entry; }
  address base_address();
  void relocate(address new_base);

  AdapterFingerPrint* fingerprint() const { return _fingerprint; }

  AdapterHandlerEntry* next() {
    return (AdapterHandlerEntry*)BasicHashtableEntry<mtCode>::next();
  }

  //virtual void print_on(outputStream* st) const;  DO NOT USE
  void print_adapter_on(outputStream* st) const;
};

class AdapterHandlerLibrary: public AllStatic {
 private:
  static BufferBlob* _buffer; // the temporary code buffer in CodeCache
  static AdapterHandlerTable* _adapters;
  static AdapterHandlerEntry* _abstract_method_handler;
  static BufferBlob* buffer_blob();
  static void initialize();
  static AdapterHandlerEntry* get_adapter0(const methodHandle& method);

 public:
  static AdapterHandlerEntry* new_entry(AdapterFingerPrint* fingerprint, address i2c_entry, address c2i_entry, address c2i_unverified_entry);
  static void create_native_wrapper(const methodHandle& method);
  static AdapterHandlerEntry* get_adapter(const methodHandle& method);

  static void print_handler(const CodeBlob* b) { print_handler_on(tty, b); }
  static void print_handler_on(outputStream* st, const CodeBlob* b);
  static bool contains(const CodeBlob* b);
};

#endif
