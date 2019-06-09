#ifndef SHARE_VM_RUNTIME_FRAME_HPP
#define SHARE_VM_RUNTIME_FRAME_HPP

#include "oops/method.hpp"
#include "runtime/basicLock.hpp"
#include "runtime/monitorChunk.hpp"
#include "runtime/registerMap.hpp"
#include "utilities/macros.hpp"

class CodeBlob;
class FrameValues;
class vframeArray;

// A frame represents a physical stack frame (an activation).  Frames
// can be C or Java frames, and the Java frames can be interpreted or
// compiled.  In contrast, vframes represent source-level activations,
// so that one physical frame can correspond to multiple source level
// frames because of inlining.

class frame {
 private:
  // Instance variables:
  intptr_t* _sp; // stack pointer (from Thread::last_Java_sp)
  address   _pc; // program counter (the next instruction after the call)
  CodeBlob* _cb; // CodeBlob that "owns" pc

 public:
  // Constructors
  frame();

  // Accessors

  // pc: Returns the pc at which this frame will continue normally.
  // It must point at the beginning of the next instruction to execute.
  address pc()             const { return _pc; }

  // This returns the pc that if you were in the debugger you'd see. Not
  // the idealized value in the frame object. This undoes the magic conversion
  // that happens for deoptimized frames. In addition it makes the value the
  // hardware would want to see in the native frame. The only user (at this point)
  // is deoptimization. It likely no one else should ever use it.
  address raw_pc() const;

  void set_pc( address   newpc );

  intptr_t* sp()           const { return _sp; }
  void set_sp( intptr_t* newsp ) { _sp = newsp; }

  CodeBlob* cb()           const { return _cb; }

  // patching operations
  void patch_pc(Thread* thread, address pc);

  // Every frame needs to return a unique id which distinguishes it from all other frames.
  // For sparc and ia32 use sp. ia64 can have memory frames that are empty so multiple frames
  // will have identical sp values. For ia64 the bsp (fp) value will serve. No real frame
  // should have an id() of NULL so it is a distinguishing value for an unmatchable frame.
  // We also have relationals which allow comparing a frame to anoth frame's id() allow
  // us to distinguish younger (more recent activation) from older (less recent activations)
  // A NULL id is only valid when comparing for equality.

  intptr_t* id(void) const;
  bool is_younger(intptr_t* id) const;
  bool is_older(intptr_t* id) const;

  // testers

  // Compares for strict equality. Rarely used or needed.
  // It can return a different result than f1.id() == f2.id()
  bool equal(frame other) const;

  // type testers
  bool is_java_frame()           const;
  bool is_entry_frame()          const;             // Java frame called from C?
  bool is_stub_frame()           const;
  bool is_ignored_frame()        const;
  bool is_native_frame()         const;
  bool is_runtime_frame()        const;
  bool is_compiled_frame()       const;
  bool is_safepoint_blob_frame() const;

  // testers
  bool is_first_frame() const;      // oldest frame? (has no sender)
  bool is_first_java_frame() const; // same for Java frame

  // returns the frame size in stack slots
  int frame_size(RegisterMap* map) const;

  // returns the sending frame
  frame sender(RegisterMap* map) const;

  bool safe_for_sender(JavaThread *thread);

  // returns the sender, but skips conversion frames
  frame real_sender(RegisterMap* map) const;

  // returns the the sending Java frame, skipping any intermediate C frames
  // NB: receiver must not be first frame
  frame java_sender() const;

 private:
  // Helper methods for better factored code in frame::sender
  frame sender_for_compiled_frame(RegisterMap* map) const;
  frame sender_for_entry_frame(RegisterMap* map) const;
  frame sender_for_native_frame(RegisterMap* map) const;

  bool is_entry_frame_valid(JavaThread* thread) const;

  // All frames:

  // A low-level interface for vframes:

 public:
  intptr_t* addr_at(int index)      const { return &fp()[index]; }
  intptr_t  at(int index)           const { return *addr_at(index); }

  // accessors for locals
  oop obj_at(int offset)            const { return *obj_at_addr(offset); }
  void obj_at_put(int offset, oop value)  { *obj_at_addr(offset) = value; }

  jint int_at(int offset)           const { return *int_at_addr(offset); }
  void int_at_put(int offset, jint value) { *int_at_addr(offset) = value; }

  oop* obj_at_addr(int offset)      const { return (oop*) addr_at(offset); }

  oop* adjusted_obj_at_addr(Method* method, int index) { return obj_at_addr(adjust_offset(method, index)); }

 private:
  jint* int_at_addr(int offset)     const { return (jint*) addr_at(offset); }

 public:
  // Link (i.e., the pointer to the previous frame)
  intptr_t* link() const;

  // Return address
  address sender_pc() const;

  // The frame's original SP, before any extension by an interpreted callee;
  // used for packing debug info into vframeArray objects and vframeArray lookup.
  intptr_t* unextended_sp() const;

  // returns the stack pointer of the calling frame
  intptr_t* sender_sp() const;

  // Returns the real 'frame pointer' for the current frame.
  // This is the value expected by the platform ABI when it defines a
  // frame pointer register. It may differ from the effective value of
  // the FP register when that register is used in the JVM for other
  // purposes (like compiled frames on some platforms).
  // On other platforms, it is defined so that the stack area used by
  // this frame goes from real_fp() to sp().
  intptr_t* real_fp() const;

 public:
  // Find receiver out of caller's (compiled) argument list
  oop retrieve_receiver(RegisterMap *reg_map);

  // Return the monitor owner and BasicLock for compiled synchronized
  // native methods so that biased locking can revoke the receiver's
  // bias if necessary.  This is also used by JVMTI's GetLocalInstance method
  // (via VM_GetReceiver) to retrieve the receiver from a native wrapper frame.
  BasicLock* get_native_monitor();
  oop        get_native_receiver();

 public:
  // Entry frames
  JavaCallWrapper* entry_frame_call_wrapper() const { return *entry_frame_call_wrapper_addr(); }
  JavaCallWrapper* entry_frame_call_wrapper_if_safe(JavaThread* thread) const;
  JavaCallWrapper** entry_frame_call_wrapper_addr() const;
  intptr_t* entry_frame_argument_at(int offset) const;

  // tells whether there is another chunk of Delta stack above
  bool entry_frame_is_first() const;

  // Safepoints

 public:
  oop saved_oop_result(RegisterMap* map) const;
  void set_saved_oop_result(RegisterMap* map, oop obj);

  // For debugging
 private:
  const char* print_name() const;

  void describe_pd(FrameValues& values, int frame_no);

 public:
  void print_value() const { print_value_on(tty,NULL); }
  void print_value_on(outputStream* st, JavaThread *thread) const;
  void print_on(outputStream* st) const;
  void print_on_error(outputStream* st, char* buf, int buflen, bool verbose = false) const;
  static void print_C_frame(outputStream* st, char* buf, int buflen, address pc);

  // Add annotated descriptions of memory locations belonging to this frame to values
  void describe(FrameValues& values, int frame_no);

  // Conversion from a VMReg to physical stack location
  oop* oopmapreg_to_location(VMReg reg, const RegisterMap* reg_map) const;

  // Oops-do's
  void oops_compiled_arguments_do(Symbol* signature, bool has_receiver, bool has_appendix, const RegisterMap* reg_map, OopClosure* f);

 private:
  // Iteration of oops
  void oops_do_internal(OopClosure* f, CodeBlobClosure* cf, RegisterMap* map);
  void oops_entry_do(OopClosure* f, const RegisterMap* map);
  void oops_code_blob_do(OopClosure* f, CodeBlobClosure* cf, const RegisterMap* map);
  int adjust_offset(Method* method, int index); // helper for above fn
 public:
  // Memory management
  void oops_do(OopClosure* f, CodeBlobClosure* cf, RegisterMap* map) { oops_do_internal(f, cf, map); }
  void nmethods_do(CodeBlobClosure* cf);

  // RedefineClasses support for finding live interpreted methods on the stack
  void metadata_do(void f(Metadata*));

  // Verification
  static bool verify_return_pc(address x);

#include CPU_HEADER(frame)
};

//
// StackFrameStream iterates through the frames of a thread starting from
// top most frame. It automatically takes care of updating the location of
// all (callee-saved) registers. Notice: If a thread is stopped at
// a safepoint, all registers are saved, not only the callee-saved ones.
//
// Use:
//
//   for (StackFrameStream fst(thread); !fst.is_done(); fst.next()) {
//     ...
//   }
//
class StackFrameStream : public StackObj {
 private:
  frame       _fr;
  RegisterMap _reg_map;
  bool        _is_done;
 public:
   StackFrameStream(JavaThread *thread, bool update = true);

  // Iteration
  inline bool is_done();
  void next()                     { if (!_is_done) _fr = _fr.sender(&_reg_map); }

  // Query
  frame *current()                { return &_fr; }
  RegisterMap* register_map()     { return &_reg_map; }
};

#endif
