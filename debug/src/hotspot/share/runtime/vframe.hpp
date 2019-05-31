#ifndef SHARE_VM_RUNTIME_VFRAME_HPP
#define SHARE_VM_RUNTIME_VFRAME_HPP

#include "code/debugInfo.hpp"
#include "code/debugInfoRec.hpp"
#include "code/location.hpp"
#include "oops/oop.hpp"
#include "runtime/frame.hpp"
#include "runtime/stackValue.hpp"
#include "runtime/stackValueCollection.hpp"
#include "utilities/growableArray.hpp"

// vframes are virtual stack frames representing source level activations.
// A single frame may hold several source level activations in the case of
// optimized code. The debugging stored with the optimized code enables
// us to unfold a frame as a stack of vframes.
// A cVFrame represents an activation of a non-java method.

// The vframe inheritance hierarchy:
// - vframe
//   - javaVFrame
//     - interpretedVFrame
//     - compiledVFrame     ; (used for both compiled Java methods and native stubs)
//   - externalVFrame
//     - entryVFrame        ; special frame created when calling Java from C

// - BasicLock

class vframe: public ResourceObj {
 protected:
  frame        _fr;      // Raw frame behind the virtual frame.
  RegisterMap  _reg_map; // Register map for the raw frame (used to handle callee-saved registers).
  JavaThread*  _thread;  // The thread owning the raw frame.

  vframe(const frame* fr, const RegisterMap* reg_map, JavaThread* thread);
  vframe(const frame* fr, JavaThread* thread);
 public:
  // Factory method for creating vframes
  static vframe* new_vframe(const frame* f, const RegisterMap *reg_map, JavaThread* thread);

  // Accessors
  frame              fr()           const { return _fr; }
  CodeBlob*          cb()         const { return _fr.cb(); }
  CompiledMethod*   nm()         const { return (CompiledMethod*) cb(); }

// ???? Does this need to be a copy?
  frame*             frame_pointer() { return &_fr; }
  const RegisterMap* register_map() const { return &_reg_map; }
  JavaThread*        thread()       const { return _thread; }

  // Returns the sender vframe
  virtual vframe* sender() const;

  // Returns the next javaVFrame on the stack (skipping all other kinds of frame)
  javaVFrame *java_sender() const;

  // Answers if the this is the top vframe in the frame, i.e., if the sender vframe
  // is in the caller frame
  virtual bool is_top() const { return true; }

  // Returns top vframe within same frame (see is_top())
  virtual vframe* top() const;

  // Type testing operations
  virtual bool is_entry_frame()       const { return false; }
  virtual bool is_java_frame()        const { return false; }
  virtual bool is_interpreted_frame() const { return false; }
  virtual bool is_compiled_frame()    const { return false; }
};

class javaVFrame: public vframe {
 public:
  // JVM state
  virtual Method*                      method()         const = 0;
  virtual int                          bci()            const = 0;
  virtual StackValueCollection*        locals()         const = 0;
  virtual StackValueCollection*        expressions()    const = 0;
  // the order returned by monitors() is from oldest -> youngest#4418568
  virtual GrowableArray<MonitorInfo*>* monitors()       const = 0;

  // Debugging support via JVMTI.
  // NOTE that this is not guaranteed to give correct results for compiled vframes.
  // Deoptimize first if necessary.
  virtual void set_locals(StackValueCollection* values) const = 0;

  // Test operation
  bool is_java_frame() const { return true; }

 protected:
  javaVFrame(const frame* fr, const RegisterMap* reg_map, JavaThread* thread) : vframe(fr, reg_map, thread) { }
  javaVFrame(const frame* fr, JavaThread* thread) : vframe(fr, thread) { }

 public:
  // casting
  static javaVFrame* cast(vframe* vf) {
    return (javaVFrame*) vf;
  }

  // Return an array of monitors locked by this frame in the youngest to oldest order
  GrowableArray<MonitorInfo*>* locked_monitors();

  // printing used during stack dumps and diagnostics
  static void print_locked_object_class_name(outputStream* st, Handle obj, const char* lock_state);
  void print_lock_info_on(outputStream* st, int frame_count);
  void print_lock_info(int frame_count) { print_lock_info_on(tty, frame_count); }

  friend class vframe;
};

class interpretedVFrame: public javaVFrame {
 public:
  // JVM state
  Method*                      method()         const;
  int                          bci()            const;
  StackValueCollection*        locals()         const;
  StackValueCollection*        expressions()    const;
  GrowableArray<MonitorInfo*>* monitors()       const;

  void set_locals(StackValueCollection* values) const;

  // Test operation
  bool is_interpreted_frame() const { return true; }

 protected:
  interpretedVFrame(const frame* fr, const RegisterMap* reg_map, JavaThread* thread) : javaVFrame(fr, reg_map, thread) { };

 public:
  // Accessors for Byte Code Pointer
  u_char* bcp() const;
  void set_bcp(u_char* bcp);

  // casting
  static interpretedVFrame* cast(vframe* vf) {
    return (interpretedVFrame*) vf;
  }

 private:
  static const int bcp_offset;
  intptr_t* locals_addr_at(int offset) const;
  StackValueCollection* stack_data(bool expressions) const;
  // returns where the parameters starts relative to the frame pointer
  int start_of_parameters() const;

  friend class vframe;
};

class externalVFrame: public vframe {
 protected:
  externalVFrame(const frame* fr, const RegisterMap* reg_map, JavaThread* thread) : vframe(fr, reg_map, thread) { }

  friend class vframe;
};

class entryVFrame: public externalVFrame {
 public:
  bool is_entry_frame() const { return true; }

 protected:
  entryVFrame(const frame* fr, const RegisterMap* reg_map, JavaThread* thread);

 public:
  // casting
  static entryVFrame* cast(vframe* vf) {
    return (entryVFrame*) vf;
  }

  friend class vframe;
};

// A MonitorInfo is a ResourceObject that describes a the pair:
// 1) the owner of the monitor
// 2) the monitor lock
class MonitorInfo : public ResourceObj {
 private:
  oop        _owner; // the object owning the monitor
  BasicLock* _lock;
  oop        _owner_klass; // klass (mirror) if owner was scalar replaced
  bool       _eliminated;
  bool       _owner_is_scalar_replaced;
 public:
  // Constructor
  MonitorInfo(oop owner, BasicLock* lock, bool eliminated, bool owner_is_scalar_replaced) {
    if (!owner_is_scalar_replaced) {
      _owner = owner;
      _owner_klass = NULL;
    } else {
      _owner = NULL;
      _owner_klass = owner;
    }
    _lock  = lock;
    _eliminated = eliminated;
    _owner_is_scalar_replaced = owner_is_scalar_replaced;
  }
  // Accessors
  oop        owner() const {
    return _owner;
  }
  oop   owner_klass() const {
    return _owner_klass;
  }
  BasicLock* lock()  const { return _lock; }
  bool eliminated()  const { return _eliminated; }
  bool owner_is_scalar_replaced()  const { return _owner_is_scalar_replaced; }
};

class vframeStreamCommon : StackObj {
 protected:
  // common
  frame        _frame;
  JavaThread*  _thread;
  RegisterMap  _reg_map;
  enum { interpreted_mode, compiled_mode, at_end_mode } _mode;

  int _sender_decode_offset;

  // Cached information
  Method* _method;
  int       _bci;

  // Should VM activations be ignored or not
  bool _stop_at_java_call_stub;

  bool fill_in_compiled_inlined_sender();
  void fill_from_compiled_frame(int decode_offset);
  void fill_from_compiled_native_frame();

  void fill_from_interpreter_frame();
  bool fill_from_frame();

  // Helper routine for security_get_caller_frame
  void skip_prefixed_method_and_wrappers();

 public:
  // Constructor
  inline vframeStreamCommon(JavaThread* thread);

  // Accessors
  Method* method() const { return _method; }
  int bci() const { return _bci; }
  inline intptr_t* frame_id() const;
  address frame_pc() const { return _frame.pc(); }

  CodeBlob*          cb()         const { return _frame.cb(); }
  CompiledMethod*   nm()         const { return (CompiledMethod*) cb(); }

  // Frame type
  inline bool is_interpreted_frame() const;
  inline bool is_entry_frame() const;

  // Iteration
  inline void next();
  void security_next();

  bool at_end() const { return _mode == at_end_mode; }

  // Implements security traversal. Skips depth no. of frame including
  // special security frames and prefixed native methods
  void security_get_caller_frame(int depth);

  // Helper routine for JVM_LatestUserDefinedLoader -- needed for 1.4
  // reflection implementation
  void skip_reflection_related_frames();
};

class vframeStream : public vframeStreamCommon {
 public:
  // Constructors
  vframeStream(JavaThread* thread, bool stop_at_java_call_stub = false);

  // top_frame may not be at safepoint, start with sender
  vframeStream(JavaThread* thread, frame top_frame, bool stop_at_java_call_stub = false);
};

#endif
