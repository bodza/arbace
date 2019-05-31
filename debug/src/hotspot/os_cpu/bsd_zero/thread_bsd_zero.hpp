#ifndef OS_CPU_BSD_ZERO_VM_THREAD_BSD_ZERO_HPP
#define OS_CPU_BSD_ZERO_VM_THREAD_BSD_ZERO_HPP

 private:
  ZeroStack  _zero_stack;
  ZeroFrame* _top_zero_frame;

  void pd_initialize() {
    _top_zero_frame = NULL;
  }

 public:
  ZeroStack *zero_stack() {
    return &_zero_stack;
  }

 public:
  ZeroFrame *top_zero_frame() {
    return _top_zero_frame;
  }
  void push_zero_frame(ZeroFrame *frame) {
    *(ZeroFrame **) frame = _top_zero_frame;
    _top_zero_frame = frame;
  }
  void pop_zero_frame() {
    zero_stack()->set_sp((intptr_t *) _top_zero_frame + 1);
    _top_zero_frame = *(ZeroFrame **) _top_zero_frame;
  }

 public:
  static ByteSize zero_stack_offset() {
    return byte_offset_of(JavaThread, _zero_stack);
  }
  static ByteSize top_zero_frame_offset() {
    return byte_offset_of(JavaThread, _top_zero_frame);
  }

 public:
  void record_base_of_stack_pointer() {
  }
  void set_base_of_stack_pointer(intptr_t* base_sp) {
  }

 public:
  void set_last_Java_frame() {
    set_last_Java_frame(top_zero_frame(), zero_stack()->sp());
  }
  void reset_last_Java_frame() {
    frame_anchor()->zap();
  }
  void set_last_Java_frame(ZeroFrame* fp, intptr_t* sp) {
    frame_anchor()->set(sp, NULL, fp);
  }

 public:
  ZeroFrame* last_Java_fp() {
    return frame_anchor()->last_Java_fp();
  }

 private:
  frame pd_last_frame();

 public:
  static ByteSize last_Java_fp_offset() {
    return byte_offset_of(JavaThread, _anchor) + JavaFrameAnchor::last_Java_fp_offset();
  }

 public:
  // Check for pending suspend requests and pending asynchronous
  // exceptions.  There are separate accessors for these, but
  // _suspend_flags is volatile so using them would be unsafe.
  bool has_special_condition_for_native_trans() {
    return _suspend_flags != 0;
  }

 public:
  bool pd_get_top_frame_for_signal_handler(frame* fr_addr,
                                           void* ucontext,
                                           bool isInJava) {
    ShouldNotCallThis();
    return false;
  }

  // These routines are only used on cpu architectures that
  // have separate register stacks (Itanium).
  static bool register_stack_overflow() { return false; }
  static void enable_register_stack_guard() { }
  static void disable_register_stack_guard() { }

#endif
