#ifndef OS_CPU_LINUX_X86_VM_THREAD_LINUX_X86_HPP
#define OS_CPU_LINUX_X86_VM_THREAD_LINUX_X86_HPP

 private:
  void pd_initialize() {
    _anchor.clear();
  }

  frame pd_last_frame();

 public:
  // Mutators are highly dangerous....
  intptr_t* last_Java_fp()              { return _anchor.last_Java_fp(); }
  void  set_last_Java_fp(intptr_t* fp)  { _anchor.set_last_Java_fp(fp); }

  void set_base_of_stack_pointer(intptr_t* base_sp) { }

  static ByteSize last_Java_fp_offset() {
    return byte_offset_of(JavaThread, _anchor) + JavaFrameAnchor::last_Java_fp_offset();
  }

  intptr_t* base_of_stack_pointer() {
    return NULL;
  }

  void record_base_of_stack_pointer() { }

  bool pd_get_top_frame_for_signal_handler(frame* fr_addr, void* ucontext, bool isInJava);
  bool pd_get_top_frame_for_profiling(frame* fr_addr, void* ucontext, bool isInJava);
private:
  bool pd_get_top_frame(frame* fr_addr, void* ucontext, bool isInJava);
public:
  // These routines are only used on cpu architectures that
  // have separate register stacks (Itanium).
  static bool register_stack_overflow() { return false; }
  static void enable_register_stack_guard() { }
  static void disable_register_stack_guard() { }

#endif
