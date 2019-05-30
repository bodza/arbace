#ifndef CPU_ZERO_VM_FRAME_ZERO_HPP
#define CPU_ZERO_VM_FRAME_ZERO_HPP

#include "runtime/synchronizer.hpp"

// A frame represents a physical stack frame on the Zero stack.

 public:
  enum {
    pc_return_offset = 0
  };

  // Constructor
 public:
  frame(ZeroFrame* zeroframe, intptr_t* sp);

 private:
  ZeroFrame* _zeroframe;

 public:
  const ZeroFrame *zeroframe() const {
    return _zeroframe;
  }

  intptr_t* fp() const {
    return (intptr_t *) zeroframe();
  }

#ifdef CC_INTERP
  inline interpreterState get_interpreterState() const;
#endif

 public:
  const EntryFrame *zero_entryframe() const {
    return zeroframe()->as_entry_frame();
  }
  const InterpreterFrame *zero_interpreterframe() const {
    return zeroframe()->as_interpreter_frame();
  }

 public:
  bool is_fake_stub_frame() const;

 public:
  frame sender_for_nonentry_frame(RegisterMap* map) const;

 public:
  void zero_print_on_error(int           index,
                           outputStream* st,
                           char*         buf,
                           int           buflen) const;

  static jint interpreter_frame_expression_stack_direction() { return -1; }

#endif
