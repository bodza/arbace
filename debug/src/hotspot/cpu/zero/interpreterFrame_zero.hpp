#ifndef CPU_ZERO_VM_INTERPRETERFRAME_ZERO_HPP
#define CPU_ZERO_VM_INTERPRETERFRAME_ZERO_HPP

#include "interpreter/bytecodeInterpreter.hpp"
#include "oops/method.hpp"
#include "runtime/thread.hpp"
#include "stack_zero.hpp"
#include "utilities/align.hpp"

#ifdef CC_INTERP
// |  ...               |
// +--------------------+  ------------------
// | stack slot n-1     |       low addresses
// |  ...               |
// | stack slot 0       |
// | monitor 0 (maybe)  |
// |  ...               |
// | interpreter state  |
// |  ...               |
// | frame_type         |
// | next_frame         |      high addresses
// +--------------------+  ------------------
// |  ...               |

class InterpreterFrame : public ZeroFrame {
  friend class AbstractInterpreter;

 private:
  InterpreterFrame() : ZeroFrame() {
    ShouldNotCallThis();
  }

 protected:
  enum Layout {
    istate_off = jf_header_words + (align_up_(sizeof(BytecodeInterpreter), wordSize) >> LogBytesPerWord) - 1,
    header_words
  };

 public:
  static InterpreterFrame *build(Method* const method, TRAPS);
  static InterpreterFrame *build(int size, TRAPS);

 public:
  interpreterState interpreter_state() const {
    return (interpreterState) addr_of_word(istate_off);
  }

 public:
  void identify_word(int   frame_index,
                     int   offset,
                     char* fieldbuf,
                     char* valuebuf,
                     int   buflen) const;
};
#endif

#endif
