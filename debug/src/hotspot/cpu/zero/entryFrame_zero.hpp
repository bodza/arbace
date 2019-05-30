#ifndef CPU_ZERO_VM_ENTRYFRAME_ZERO_HPP
#define CPU_ZERO_VM_ENTRYFRAME_ZERO_HPP

#include "runtime/javaCalls.hpp"
#include "stack_zero.hpp"

// |  ...               |
// +--------------------+  ------------------
// | parameter n-1      |       low addresses
// |  ...               |
// | parameter 0        |
// | call_wrapper       |
// | frame_type         |
// | next_frame         |      high addresses
// +--------------------+  ------------------
// |  ...               |

class EntryFrame : public ZeroFrame {
 private:
  EntryFrame() : ZeroFrame() {
    ShouldNotCallThis();
  }

 protected:
  enum Layout {
    call_wrapper_off = jf_header_words,
    header_words
  };

 public:
  static EntryFrame *build(const intptr_t*  parameters,
                           int              parameter_words,
                           JavaCallWrapper* call_wrapper,
                           TRAPS);
 public:
  JavaCallWrapper **call_wrapper() const {
    return (JavaCallWrapper **) addr_of_word(call_wrapper_off);
  }

 public:
  void identify_word(int   frame_index,
                     int   offset,
                     char* fieldbuf,
                     char* valuebuf,
                     int   buflen) const;
};

#endif
