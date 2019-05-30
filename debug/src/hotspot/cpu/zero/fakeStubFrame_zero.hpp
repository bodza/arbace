#ifndef CPU_ZERO_VM_FAKESTUBFRAME_ZERO_HPP
#define CPU_ZERO_VM_FAKESTUBFRAME_ZERO_HPP

#include "stack_zero.hpp"

// |  ...               |
// +--------------------+  ------------------
// | frame_type         |       low addresses
// | next_frame         |      high addresses
// +--------------------+  ------------------
// |  ...               |

class FakeStubFrame : public ZeroFrame {
 private:
  FakeStubFrame() : ZeroFrame() {
    ShouldNotCallThis();
  }

 protected:
  enum Layout {
    header_words = jf_header_words
  };

 public:
  static FakeStubFrame *build(TRAPS);

 public:
  void identify_word(int   frame_index,
                     int   offset,
                     char* fieldbuf,
                     char* valuebuf,
                     int   buflen) const {}
};

#endif
