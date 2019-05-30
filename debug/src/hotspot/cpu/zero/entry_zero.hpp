#ifndef CPU_ZERO_VM_ENTRY_ZERO_HPP
#define CPU_ZERO_VM_ENTRY_ZERO_HPP

#include "interpreter/cppInterpreter.hpp"

class ZeroEntry {
 public:
  ZeroEntry() {
    ShouldNotCallThis();
  }

 private:
  address _entry_point;

 public:
  address entry_point() const {
    return _entry_point;
  }
  void set_entry_point(address entry_point) {
    _entry_point = entry_point;
  }

 private:
  typedef int (*NormalEntryFunc)(Method* method, intptr_t  base_pc, TRAPS);
  typedef int (*OSREntryFunc)(Method* method, address   osr_buf, intptr_t  base_pc, TRAPS);

 public:
  void invoke(Method* method, TRAPS) const {
    maybe_deoptimize(((NormalEntryFunc) entry_point())(method, (intptr_t) this, THREAD), THREAD);
  }
  void invoke_osr(Method* method, address osr_buf, TRAPS) const {
    maybe_deoptimize(((OSREntryFunc) entry_point())(method, osr_buf, (intptr_t) this, THREAD), THREAD);
  }

 private:
  static void maybe_deoptimize(int deoptimized_frames, TRAPS) {
    if (deoptimized_frames)
      CppInterpreter::main_loop(deoptimized_frames - 1, THREAD);
  }

 public:
  static ByteSize entry_point_offset() {
    return byte_offset_of(ZeroEntry, _entry_point);
  }
};

#endif
