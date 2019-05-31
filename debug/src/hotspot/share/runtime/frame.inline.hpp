#ifndef SHARE_VM_RUNTIME_FRAME_INLINE_HPP
#define SHARE_VM_RUNTIME_FRAME_INLINE_HPP

#include "code/compiledMethod.inline.hpp"
#include "interpreter/bytecodeInterpreter.hpp"
#include "interpreter/bytecodeInterpreter.inline.hpp"
#include "interpreter/interpreter.hpp"
#include "oops/method.hpp"
#include "runtime/frame.hpp"
#include "runtime/signature.hpp"
#include "utilities/macros.hpp"
#ifdef ZERO
# include "entryFrame_zero.hpp"
# include "fakeStubFrame_zero.hpp"
# include "interpreterFrame_zero.hpp"
#endif

#include CPU_HEADER_INLINE(frame)

inline bool frame::is_entry_frame() const {
  return StubRoutines::returns_to_call_stub(pc());
}

inline bool frame::is_stub_frame() const {
  return StubRoutines::is_stub_code(pc()) || (_cb != NULL && _cb->is_adapter_blob());
}

inline bool frame::is_first_frame() const {
  return is_entry_frame() && entry_frame_is_first();
}

inline oop* frame::oopmapreg_to_location(VMReg reg, const RegisterMap* reg_map) const {
  if (reg->is_reg()) {
    // If it is passed in a register, it got spilled in the stub frame.
    return (oop *)reg_map->location(reg);
  } else {
    int sp_offset_in_bytes = reg->reg2stack() * VMRegImpl::stack_slot_size;
    return (oop*)(((address)unextended_sp()) + sp_offset_in_bytes);
  }
}

inline bool StackFrameStream::is_done() {
  return (_is_done) ? true : (_is_done = _fr.is_first_frame(), false);
}

#endif
