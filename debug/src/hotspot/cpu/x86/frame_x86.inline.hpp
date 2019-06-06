#ifndef CPU_X86_VM_FRAME_X86_INLINE_HPP
#define CPU_X86_VM_FRAME_X86_INLINE_HPP

#include "code/codeCache.hpp"
#include "code/vmreg.inline.hpp"

// Inline functions for Intel frames:

// Constructors:

inline frame::frame() {
  _pc = NULL;
  _sp = NULL;
  _unextended_sp = NULL;
  _fp = NULL;
  _cb = NULL;
  _deopt_state = unknown;
}

inline void frame::init(intptr_t* sp, intptr_t* fp, address pc) {
  _sp = sp;
  _unextended_sp = sp;
  _fp = fp;
  _pc = pc;
  _cb = CodeCache::find_blob(pc);
  adjust_unextended_sp();

  address original_pc = CompiledMethod::get_deopt_original_pc(this);
  if (original_pc != NULL) {
    _pc = original_pc;
    _deopt_state = is_deoptimized;
  } else {
    _deopt_state = not_deoptimized;
  }
}

inline frame::frame(intptr_t* sp, intptr_t* fp, address pc) {
  init(sp, fp, pc);
}

inline frame::frame(intptr_t* sp, intptr_t* unextended_sp, intptr_t* fp, address pc) {
  _sp = sp;
  _unextended_sp = unextended_sp;
  _fp = fp;
  _pc = pc;
  _cb = CodeCache::find_blob(pc);
  adjust_unextended_sp();

  address original_pc = CompiledMethod::get_deopt_original_pc(this);
  if (original_pc != NULL) {
    _pc = original_pc;
    _deopt_state = is_deoptimized;
  } else {
    if (_cb->is_deoptimization_stub()) {
      _deopt_state = is_deoptimized;
    } else {
      _deopt_state = not_deoptimized;
    }
  }
}

inline frame::frame(intptr_t* sp, intptr_t* fp) {
  _sp = sp;
  _unextended_sp = sp;
  _fp = fp;
  _pc = (address)(sp[-1]);

  // Here's a sticky one. This constructor can be called via AsyncGetCallTrace
  // when last_Java_sp is non-null but the pc fetched is junk. If we are truly
  // unlucky the junk value could be to a zombied method and we'll die on the
  // find_blob call. This is also why we can have no asserts on the validity
  // of the pc we find here. AsyncGetCallTrace -> pd_get_top_frame_for_signal_handler
  // -> pd_last_frame should use a specialized version of pd_last_frame which could
  // call a specialized frame constructor instead of this one.
  // Then we could use the assert below. However this assert is of somewhat dubious
  // value.
  // UPDATE: this constructor is only used by trace_method_handle_stub() now.

  _cb = CodeCache::find_blob(_pc);
  adjust_unextended_sp();

  address original_pc = CompiledMethod::get_deopt_original_pc(this);
  if (original_pc != NULL) {
    _pc = original_pc;
    _deopt_state = is_deoptimized;
  } else {
    _deopt_state = not_deoptimized;
  }
}

// Accessors

inline bool frame::equal(frame other) const {
  bool ret =  sp() == other.sp() && unextended_sp() == other.unextended_sp() && fp() == other.fp() && pc() == other.pc();
  return ret;
}

// Return unique id for this frame. The id must have a value where we can distinguish
// identity and younger/older relationship. NULL represents an invalid (incomparable)
// frame.
inline intptr_t* frame::id(void) const { return unextended_sp(); }

// Relationals on frames based
// Return true if the frame is younger (more recent activation) than the frame represented by id
inline bool frame::is_younger(intptr_t* id) const { return this->id() < id; }

// Return true if the frame is older (less recent activation) than the frame represented by id
inline bool frame::is_older(intptr_t* id) const { return this->id() > id; }

inline intptr_t* frame::link() const { return (intptr_t*) *(intptr_t **)addr_at(link_offset); }

inline intptr_t* frame::unextended_sp() const { return _unextended_sp; }

// Return address:

inline address* frame::sender_pc_addr() const { return (address*) addr_at( return_addr_offset); }
inline address frame::sender_pc() const { return *sender_pc_addr(); }

inline intptr_t* frame::sender_sp() const { return addr_at( sender_sp_offset); }

// Entry frames

inline JavaCallWrapper** frame::entry_frame_call_wrapper_addr() const {
 return (JavaCallWrapper**)addr_at(entry_frame_call_wrapper_offset);
}

// Compiled frames

inline oop frame::saved_oop_result(RegisterMap* map) const {
  oop* result_adr = (oop *)map->location(rax->as_VMReg());
  guarantee(result_adr != NULL, "bad register save location");

  return (*result_adr);
}

inline void frame::set_saved_oop_result(RegisterMap* map, oop obj) {
  oop* result_adr = (oop *)map->location(rax->as_VMReg());
  guarantee(result_adr != NULL, "bad register save location");

  *result_adr = obj;
}

#endif
