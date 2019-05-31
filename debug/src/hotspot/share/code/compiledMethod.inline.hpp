#ifndef SHARE_VM_CODE_COMPILEDMETHOD_INLINE_HPP
#define SHARE_VM_CODE_COMPILEDMETHOD_INLINE_HPP

#include "code/compiledMethod.hpp"
#include "code/nativeInst.hpp"
#include "runtime/frame.hpp"
#include "runtime/orderAccess.hpp"

inline bool CompiledMethod::is_deopt_pc(address pc) { return is_deopt_entry(pc) || is_deopt_mh_entry(pc); }

// When using JVMCI the address might be off by the size of a call instruction.
inline bool CompiledMethod::is_deopt_entry(address pc) {
  return pc == deopt_handler_begin() || (is_compiled_by_jvmci() && pc == (deopt_handler_begin() + NativeCall::instruction_size));
}

inline void CompiledMethod::release_set_exception_cache(ExceptionCache *ec) {
  OrderAccess::release_store(&_exception_cache, ec);
}

// -----------------------------------------------------------------------------
// CompiledMethod::get_deopt_original_pc
//
// Return the original PC for the given PC if:
// (a) the given PC belongs to a nmethod and
// (b) it is a deopt PC

inline address CompiledMethod::get_deopt_original_pc(const frame* fr) {
  if (fr->cb() == NULL)  return NULL;

  CompiledMethod* cm = fr->cb()->as_compiled_method_or_null();
  if (cm != NULL && cm->is_deopt_pc(fr->pc()))
    return cm->get_original_pc(fr);

  return NULL;
}

// class ExceptionCache methods

inline int ExceptionCache::count() { return OrderAccess::load_acquire(&_count); }

address ExceptionCache::pc_at(int index) {
  return _pc[index];
}

address ExceptionCache::handler_at(int index) {
  return _handler[index];
}

// increment_count is only called under lock, but there may be concurrent readers.
inline void ExceptionCache::increment_count() { OrderAccess::release_store(&_count, _count + 1); }

#endif
