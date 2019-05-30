#ifndef CPU_ZERO_VM_STACK_ZERO_INLINE_HPP
#define CPU_ZERO_VM_STACK_ZERO_INLINE_HPP

#include "runtime/thread.hpp"
#include "stack_zero.hpp"

inline void ZeroStack::overflow_check(int required_words, TRAPS) {
  // Check the Zero stack
  if (available_words() < required_words) {
    handle_overflow(THREAD);
    return;
  }

  // Check the ABI stack
  if (abi_stack_available(THREAD) < 0) {
    handle_overflow(THREAD);
    return;
  }
}

// This method returns the amount of ABI stack available for us
// to use under normal circumstances.  Note that the returned
// value can be negative.
inline int ZeroStack::abi_stack_available(Thread *thread) const {
  guarantee(Thread::current() == thread, "should run in the same thread");
  int stack_used = thread->stack_base() - (address) &stack_used
    + (JavaThread::stack_guard_zone_size() + JavaThread::stack_shadow_zone_size());
  int stack_free = thread->stack_size() - stack_used;
  return stack_free;
}

#endif
