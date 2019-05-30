#ifndef SHARE_UTILITIES_GLOBAL_COUNTER_INLINE_HPP
#define SHARE_UTILITIES_GLOBAL_COUNTER_INLINE_HPP

#include "runtime/orderAccess.hpp"
#include "runtime/thread.inline.hpp"
#include "utilities/globalCounter.hpp"

inline void GlobalCounter::critical_section_begin(Thread *thread) {
  assert(thread == Thread::current(), "must be current thread");
  assert(thread->is_VM_thread() || thread->is_Java_thread(), "must be VMThread or JavaThread");
  assert((*thread->get_rcu_counter() & COUNTER_ACTIVE) == 0x0, "nestled critical sections, not supported yet");
  uintx gbl_cnt = OrderAccess::load_acquire(&_global_counter._counter);
  OrderAccess::release_store_fence(thread->get_rcu_counter(), gbl_cnt | COUNTER_ACTIVE);
}

inline void GlobalCounter::critical_section_end(Thread *thread) {
  assert(thread == Thread::current(), "must be current thread");
  assert(thread->is_VM_thread() || thread->is_Java_thread(), "must be VMThread or JavaThread");
  assert((*thread->get_rcu_counter() & COUNTER_ACTIVE) == COUNTER_ACTIVE, "must be in ctitical section");
  // Mainly for debugging we set it to 'now'.
  uintx gbl_cnt = OrderAccess::load_acquire(&_global_counter._counter);
  OrderAccess::release_store(thread->get_rcu_counter(), gbl_cnt);
}

class GlobalCounter::CriticalSection {
 private:
  Thread* _thread;
 public:
  inline CriticalSection(Thread* thread) : _thread(thread) {
    GlobalCounter::critical_section_begin(_thread);
  }
  inline  ~CriticalSection() {
    GlobalCounter::critical_section_end(_thread);
  }
};

#endif
