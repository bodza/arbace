#ifndef SHARE_UTILITIES_GLOBAL_COUNTER_INLINE_HPP
#define SHARE_UTILITIES_GLOBAL_COUNTER_INLINE_HPP

#include "runtime/orderAccess.hpp"
#include "runtime/thread.inline.hpp"
#include "utilities/globalCounter.hpp"

inline void GlobalCounter::critical_section_begin(Thread *thread) {
  uintx gbl_cnt = OrderAccess::load_acquire(&_global_counter._counter);
  OrderAccess::release_store_fence(thread->get_rcu_counter(), gbl_cnt | COUNTER_ACTIVE);
}

inline void GlobalCounter::critical_section_end(Thread *thread) {
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
