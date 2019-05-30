#include "precompiled.hpp"
#include "utilities/globalCounter.hpp"
#include "runtime/orderAccess.hpp"
#include "runtime/thread.hpp"
#include "runtime/threadSMR.inline.hpp"
#include "runtime/vmThread.hpp"
#include "utilities/spinYield.hpp"

GlobalCounter::PaddedCounter GlobalCounter::_global_counter;

class GlobalCounter::CounterThreadCheck : public ThreadClosure {
 private:
  uintx _gbl_cnt;
 public:
  CounterThreadCheck(uintx gbl_cnt) : _gbl_cnt(gbl_cnt) {}
  void do_thread(Thread* thread) {
    SpinYield yield;
    // Loops on this thread until it has exited the critical read section.
    while(true) {
      uintx cnt = OrderAccess::load_acquire(thread->get_rcu_counter());
      // This checks if the thread's counter is active. And if so is the counter
      // for a pre-existing reader (belongs to this grace period). A pre-existing
      // reader will have a lower counter than the global counter version for this
      // generation. If the counter is larger than the global counter version this
      //  is a new reader and we can continue.
      if (((cnt & COUNTER_ACTIVE) != 0) && (cnt - _gbl_cnt) > (max_uintx / 2)) {
        yield.wait();
      } else {
        break;
      }
    }
  }
};

void GlobalCounter::write_synchronize() {
  assert((*Thread::current()->get_rcu_counter() & COUNTER_ACTIVE) == 0x0, "must be outside a critcal section");
  // Atomic::add must provide fence since we have storeload dependency.
  volatile uintx gbl_cnt = Atomic::add((uintx)COUNTER_INCREMENT, &_global_counter._counter,
                                       memory_order_conservative);
  // Do all RCU threads.
  CounterThreadCheck ctc(gbl_cnt);
  for (JavaThreadIteratorWithHandle jtiwh; JavaThread *thread = jtiwh.next(); ) {
    ctc.do_thread(thread);
  }
  ctc.do_thread(VMThread::vm_thread());
}
