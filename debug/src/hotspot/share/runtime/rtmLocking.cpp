#include "precompiled.hpp"
#include "compiler/compilerDefinitions.hpp"

#if INCLUDE_RTM_OPT

#include "memory/allocation.inline.hpp"
#include "runtime/task.hpp"
#include "runtime/rtmLocking.hpp"

// One-shot PeriodicTask subclass for enabling RTM locking
uintx RTMLockingCounters::_calculation_flag = 0;

class RTMLockingCalculationTask : public PeriodicTask {
 public:
  RTMLockingCalculationTask(size_t interval_time) : PeriodicTask(interval_time){  }

  virtual void task() {
    RTMLockingCounters::_calculation_flag = 1;
    // Reclaim our storage and disenroll ourself
    delete this;
  }
};

void RTMLockingCounters::init() {
  if (UseRTMLocking && RTMLockingCalculationDelay > 0) {
    RTMLockingCalculationTask* task = new RTMLockingCalculationTask(RTMLockingCalculationDelay);
    task->enroll();
  } else {
    _calculation_flag = 1;
  }
}

//------------------------------print_on-------------------------------
void RTMLockingCounters::print_on(outputStream* st) {
  tty->print_cr("# rtm locks total (estimated): " UINTX_FORMAT, _total_count * RTMTotalCountIncrRate);
  tty->print_cr("# rtm lock aborts  : " UINTX_FORMAT, _abort_count);
  for (int i = 0; i < ABORT_STATUS_LIMIT; i++) {
    tty->print_cr("# rtm lock aborts %d: " UINTX_FORMAT, i, _abortX_count[i]);
  }
}

#endif
