#include "precompiled.hpp"

#include "memory/allocation.inline.hpp"
#include "runtime/jniPeriodicChecker.hpp"
#include "runtime/task.hpp"

// --------------------------------------------------------
// Class to aid in periodic checking under CheckJNICalls
class JniPeriodicCheckerTask : public PeriodicTask {
  public:
     JniPeriodicCheckerTask(int interval_time) : PeriodicTask(interval_time) { }
     void task() { os::run_periodic_checks(); }
     static void engage();
     static void disengage();
};

//----------------------------------------------------------
// Implementation of JniPeriodicChecker

JniPeriodicCheckerTask*              JniPeriodicChecker::_task   = NULL;

/*
 * The engage() method is called at initialization time via
 * Thread::create_vm() to initialize the JniPeriodicChecker and
 * register it with the WatcherThread as a periodic task.
 */
void JniPeriodicChecker::engage() {
  if (CheckJNICalls && !is_active()) {
    // start up the periodic task
    _task = new JniPeriodicCheckerTask(10);
    _task->enroll();
  }
}

/*
 * the disengage() method is responsible for deactivating the periodic
 * task. This  method is called from before_exit() in java.cpp and is only called
 * after the WatcherThread has been stopped.
 */
void JniPeriodicChecker::disengage() {
  if (CheckJNICalls && is_active()) {
    // remove JniPeriodicChecker
    _task->disenroll();
    delete _task;
    _task = NULL;
  }
}

void jniPeriodicChecker_exit() {
  if (!CheckJNICalls) return;
}
