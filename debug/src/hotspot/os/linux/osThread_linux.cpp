// no precompiled headers
#include "memory/allocation.inline.hpp"
#include "runtime/mutex.hpp"
#include "runtime/osThread.hpp"

#include <signal.h>

void OSThread::pd_initialize() {
  _thread_id        = 0;
  _pthread_id       = 0;
  _siginfo = NULL;
  _ucontext = NULL;
  _expanding_stack = 0;
  _alt_sig_stack = NULL;

  sigemptyset(&_caller_sigmask);

  _startThread_lock = new Monitor(Mutex::event, "startThread_lock", true,
                                  Monitor::_safepoint_check_never);
}

void OSThread::pd_destroy() {
  delete _startThread_lock;
}
