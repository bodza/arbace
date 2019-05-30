// no precompiled headers
#include "memory/allocation.inline.hpp"
#include "runtime/mutexLocker.hpp"
#include "runtime/osThread.hpp"

#include <signal.h>

void OSThread::pd_initialize() {
  assert(this != NULL, "check");
#ifdef __APPLE__
  _thread_id        = 0;
#else
  _thread_id        = NULL;
#endif
  _pthread_id       = NULL;
  _siginfo = NULL;
  _ucontext = NULL;
  _expanding_stack = 0;
  _alt_sig_stack = NULL;

  sigemptyset(&_caller_sigmask);

  _startThread_lock = new Monitor(Mutex::event, "startThread_lock", true,
                                  Monitor::_safepoint_check_never);
  assert(_startThread_lock !=NULL, "check");
}

void OSThread::pd_destroy() {
  delete _startThread_lock;
}
