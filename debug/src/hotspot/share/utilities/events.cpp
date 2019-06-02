#include "precompiled.hpp"

#include "memory/allocation.inline.hpp"
#include "runtime/mutexLocker.hpp"
#include "runtime/os.inline.hpp"
#include "runtime/osThread.hpp"
#include "runtime/thread.inline.hpp"
#include "runtime/threadCritical.hpp"
#include "runtime/timer.hpp"
#include "utilities/events.hpp"

EventLog* Events::_logs = NULL;
StringEventLog* Events::_messages = NULL;
StringEventLog* Events::_exceptions = NULL;
StringEventLog* Events::_redefinitions = NULL;
StringEventLog* Events::_deopt_messages = NULL;

EventLog::EventLog() {
  // This normally done during bootstrap when we're only single
  // threaded but use a ThreadCritical to ensure inclusion in case
  // some are created slightly late.
  ThreadCritical tc;
  _next = Events::_logs;
  Events::_logs = this;
}

// For each registered event logger, print out the current contents of
// the buffer.  This is normally called when the JVM is crashing.
void Events::print_all(outputStream* out) {
  EventLog* log = _logs;
  while (log != NULL) {
    log->print_log_on(out);
    log = log->next();
  }
}

void Events::print() {
  print_all(tty);
}

void Events::init() { }

void eventlog_init() {
  Events::init();
}

///////////////////////////////////////////////////////////////////////////
// EventMark

EventMark::EventMark(const char* format, ...) { }

EventMark::~EventMark() { }
