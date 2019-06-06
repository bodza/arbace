#include "precompiled.hpp"

#include "jvm.h"
#include "classfile/systemDictionary.hpp"
#include "code/codeCache.hpp"
#include "code/icBuffer.hpp"
#include "code/nmethod.hpp"
#include "code/vtableStubs.hpp"
#include "compiler/compileBroker.hpp"
#include "compiler/disassembler.hpp"
#include "gc/shared/collectedHeap.hpp"
#include "memory/resourceArea.hpp"
#include "memory/universe.hpp"
#include "oops/oop.inline.hpp"
#include "prims/privilegedStack.hpp"
#include "runtime/arguments.hpp"
#include "runtime/atomic.hpp"
#include "runtime/flags/flagSetting.hpp"
#include "runtime/frame.inline.hpp"
#include "runtime/handles.inline.hpp"
#include "runtime/java.hpp"
#include "runtime/os.hpp"
#include "runtime/sharedRuntime.hpp"
#include "runtime/stubCodeGenerator.hpp"
#include "runtime/stubRoutines.hpp"
#include "runtime/thread.inline.hpp"
#include "runtime/vframe.hpp"
#include "runtime/vm_version.hpp"
#include "utilities/defaultStream.hpp"
#include "utilities/formatBuffer.hpp"
#include "utilities/globalDefinitions.hpp"
#include "utilities/macros.hpp"
#include "utilities/vmError.hpp"

#include <stdio.h>

ATTRIBUTE_PRINTF(1, 2)
void warning(const char* format, ...) {
  if (BreakAtWarning) BREAKPOINT;
}

// Place-holder for non-existent suppression check:
#define error_is_suppressed(file_name, line_no) (false)

void report_vm_error(const char* file, int line, const char* error_msg) {
  report_vm_error(file, line, error_msg, "%s", "");
}

void report_vm_error(const char* file, int line, const char* error_msg, const char* detail_fmt, ...) {
  if (error_is_suppressed(file, line)) return;
  va_list detail_args;
  va_start(detail_args, detail_fmt);
  void* context = NULL;
  VMError::report_and_die(Thread::current_or_null(), context, file, line, error_msg, detail_fmt, detail_args);
  va_end(detail_args);
}

void report_vm_status_error(const char* file, int line, const char* error_msg, int status, const char* detail) {
  report_vm_error(file, line, error_msg, "error %s(%d), %s", os::errno_name(status), status, detail);
}

void report_fatal(const char* file, int line, const char* detail_fmt, ...) {
  if (error_is_suppressed(file, line)) return;
  va_list detail_args;
  va_start(detail_args, detail_fmt);
  void* context = NULL;
  VMError::report_and_die(Thread::current_or_null(), context, file, line, "fatal error", detail_fmt, detail_args);
  va_end(detail_args);
}

void report_vm_out_of_memory(const char* file, int line, size_t size, VMErrorType vm_err_type, const char* detail_fmt, ...) {
  va_list detail_args;
  va_start(detail_args, detail_fmt);
  VMError::report_and_die(Thread::current_or_null(), file, line, size, vm_err_type, detail_fmt, detail_args);
  va_end(detail_args);

  // The UseOSErrorReporting option in report_and_die() may allow a return
  // to here. If so then we'll have to figure out how to handle it.
  guarantee(false, "report_and_die() should not return here");
}

void report_should_not_call(const char* file, int line) {
  report_vm_error(file, line, "ShouldNotCall()");
}

void report_should_not_reach_here(const char* file, int line) {
  report_vm_error(file, line, "ShouldNotReachHere()");
}

void report_unimplemented(const char* file, int line) {
  report_vm_error(file, line, "Unimplemented()");
}

void report_untested(const char* file, int line, const char* message) { }

void report_java_out_of_memory(const char* message) {
  static int out_of_memory_reported = 0;

  // A number of threads may attempt to report OutOfMemoryError at around the
  // same time. To avoid dumping the heap or executing the data collection
  // commands multiple times we just do it once when the first threads reports
  // the error.
  if (Atomic::cmpxchg(1, &out_of_memory_reported, 0) == 0) {
    if (OnOutOfMemoryError && OnOutOfMemoryError[0]) {
      VMError::report_java_out_of_memory(message);
    }

    if (CrashOnOutOfMemoryError) {
      tty->print_cr("Aborting due to java.lang.OutOfMemoryError: %s", message);
      fatal("OutOfMemory encountered: %s", message);
    }

    if (ExitOnOutOfMemoryError) {
      tty->print_cr("Terminating due to java.lang.OutOfMemoryError: %s", message);
      os::exit(3);
    }
  }
}

// ------ helper functions for debugging go here ------------

// All debug entries should be wrapped with a stack allocated
// Command object. It makes sure a resource mark is set and
// flushes the logfile to prevent file sharing problems.

class Command : public StackObj {
 private:
  ResourceMark rm;
  ResetNoHandleMark rnhm;
  HandleMark   hm;
 public:
  static int level;
  Command(const char* str) {
    if (level++ > 0)  return;
    tty->cr();
    tty->print_cr("\"Executing %s\"", str);
  }

  ~Command() {
    tty->flush();
    level--;
  }
};

int Command::level = 0;

extern "C" void ps() { // print stack
  if (Thread::current_or_null() == NULL) return;
  Command c("ps");

  // Prints the stack of the current Java thread
  JavaThread* p = JavaThread::active();
  tty->print(" for thread: ");
  p->print();
  tty->cr();

  if (p->has_last_Java_frame()) {
    // If the last_Java_fp is set we are in C land and
    // can call the standard stack_trace function.
    p->print_stack();
  } else {
    tty->print_cr("Cannot find the last Java frame, printing stack disabled.");
  }
}

extern "C" void pfl() {
  // print frame layout
  Command c("pfl");
  JavaThread* p = JavaThread::active();
  tty->print(" for thread: ");
  p->print();
  tty->cr();
  if (p->has_last_Java_frame()) {
    p->print_frame_layout();
  }
}

extern "C" void pss() { // print all stacks
  if (Thread::current_or_null() == NULL) return;
  Command c("pss");
  Threads::print(true, false);
}
