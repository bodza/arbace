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
#include "interpreter/bytecodeHistogram.hpp"
#include "interpreter/interpreter.hpp"
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
#include "services/heapDumper.hpp"
#include "utilities/defaultStream.hpp"
#include "utilities/events.hpp"
#include "utilities/formatBuffer.hpp"
#include "utilities/globalDefinitions.hpp"
#include "utilities/macros.hpp"
#include "utilities/vmError.hpp"

#include <stdio.h>

// Support for showing register content on asserts/guarantees.
#ifdef CAN_SHOW_REGISTERS_ON_ASSERT
static char g_dummy;
char* g_assert_poison = &g_dummy;
static intx g_asserting_thread = 0;
static void* g_assertion_context = NULL;
#endif

ATTRIBUTE_PRINTF(1, 2)
void warning(const char* format, ...) {
  if (PrintWarnings) {
    FILE* const err = defaultStream::error_stream();
    jio_fprintf(err, "%s warning: ", VM_Version::vm_name());
    va_list ap;
    va_start(ap, format);
    vfprintf(err, format, ap);
    va_end(ap);
    fputc('\n', err);
  }
  if (BreakAtWarning) BREAKPOINT;
}

// Place-holder for non-existent suppression check:
#define error_is_suppressed(file_name, line_no) (false)

void report_vm_error(const char* file, int line, const char* error_msg)
{
  report_vm_error(file, line, error_msg, "%s", "");
}

void report_vm_error(const char* file, int line, const char* error_msg, const char* detail_fmt, ...)
{
  if (Debugging || error_is_suppressed(file, line)) return;
  va_list detail_args;
  va_start(detail_args, detail_fmt);
  void* context = NULL;
#ifdef CAN_SHOW_REGISTERS_ON_ASSERT
  if (g_assertion_context != NULL && os::current_thread_id() == g_asserting_thread) {
    context = g_assertion_context;
  }
#endif
  VMError::report_and_die(Thread::current_or_null(), context, file, line, error_msg, detail_fmt, detail_args);
  va_end(detail_args);
}

void report_vm_status_error(const char* file, int line, const char* error_msg, int status, const char* detail) {
  report_vm_error(file, line, error_msg, "error %s(%d), %s", os::errno_name(status), status, detail);
}

void report_fatal(const char* file, int line, const char* detail_fmt, ...)
{
  if (Debugging || error_is_suppressed(file, line)) return;
  va_list detail_args;
  va_start(detail_args, detail_fmt);
  void* context = NULL;
#ifdef CAN_SHOW_REGISTERS_ON_ASSERT
  if (g_assertion_context != NULL && os::current_thread_id() == g_asserting_thread) {
    context = g_assertion_context;
  }
#endif
  VMError::report_and_die(Thread::current_or_null(), context, file, line, "fatal error", detail_fmt, detail_args);
  va_end(detail_args);
}

void report_vm_out_of_memory(const char* file, int line, size_t size, VMErrorType vm_err_type, const char* detail_fmt, ...) {
  if (Debugging) return;
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

void report_untested(const char* file, int line, const char* message) {
}

void report_java_out_of_memory(const char* message) {
  static int out_of_memory_reported = 0;

  // A number of threads may attempt to report OutOfMemoryError at around the
  // same time. To avoid dumping the heap or executing the data collection
  // commands multiple times we just do it once when the first threads reports
  // the error.
  if (Atomic::cmpxchg(1, &out_of_memory_reported, 0) == 0) {
    // create heap dump before OnOutOfMemoryError commands are executed
    if (HeapDumpOnOutOfMemoryError) {
      tty->print_cr("java.lang.OutOfMemoryError: %s", message);
      HeapDumper::dump_heap_from_oome();
    }

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
  bool debug_save;
 public:
  static int level;
  Command(const char* str) {
    debug_save = Debugging;
    Debugging = true;
    if (level++ > 0)  return;
    tty->cr();
    tty->print_cr("\"Executing %s\"", str);
  }

  ~Command() {
        tty->flush();
        Debugging = debug_save;
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

//////////////////////////////////////////////////////////////////////////////
// Test multiple STATIC_ASSERT forms in various scopes.

// Support for showing register content on asserts/guarantees.
#ifdef CAN_SHOW_REGISTERS_ON_ASSERT

static ucontext_t g_stored_assertion_context;

void initialize_assert_poison() {
  char* page = os::reserve_memory(os::vm_page_size());
  if (page) {
    if (os::commit_memory(page, os::vm_page_size(), false) && os::protect_memory(page, os::vm_page_size(), os::MEM_PROT_NONE)) {
      g_assert_poison = page;
    }
  }
}

static void store_context(const void* context) {
  memcpy(&g_stored_assertion_context, context, sizeof(ucontext_t));
#if defined(__linux) && defined(PPC64)
  // on Linux ppc64, ucontext_t contains pointers into itself which have to be patched up
  //  after copying the context (see comment in sys/ucontext.h):
  *((void**) &g_stored_assertion_context.uc_mcontext.regs) = &(g_stored_assertion_context.uc_mcontext.gp_regs);
#endif
}

bool handle_assert_poison_fault(const void* ucVoid, const void* faulting_address) {
  if (faulting_address == g_assert_poison) {
    // Disarm poison page.
    os::protect_memory((char*)g_assert_poison, os::vm_page_size(), os::MEM_PROT_RWX);
    // Store Context away.
    if (ucVoid) {
      const intx my_tid = os::current_thread_id();
      if (Atomic::cmpxchg(my_tid, &g_asserting_thread, (intx)0) == 0) {
        store_context(ucVoid);
        g_assertion_context = &g_stored_assertion_context;
      }
    }
    return true;
  }
  return false;
}
#endif
