#include "precompiled.hpp"

#include "jvm.h"
#include "code/codeCache.hpp"
#include "compiler/compileBroker.hpp"
#include "compiler/disassembler.hpp"
#include "gc/shared/gcConfig.hpp"
#include "memory/resourceArea.hpp"
#include "runtime/arguments.hpp"
#include "runtime/atomic.hpp"
#include "runtime/frame.inline.hpp"
#include "runtime/init.hpp"
#include "runtime/os.hpp"
#include "runtime/thread.inline.hpp"
#include "runtime/threadSMR.hpp"
#include "runtime/vmThread.hpp"
#include "runtime/vm_operations.hpp"
#include "runtime/vm_version.hpp"
#include "runtime/flags/jvmFlag.hpp"
#include "utilities/debug.hpp"
#include "utilities/decoder.hpp"
#include "utilities/defaultStream.hpp"
#include "utilities/errorReporter.hpp"
#include "utilities/vmError.hpp"
#include "utilities/macros.hpp"

bool VMError::_error_reported = false;

// call this when the VM is dying--it might loosen some asserts
bool VMError::is_error_reported() { return _error_reported; }

// returns an address which is guaranteed to generate a SIGSEGV on read,
// for test purposes, which is not NULL and contains bits in every word
void* VMError::get_segfault_address() {
  return (void*)
    0xABC0000000000ABCULL;
}

// List of environment variables that should be reported in error log file.
const char *env_list[] = {
  // All platforms
  "JAVA_HOME", "JRE_HOME", "JAVA_TOOL_OPTIONS", "_JAVA_OPTIONS", "CLASSPATH",
  "JAVA_COMPILER", "PATH", "USERNAME",

  // Env variables that are defined on Solaris/Linux/BSD
  "LD_LIBRARY_PATH", "LD_PRELOAD", "SHELL", "DISPLAY",
  "HOSTTYPE", "OSTYPE", "ARCH", "MACHTYPE",

  // defined on Linux
  "LD_ASSUME_KERNEL", "_JAVA_SR_SIGNUM",

  // defined on Darwin
  "DYLD_LIBRARY_PATH", "DYLD_FALLBACK_LIBRARY_PATH",
  "DYLD_FRAMEWORK_PATH", "DYLD_FALLBACK_FRAMEWORK_PATH",
  "DYLD_INSERT_LIBRARIES",

  // defined on Windows
  "OS", "PROCESSOR_IDENTIFIER", "_ALT_JAVA_HOME_DIR",

  (const char *)0
};

// A simple parser for -XX:OnError, usage:
//  ptr = OnError;
//  while ((cmd = next_OnError_command(buffer, sizeof(buffer), &ptr) != NULL)
//     ... ...
static char* next_OnError_command(char* buf, int buflen, const char** ptr) {
  if (ptr == NULL || *ptr == NULL) return NULL;

  const char* cmd = *ptr;

  // skip leading blanks or ';'
  while (*cmd == ' ' || *cmd == ';') cmd++;

  if (*cmd == '\0') return NULL;

  const char * cmdend = cmd;
  while (*cmdend != '\0' && *cmdend != ';') cmdend++;

  Arguments::copy_expand_pid(cmd, cmdend - cmd, buf, buflen);

  *ptr = (*cmdend == '\0' ? cmdend : cmdend + 1);
  return buf;
}

static void print_bug_submit_message(outputStream* out, Thread *thread) {
  if (out == NULL) return;
  out->print_raw_cr("# If you would like to submit a bug report, please visit:");
  out->print_raw   ("#   ");
  out->print_raw_cr(Arguments::java_vendor_url_bug());
  // If the crash is in native code, encourage user to submit a bug to the
  // provider of that code.
  if (thread && thread->is_Java_thread() && !thread->is_hidden_from_external_view()) {
    JavaThread* jt = (JavaThread*)thread;
    if (jt->thread_state() == _thread_in_native) {
      out->print_cr("# The crash happened outside the Java Virtual Machine in native code.\n# See problematic frame for where to report the bug.");
    }
  }
  out->print_raw_cr("#");
}

bool VMError::coredump_status;
char VMError::coredump_message[O_BUFLEN];

void VMError::record_coredump_status(const char* message, bool status) {
  coredump_status = status;
  strncpy(coredump_message, message, sizeof(coredump_message));
  coredump_message[sizeof(coredump_message)-1] = 0;
}

// Return a string to describe the error
char* VMError::error_string(char* buf, int buflen) {
  char signame_buf[64];
  const char *signame = os::exception_name(_id, signame_buf, sizeof(signame_buf));

  if (signame) {
    jio_snprintf(buf, buflen, "%s (0x%x) at pc=" PTR_FORMAT ", pid=%d, tid=" UINTX_FORMAT, signame, _id, _pc, os::current_process_id(), os::current_thread_id());
  } else if (_filename != NULL && _lineno > 0) {
    // skip directory names
    char separator = os::file_separator()[0];
    const char *p = strrchr(_filename, separator);
    int n = jio_snprintf(buf, buflen, "Internal Error at %s:%d, pid=%d, tid=" UINTX_FORMAT, p ? p + 1 : _filename, _lineno, os::current_process_id(), os::current_thread_id());
    if (n >= 0 && n < buflen && _message) {
      if (strlen(_detail_msg) > 0) {
        jio_snprintf(buf + n, buflen - n, "%s%s: %s", os::line_separator(), _message, _detail_msg);
      } else {
        jio_snprintf(buf + n, buflen - n, "%sError: %s", os::line_separator(), _message);
      }
    }
  } else {
    jio_snprintf(buf, buflen, "Internal Error (0x%x), pid=%d, tid=" UINTX_FORMAT, _id, os::current_process_id(), os::current_thread_id());
  }

  return buf;
}

void VMError::print_stack_trace(outputStream* st, JavaThread* jt, char* buf, int buflen, bool verbose) {
  if (jt->has_last_Java_frame()) {
    st->print_cr("Java frames: (J=compiled Java code, j=interpreted, Vv=VM code)");
    for (StackFrameStream sfs(jt); !sfs.is_done(); sfs.next()) {
      sfs.current()->print_on_error(st, buf, buflen, verbose);
      st->cr();
    }
  }
}

void VMError::print_native_stack(outputStream* st, frame fr, Thread* t, char* buf, int buf_size) {
  // see if it's a valid frame
  if (fr.pc()) {
    st->print_cr("Native frames: (J=compiled Java code, A=aot compiled Java code, j=interpreted, Vv=VM code, C=native code)");

    int count = 0;
    while (count++ < StackPrintLimit) {
      fr.print_on_error(st, buf, buf_size);
      if (fr.pc()) { // print source file and line, if available
        char buf[128];
        int line_no;
        if (Decoder::get_source_info(fr.pc(), buf, sizeof(buf), &line_no)) {
          st->print("  (%s:%d)", buf, line_no);
        }
      }
      st->cr();
      // Compiled code may use EBP register on x86 so it looks like
      // non-walkable C frame. Use frame.sender() for java frames.
      if (t && t->is_Java_thread()) {
        // Catch very first native frame by using stack address.
        // For JavaThread stack_base and stack_size should be set.
        if (!t->on_local_stack((address)(fr.real_fp() + 1))) {
          break;
        }
        if (fr.is_java_frame() || fr.is_native_frame() || fr.is_runtime_frame()) {
          RegisterMap map((JavaThread*)t, false); // No update
          fr = fr.sender(&map);
        } else {
          // is_first_C_frame() does only simple checks for frame pointer,
          // it will pass if java compiled code has a pointer in EBP.
          if (os::is_first_C_frame(&fr)) break;
          fr = os::get_sender_for_C_frame(&fr);
        }
      } else {
        if (os::is_first_C_frame(&fr)) break;
        fr = os::get_sender_for_C_frame(&fr);
      }
    }

    if (count > StackPrintLimit) {
      st->print_cr("...<more frames>...");
    }

    st->cr();
  }
}

static void print_oom_reasons(outputStream* st) {
  st->print_cr("# Possible reasons:");
  st->print_cr("#   The system is out of physical RAM or swap space");
  if (UseCompressedOops) {
    st->print_cr("#   The process is running with CompressedOops enabled, and the Java Heap may be blocking the growth of the native heap");
  }
  if (LogBytesPerWord == 2) {
    st->print_cr("#   In 32 bit mode, the process size limit was hit");
  }
  st->print_cr("# Possible solutions:");
  st->print_cr("#   Reduce memory load on the system");
  st->print_cr("#   Increase physical memory or swap space");
  st->print_cr("#   Check if swap backing store is full");
  if (LogBytesPerWord == 2) {
    st->print_cr("#   Use 64 bit Java on a 64 bit OS");
  }
  st->print_cr("#   Decrease Java heap size (-Xmx/-Xms)");
  st->print_cr("#   Decrease number of Java threads");
  st->print_cr("#   Decrease Java thread stack sizes (-Xss)");
  st->print_cr("#   Set larger code cache with -XX:ReservedCodeCacheSize=");
  if (UseCompressedOops) {
    switch (Universe::narrow_oop_mode()) {
      case Universe::UnscaledNarrowOop:
        st->print_cr("#   JVM is running with Unscaled Compressed Oops mode in which the Java heap is");
        st->print_cr("#     placed in the first 4GB address space. The Java Heap base address is the");
        st->print_cr("#     maximum limit for the native heap growth. Please use -XX:HeapBaseMinAddress");
        st->print_cr("#     to set the Java Heap base and to place the Java Heap above 4GB virtual address.");
        break;
      case Universe::ZeroBasedNarrowOop:
        st->print_cr("#   JVM is running with Zero Based Compressed Oops mode in which the Java heap is");
        st->print_cr("#     placed in the first 32GB address space. The Java Heap base address is the");
        st->print_cr("#     maximum limit for the native heap growth. Please use -XX:HeapBaseMinAddress");
        st->print_cr("#     to set the Java Heap base and to place the Java Heap above 32GB virtual address.");
        break;
      default:
        break;
    }
  }
  st->print_cr("# This output file may be truncated or incomplete.");
}

static void report_vm_version(outputStream* st, char* buf, int buflen) {
   // VM version
   st->print_cr("#");
   JDK_Version::current().to_string(buf, buflen);
   const char* runtime_name = JDK_Version::runtime_name() != NULL ? JDK_Version::runtime_name() : "";
   const char* runtime_version = JDK_Version::runtime_version() != NULL ? JDK_Version::runtime_version() : "";
   const char* jdk_debug_level = Abstract_VM_Version::printable_jdk_debug_level() != NULL ? Abstract_VM_Version::printable_jdk_debug_level() : "";

   st->print_cr("# JRE version: %s (%s) (%sbuild %s)", runtime_name, buf, jdk_debug_level, runtime_version);

   // This is the long version with some default settings added
   st->print_cr("# Java VM: %s (%s%s, %s%s%s%s, %s, %s)",
                 Abstract_VM_Version::vm_name(),
                 jdk_debug_level,
                 Abstract_VM_Version::vm_release(),
                 Abstract_VM_Version::vm_info_string(),
                 EnableJVMCI ? ", jvmci" : "",
                 UseJVMCICompiler ? ", jvmci compiler" : "",
                 UseCompressedOops ? ", compressed oops" : "",
                 GCConfig::hs_err_name(),
                 Abstract_VM_Version::vm_platform_string()
               );
}

// This is the main function to report a fatal error. Only one thread can
// call this function, so we don't need to worry about MT-safety. But it's
// possible that the error handler itself may crash or die on an internal
// error, for example, when the stack/heap is badly damaged. We must be
// able to handle recursive errors that happen inside error handler.
//
// Error reporting is done in several steps. If a crash or internal error
// occurred when reporting an error, the nested signal/exception handler
// can skip steps that are already (or partially) done. Error reporting will
// continue from the next step. This allows us to retrieve and print
// information that may be unsafe to get after a fatal error. If it happens,
// you may find nested report_and_die() frames when you look at the stack
// in a debugger.
//
// In general, a hang in error handler is much worse than a crash or internal
// error, as it's harder to recover from a hang. Deadlock can happen if we
// try to grab a lock that is already owned by current thread, or if the
// owner is blocked forever (e.g. in os::infinite_sleep()). If possible, the
// error handler and all the functions it called should avoid grabbing any
// lock. An important thing to notice is that memory allocation needs a lock.
//
// We should avoid using large stack allocated buffers. Many errors happen
// when stack space is already low. Making things even worse is that there
// could be nested report_and_die() calls on stack (see above). Only one
// thread can report error, so large buffers are statically allocated in data
// segment.

int          VMError::_current_step;
const char*  VMError::_current_step_info;

volatile jlong VMError::_reporting_start_time = -1;
volatile bool VMError::_reporting_did_timeout = false;
volatile jlong VMError::_step_start_time = -1;
volatile bool VMError::_step_did_timeout = false;

// Helper, return current timestamp for timeout handling.
jlong VMError::get_current_timestamp() {
  return os::javaTimeNanos();
}
// Factor to translate the timestamp to seconds.
#define TIMESTAMP_TO_SECONDS_FACTOR (1000 * 1000 * 1000)

void VMError::record_reporting_start_time() {
  const jlong now = get_current_timestamp();
  Atomic::store(now, &_reporting_start_time);
}

jlong VMError::get_reporting_start_time() {
  return Atomic::load(&_reporting_start_time);
}

void VMError::record_step_start_time() {
  const jlong now = get_current_timestamp();
  Atomic::store(now, &_step_start_time);
}

jlong VMError::get_step_start_time() {
  return Atomic::load(&_step_start_time);
}

void VMError::report(outputStream* st, bool _verbose) {
# define BEGIN if (_current_step == 0) { _current_step = __LINE__;
# define STEP(s) } if (_current_step < __LINE__) { _current_step = __LINE__; _current_step_info = s; record_step_start_time(); _step_did_timeout = false;
# define END }

  // don't allocate large buffer on stack
  static char buf[O_BUFLEN];

  BEGIN

  STEP("printing fatal error message")

    st->print_cr("#");
    if (should_report_bug(_id)) {
      st->print_cr("# A fatal error has been detected by the Java Runtime Environment:");
    } else {
      st->print_cr("# There is insufficient memory for the Java Runtime Environment to continue.");
    }

  STEP("printing type of error")

     switch (static_cast<unsigned int>(_id)) {
       case OOM_MALLOC_ERROR:
       case OOM_MMAP_ERROR:
         if (_size) {
           st->print("# Native memory allocation ");
           st->print((_id == (int)OOM_MALLOC_ERROR) ? "(malloc) failed to allocate " : "(mmap) failed to map ");
           jio_snprintf(buf, sizeof(buf), SIZE_FORMAT, _size);
           st->print("%s", buf);
           st->print(" bytes");
           if (strlen(_detail_msg) > 0) {
             st->print(" for ");
             st->print("%s", _detail_msg);
           }
           st->cr();
         } else {
           if (strlen(_detail_msg) > 0) {
             st->print("# ");
             st->print_cr("%s", _detail_msg);
           }
         }
         // In error file give some solutions
         if (_verbose) {
           print_oom_reasons(st);
         } else {
           return;  // that's enough for the screen
         }
         break;
       case INTERNAL_ERROR:
       default:
         break;
     }

  STEP("printing exception/signal name")

     st->print_cr("#");
     st->print("#  ");
     // Is it an OS exception/signal?
     if (os::exception_name(_id, buf, sizeof(buf))) {
       st->print("%s", buf);
       st->print(" (0x%x)", _id);                // signal number
       st->print(" at pc=" PTR_FORMAT, p2i(_pc));
     } else {
       if (should_report_bug(_id)) {
         st->print("Internal Error");
       } else {
         st->print("Out of Memory Error");
       }
       if (_filename != NULL && _lineno > 0) {
         // In product mode chop off pathname?
         char separator = os::file_separator()[0];
         const char *p = strrchr(_filename, separator);
         const char *file = p ? p + 1 : _filename;
         st->print(" (%s:%d)", file, _lineno);
       } else {
         st->print(" (0x%x)", _id);
       }
     }

  STEP("printing current thread and pid")

     // process id, thread id
     st->print(", pid=%d", os::current_process_id());
     st->print(", tid=" UINTX_FORMAT, os::current_thread_id());
     st->cr();

  STEP("printing error message")

     if (should_report_bug(_id)) {  // already printed the message.
       // error message
       if (strlen(_detail_msg) > 0) {
         st->print_cr("#  %s: %s", _message ? _message : "Error", _detail_msg);
       } else if (_message) {
         st->print_cr("#  Error: %s", _message);
       }
     }

  STEP("printing Java version string")

     report_vm_version(st, buf, sizeof(buf));

  STEP("printing problematic frame")

     // Print current frame if we have a context (i.e. it's a crash)
     if (_context) {
       st->print_cr("# Problematic frame:");
       st->print("# ");
       frame fr = os::fetch_frame_from_context(_context);
       fr.print_on_error(st, buf, sizeof(buf));
       st->cr();
       st->print_cr("#");
     }

  STEP("printing bug submit message")

     if (should_report_bug(_id) && _verbose) {
       print_bug_submit_message(st, _thread);
     }

  STEP("printing summary")

     if (_verbose) {
       st->cr();
       st->print_cr("---------------  S U M M A R Y ------------");
       st->cr();
     }

  STEP("printing VM option summary")

     if (_verbose) {
       // VM options
       Arguments::print_summary_on(st);
       st->cr();
     }

  STEP("printing summary machine and OS info")

     if (_verbose) {
       os::print_summary_info(st, buf, sizeof(buf));
     }

  STEP("printing date and time")

     if (_verbose) {
       os::print_date_and_time(st, buf, sizeof(buf));
     }

  STEP("printing thread")

     if (_verbose) {
       st->cr();
       st->print_cr("---------------  T H R E A D  ---------------");
       st->cr();
     }

  STEP("printing current thread")

     // current thread
     if (_verbose) {
       if (_thread) {
         st->print("Current thread (" PTR_FORMAT "):  ", p2i(_thread));
         _thread->print_on_error(st, buf, sizeof(buf));
         st->cr();
       } else {
         st->print_cr("Current thread is native thread");
       }
       st->cr();
     }

  STEP("printing current compile task")

     if (_verbose && _thread && _thread->is_Compiler_thread()) {
        CompilerThread* t = (CompilerThread*)_thread;
        if (t->task()) {
           st->cr();
           st->print_cr("Current CompileTask:");
           t->task()->print_line_on_error(st, buf, sizeof(buf));
           st->cr();
        }
     }

  STEP("printing stack bounds")

     if (_verbose) {
       st->print("Stack: ");

       address stack_top;
       size_t stack_size;

       if (_thread) {
          stack_top = _thread->stack_base();
          stack_size = _thread->stack_size();
       } else {
          stack_top = os::current_stack_base();
          stack_size = os::current_stack_size();
       }

       address stack_bottom = stack_top - stack_size;
       st->print("[" PTR_FORMAT "," PTR_FORMAT "]", p2i(stack_bottom), p2i(stack_top));

       frame fr = _context ? os::fetch_frame_from_context(_context)
                           : os::current_frame();

       if (fr.sp()) {
         st->print(",  sp=" PTR_FORMAT, p2i(fr.sp()));
         size_t free_stack_size = pointer_delta(fr.sp(), stack_bottom, 1024);
         st->print(",  free space=" SIZE_FORMAT "k", free_stack_size);
       }

       st->cr();
     }

  STEP("printing native stack")

   if (_verbose) {
     if (os::platform_print_native_stack(st, _context, buf, sizeof(buf))) {
       // We have printed the native stack in platform-specific code
       // Windows/x64 needs special handling.
     } else {
       frame fr = _context ? os::fetch_frame_from_context(_context)
                           : os::current_frame();

       print_native_stack(st, fr, _thread, buf, sizeof(buf));
     }
   }

  STEP("printing Java stack")

     if (_verbose && _thread && _thread->is_Java_thread()) {
       print_stack_trace(st, (JavaThread*)_thread, buf, sizeof(buf));
     }

  STEP("printing target Java thread stack")

     // printing Java thread stack trace if it is involved in GC crash
     if (_verbose && _thread && (_thread->is_Named_thread())) {
       JavaThread*  jt = ((NamedThread *)_thread)->processed_thread();
       if (jt != NULL) {
         st->print_cr("JavaThread " PTR_FORMAT " (nid = %d) was being processed", p2i(jt), jt->osthread()->thread_id());
         print_stack_trace(st, jt, buf, sizeof(buf), true);
       }
     }

  STEP("printing siginfo")

     // signal no, signal code, address that caused the fault
     if (_verbose && _siginfo) {
       st->cr();
       os::print_siginfo(st, _siginfo);
       st->cr();
     }

  STEP("CDS archive access warning")

     // Print an explicit hint if we crashed on access to the CDS archive.
     if (_verbose && _siginfo) {
       check_failing_cds_access(st, _siginfo);
       st->cr();
     }

  STEP("printing register info")

     // decode register contents if possible
     if (_verbose && _context && Universe::is_fully_initialized()) {
       os::print_register_info(st, _context);
       st->cr();
     }

  STEP("printing registers, top of stack, instructions near pc")

     // registers, top of stack, instructions near pc
     if (_verbose && _context) {
       os::print_context(st, _context);
       st->cr();
     }

  STEP("inspecting top of stack")

     // decode stack contents if possible
     if (_verbose && _context && Universe::is_fully_initialized()) {
       frame fr = os::fetch_frame_from_context(_context);
       const int slots = 8;
       const intptr_t *start = fr.sp();
       const intptr_t *end = start + slots;
       if (is_aligned(start, sizeof(intptr_t)) && os::is_readable_range(start, end)) {
         st->print_cr("Stack slot to memory mapping:");
         for (int i = 0; i < slots; ++i) {
           st->print("stack at sp + %d slots: ", i);
           os::print_location(st, *(start + i));
         }
       }
       st->cr();
     }

  STEP("printing code blob if possible")

     if (_verbose && _context) {
       CodeBlob* cb = CodeCache::find_blob(_pc);
       if (cb != NULL) {
         StubCodeDesc* desc = StubCodeDesc::desc_for(_pc);
         if (desc != NULL) {
           desc->print_on(st);
           Disassembler::decode(desc->begin(), desc->end(), st);
         } else if (_thread != NULL) {
           // Disassembling nmethod will incur resource memory allocation,
           // only do so when thread is valid.
           ResourceMark rm(_thread);
           Disassembler::decode(cb, st);
           st->cr();
         }
       }
     }

  STEP("printing VM operation")

     if (_verbose && _thread && _thread->is_VM_thread()) {
        VMThread* t = (VMThread*)_thread;
        VM_Operation* op = t->vm_operation();
        if (op) {
          op->print_on_error(st);
          st->cr();
          st->cr();
        }
     }

  STEP("printing process")

     if (_verbose) {
       st->cr();
       st->print_cr("---------------  P R O C E S S  ---------------");
       st->cr();
     }

  STEP("printing all threads")

     // all threads
     if (_verbose && _thread) {
       Threads::print_on_error(st, _thread, buf, sizeof(buf));
       st->cr();
     }

  STEP("printing VM state")

     if (_verbose) {
       // Safepoint state
       st->print("VM state:");

       if (SafepointSynchronize::is_synchronizing()) st->print("synchronizing");
       else if (SafepointSynchronize::is_at_safepoint()) st->print("at safepoint");
       else st->print("not at safepoint");

       // Also see if error occurred during initialization or shutdown
       if (!Universe::is_fully_initialized()) {
         st->print(" (not fully initialized)");
       } else if (VM_Exit::vm_exited()) {
         st->print(" (shutting down)");
       } else {
         st->print(" (normal execution)");
       }
       st->cr();
       st->cr();
     }

  STEP("printing owned locks on error")

     // mutexes/monitors that currently have an owner
     if (_verbose) {
       print_owned_locks_on_error(st);
       st->cr();
     }

  STEP("printing number of OutOfMemoryError and StackOverflow exceptions")

     if (_verbose && Exceptions::has_exception_counts()) {
       st->print_cr("OutOfMemory and StackOverflow Exception counts:");
       Exceptions::print_exception_counts_on_error(st);
       st->cr();
     }

  STEP("printing compressed oops mode")

     if (_verbose && UseCompressedOops) {
       Universe::print_compressed_oops_mode(st);
       if (UseCompressedClassPointers) {
         Metaspace::print_compressed_class_space(st);
       }
       st->cr();
     }

  STEP("printing heap information")

     if (_verbose && Universe::is_fully_initialized()) {
       Universe::heap()->print_on_error(st);
       st->cr();
       st->print_cr("Polling page: " INTPTR_FORMAT, p2i(os::get_polling_page()));
       st->cr();
     }

  STEP("printing code cache information")

     if (_verbose && Universe::is_fully_initialized()) {
       // print code cache information before vm abort
       CodeCache::print_summary(st);
       st->cr();
     }

  STEP("printing dynamic libraries")

     if (_verbose) {
       // dynamic libraries, or memory map
       os::print_dll_info(st);
       st->cr();
     }

  STEP("printing native decoder state")

     if (_verbose) {
       Decoder::print_state_on(st);
       st->cr();
     }

  STEP("printing VM options")

     if (_verbose) {
       // VM options
       Arguments::print_on(st);
       st->cr();
     }

  STEP("printing flags")

    if (_verbose) {
      JVMFlag::printFlags(
        st,
        true, // with comments
        false, // no ranges
        true); // skip defaults
      st->cr();
    }

  STEP("printing all environment variables")

     if (_verbose) {
       os::print_environment_variables(st, env_list);
       st->cr();
     }

  STEP("printing signal handlers")

     if (_verbose) {
       os::print_signal_handlers(st, buf, sizeof(buf));
       st->cr();
     }

  STEP("printing system")

     if (_verbose) {
       st->cr();
       st->print_cr("---------------  S Y S T E M  ---------------");
       st->cr();
     }

  STEP("printing OS information")

     if (_verbose) {
       os::print_os_info(st);
       st->cr();
     }

  STEP("printing CPU info")
     if (_verbose) {
       os::print_cpu_info(st, buf, sizeof(buf));
       st->cr();
     }

  STEP("printing memory info")

     if (_verbose) {
       os::print_memory_info(st);
       st->cr();
     }

  STEP("printing internal vm info")

     if (_verbose) {
       st->print_cr("vm_info: %s", Abstract_VM_Version::internal_vm_info_string());
       st->cr();
     }

  // print a defined marker to show that error handling finished correctly.
  STEP("printing end marker")

     if (_verbose) {
       st->print_cr("END.");
     }

  END

# undef BEGIN
# undef STEP
# undef END
}

// Report for the vm_info_cmd. This prints out the information above omitting
// crash and thread specific information.  If output is added above, it should be added
// here also, if it is safe to call during a running process.
void VMError::print_vm_info(outputStream* st) {
  char buf[O_BUFLEN];
  report_vm_version(st, buf, sizeof(buf));

  // STEP("printing summary")

  st->cr();
  st->print_cr("---------------  S U M M A R Y ------------");
  st->cr();

  // STEP("printing VM option summary")

  // VM options
  Arguments::print_summary_on(st);
  st->cr();

  // STEP("printing summary machine and OS info")

  os::print_summary_info(st, buf, sizeof(buf));

  // STEP("printing date and time")

  os::print_date_and_time(st, buf, sizeof(buf));

  // Skip: STEP("printing thread")

  // STEP("printing process")

  st->cr();
  st->print_cr("---------------  P R O C E S S  ---------------");
  st->cr();

  // STEP("printing number of OutOfMemoryError and StackOverflow exceptions")

  if (Exceptions::has_exception_counts()) {
    st->print_cr("OutOfMemory and StackOverflow Exception counts:");
    Exceptions::print_exception_counts_on_error(st);
    st->cr();
  }

  // STEP("printing compressed oops mode")

  if (UseCompressedOops) {
    Universe::print_compressed_oops_mode(st);
    if (UseCompressedClassPointers) {
      Metaspace::print_compressed_class_space(st);
    }
    st->cr();
  }

  // STEP("printing heap information")

  if (Universe::is_fully_initialized()) {
    MutexLocker hl(Heap_lock);
    Universe::heap()->print_on_error(st);
    st->cr();
    st->print_cr("Polling page: " INTPTR_FORMAT, p2i(os::get_polling_page()));
    st->cr();
  }

  // STEP("printing code cache information")

  if (Universe::is_fully_initialized()) {
    // print code cache information before vm abort
    CodeCache::print_summary(st);
    st->cr();
  }

  // STEP("printing dynamic libraries")

  // dynamic libraries, or memory map
  os::print_dll_info(st);
  st->cr();

  // STEP("printing VM options")

  // VM options
  Arguments::print_on(st);
  st->cr();

  // STEP("printing all environment variables")

  os::print_environment_variables(st, env_list);
  st->cr();

  // STEP("printing signal handlers")

  os::print_signal_handlers(st, buf, sizeof(buf));
  st->cr();

  // STEP("printing system")

  st->cr();
  st->print_cr("---------------  S Y S T E M  ---------------");
  st->cr();

  // STEP("printing OS information")

  os::print_os_info(st);
  st->cr();

  // STEP("printing CPU info")

  os::print_cpu_info(st, buf, sizeof(buf));
  st->cr();

  // STEP("printing memory info")

  os::print_memory_info(st);
  st->cr();

  // STEP("printing internal vm info")

  st->print_cr("vm_info: %s", Abstract_VM_Version::internal_vm_info_string());
  st->cr();

  // print a defined marker to show that error handling finished correctly.
  // STEP("printing end marker")

  st->print_cr("END.");
}

volatile intptr_t VMError::first_error_tid = -1;

// An error could happen before tty is initialized or after it has been
// destroyed.
// Please note: to prevent large stack allocations, the log- and
// output-stream use a global scratch buffer for format printing.
// (see VmError::report_and_die(). Access to those streams is synchronized
// in  VmError::report_and_die() - there is only one reporting thread at
// any given time.
fdStream VMError::out(defaultStream::output_fd());
fdStream VMError::log; // error log used by VMError::report_and_die()

/** Expand a pattern into a buffer starting at pos and open a file using constructed path */
static int expand_and_open(const char* pattern, char* buf, size_t buflen, size_t pos) {
  int fd = -1;
  if (Arguments::copy_expand_pid(pattern, strlen(pattern), &buf[pos], buflen - pos)) {
    // the O_EXCL flag will cause the open to fail if the file exists
    fd = open(buf, O_RDWR | O_CREAT | O_EXCL, 0666);
  }
  return fd;
}

/**
 * Construct file name for a log file and return it's file descriptor.
 * Name and location depends on pattern, default_pattern params and access
 * permissions.
 */
static int prepare_log_file(const char* pattern, const char* default_pattern, char* buf, size_t buflen) {
  int fd = -1;

  // If possible, use specified pattern to construct log file name
  if (pattern != NULL) {
    fd = expand_and_open(pattern, buf, buflen, 0);
  }

  // Either user didn't specify, or the user's location failed,
  // so use the default name in the current directory
  if (fd == -1) {
    const char* cwd = os::get_current_directory(buf, buflen);
    if (cwd != NULL) {
      size_t pos = strlen(cwd);
      int fsep_len = jio_snprintf(&buf[pos], buflen-pos, "%s", os::file_separator());
      pos += fsep_len;
      if (fsep_len > 0) {
        fd = expand_and_open(default_pattern, buf, buflen, pos);
      }
    }
  }

   // try temp directory if it exists.
   if (fd == -1) {
     const char* tmpdir = os::get_temp_directory();
     if (tmpdir != NULL && strlen(tmpdir) > 0) {
       int pos = jio_snprintf(buf, buflen, "%s%s", tmpdir, os::file_separator());
       if (pos > 0) {
         fd = expand_and_open(default_pattern, buf, buflen, pos);
       }
     }
   }

  return fd;
}

int         VMError::_id;
const char* VMError::_message;
char        VMError::_detail_msg[1024];
Thread*     VMError::_thread;
address     VMError::_pc;
void*       VMError::_siginfo;
void*       VMError::_context;
const char* VMError::_filename;
int         VMError::_lineno;
size_t      VMError::_size;

void VMError::report_and_die(Thread* thread, unsigned int sig, address pc, void* siginfo, void* context, const char* detail_fmt, ...) {
  va_list detail_args;
  va_start(detail_args, detail_fmt);
  report_and_die(sig, NULL, detail_fmt, detail_args, thread, pc, siginfo, context, NULL, 0, 0);
  va_end(detail_args);
}

void VMError::report_and_die(Thread* thread, unsigned int sig, address pc, void* siginfo, void* context) {
  report_and_die(thread, sig, pc, siginfo, context, "%s", "");
}

void VMError::report_and_die(const char* message, const char* detail_fmt, ...) {
  va_list detail_args;
  va_start(detail_args, detail_fmt);
  report_and_die(INTERNAL_ERROR, message, detail_fmt, detail_args, NULL, NULL, NULL, NULL, NULL, 0, 0);
  va_end(detail_args);
}

void VMError::report_and_die(const char* message) {
  report_and_die(message, "%s", "");
}

void VMError::report_and_die(Thread* thread, void* context, const char* filename, int lineno, const char* message, const char* detail_fmt, va_list detail_args) {
  report_and_die(INTERNAL_ERROR, message, detail_fmt, detail_args, thread, NULL, NULL, context, filename, lineno, 0);
}

void VMError::report_and_die(Thread* thread, const char* filename, int lineno, size_t size, VMErrorType vm_err_type, const char* detail_fmt, va_list detail_args) {
  report_and_die(vm_err_type, NULL, detail_fmt, detail_args, thread, NULL, NULL, NULL, filename, lineno, size);
}

void VMError::report_and_die(int id, const char* message, const char* detail_fmt, va_list detail_args, Thread* thread, address pc, void* siginfo, void* context, const char* filename, int lineno, size_t size) {
  // Don't allocate large buffer on stack
  static char buffer[O_BUFLEN];
  out.set_scratch_buffer(buffer, sizeof(buffer));
  log.set_scratch_buffer(buffer, sizeof(buffer));

  // How many errors occurred in error handler when reporting first_error.
  static int recursive_error_count;

  // We will first print a brief message to standard out (verbose = false),
  // then save detailed information in log file (verbose = true).
  static bool out_done = false;         // done printing to standard out
  static bool log_done = false;         // done saving error log
  static bool transmit_report_done = false; // done error reporting

  if (SuppressFatalErrorMessage) {
      os::abort(false);
  }
  intptr_t mytid = os::current_thread_id();
  if (first_error_tid == -1 && Atomic::cmpxchg(mytid, &first_error_tid, (intptr_t)-1) == -1) {
    // Initialize time stamps to use the same base.
    out.time_stamp().update_to(1);
    log.time_stamp().update_to(1);

    _id = id;
    _message = message;
    _thread = thread;
    _pc = pc;
    _siginfo = siginfo;
    _context = context;
    _filename = filename;
    _lineno = lineno;
    _size = size;
    jio_vsnprintf(_detail_msg, sizeof(_detail_msg), detail_fmt, detail_args);

    // first time
    _error_reported = true;

    reporting_started();
    record_reporting_start_time();

    // reset signal handlers or exception filter; make sure recursive crashes
    // are handled properly.
    reset_signal_handlers();
  } else {
    // If UseOsErrorReporting we call this for each level of the call stack
    // while searching for the exception handler.  Only the first level needs
    // to be reported.
    if (UseOSErrorReporting && log_done) return;

    // This is not the first error, see if it happened in a different thread
    // or in the same thread during error reporting.
    if (first_error_tid != mytid) {
      char msgbuf[64];
      jio_snprintf(msgbuf, sizeof(msgbuf), "[thread " INTX_FORMAT " also had an error]", mytid);
      out.print_raw_cr(msgbuf);

      // error reporting is not MT-safe, block current thread
      os::infinite_sleep();
    } else {
      if (recursive_error_count++ > 30) {
        out.print_raw_cr("[Too many errors, abort]");
        os::die();
      }

      outputStream* const st = log.is_open() ? &log : &out;
      st->cr();

      // Timeout handling.
      if (_step_did_timeout) {
        // The current step had a timeout. Lets continue reporting with the next step.
        st->print_raw("[timeout occurred during error reporting in step \"");
        st->print_raw(_current_step_info);
        st->print_cr("\"] after " INT64_FORMAT " s.", (int64_t) ((get_current_timestamp() - _step_start_time) / TIMESTAMP_TO_SECONDS_FACTOR));
      } else if (_reporting_did_timeout) {
        // We hit ErrorLogTimeout. Reporting will stop altogether. Let's wrap things
        // up, the process is about to be stopped by the WatcherThread.
        st->print_cr("------ Timeout during error reporting after " INT64_FORMAT " s. ------",
                     (int64_t)
                     ((get_current_timestamp() - _reporting_start_time) / TIMESTAMP_TO_SECONDS_FACTOR));
        st->flush();
        // Watcherthread is about to call os::die. Lets just wait.
        os::infinite_sleep();
      } else {
        // Crash or assert during error reporting. Lets continue reporting with the next step.
        stringStream ss(buffer, sizeof(buffer));
        // Note: this string does get parsed by a number of jtreg tests,
        // see hotspot/jtreg/runtime/ErrorHandling.
        ss.print("[error occurred during error reporting (%s), id 0x%x", _current_step_info, id);
        char signal_name[64];
        if (os::exception_name(id, signal_name, sizeof(signal_name))) {
          ss.print(", %s (0x%x) at pc=" PTR_FORMAT, signal_name, id, p2i(pc));
        } else {
          if (should_report_bug(id)) {
            ss.print(", Internal Error (%s:%d)", filename == NULL ? "??" : filename, lineno);
          } else {
            ss.print(", Out of Memory Error (%s:%d)", filename == NULL ? "??" : filename, lineno);
          }
        }
        ss.print("]");
        st->print_raw_cr(buffer);
        st->cr();
      }
    }
  }

  // print to screen
  if (!out_done) {
    report(&out, false);

    out_done = true;

    _current_step = 0;
    _current_step_info = "";
  }

  // print to error log file
  if (!log_done) {
    // see if log file is already open
    if (!log.is_open()) {
      // open log file
      int fd = prepare_log_file(NULL, "hs_err_pid%p.log", buffer, sizeof(buffer));
      if (fd != -1) {
        out.print_raw("# An error report file with more information is saved as:\n# ");
        out.print_raw_cr(buffer);

        log.set_fd(fd);
      } else {
        out.print_raw_cr("# Can not save log file, dump to screen..");
        log.set_fd(defaultStream::output_fd());
        /* Error reporting currently needs dumpfile.
         * Maybe implement direct streaming in the future.*/
        transmit_report_done = true;
      }
    }

    report(&log, true);
    log_done = true;
    _current_step = 0;
    _current_step_info = "";

    // Run error reporting to determine whether or not to report the crash.
    if (!transmit_report_done && should_report_bug(_id)) {
      transmit_report_done = true;
      const int fd2 = ::dup(log.fd());
      if (fd2 != -1) {
        FILE* const hs_err = ::fdopen(fd2, "r");
        if (NULL != hs_err) {
          ErrorReporter er;
          er.call(hs_err, buffer, O_BUFLEN);
          ::fclose(hs_err);
        }
      }
    }

    if (log.fd() != defaultStream::output_fd()) {
      close(log.fd());
    }

    log.set_fd(-1);
  }

  static bool skip_bug_url = !should_report_bug(_id);
  if (!skip_bug_url) {
    skip_bug_url = true;

    out.print_raw_cr("#");
    print_bug_submit_message(&out, _thread);
  }

  static bool skip_OnError = false;
  if (!skip_OnError && OnError && OnError[0]) {
    skip_OnError = true;

    // Flush output and finish logs before running OnError commands.
    ostream_abort();

    out.print_raw_cr("#");
    out.print_raw   ("# -XX:OnError=\"");
    out.print_raw   (OnError);
    out.print_raw_cr("\"");

    char* cmd;
    const char* ptr = OnError;
    while ((cmd = next_OnError_command(buffer, sizeof(buffer), &ptr)) != NULL) {
      out.print_raw   ("#   Executing ");
#if defined(LINUX) || defined(_ALLBSD_SOURCE)
      out.print_raw   ("/bin/sh -c ");
#endif
      out.print_raw   ("\"");
      out.print_raw   (cmd);
      out.print_raw_cr("\" ...");

      if (os::fork_and_exec(cmd) < 0) {
        out.print_cr("os::fork_and_exec failed: %s (%s=%d)", os::strerror(errno), os::errno_name(errno), errno);
      }
    }

    // done with OnError
    OnError = NULL;
  }

  if (!UseOSErrorReporting) {
    // os::abort() will call abort hooks, try it first.
    static bool skip_os_abort = false;
    if (!skip_os_abort) {
      skip_os_abort = true;
      bool dump_core = should_report_bug(_id);
      os::abort(false, _siginfo, _context);
    }

    // if os::abort() doesn't abort, try os::die();
    os::die();
  }
}

/*
 * OnOutOfMemoryError scripts/commands executed while VM is a safepoint - this
 * ensures utilities such as jmap can observe the process is a consistent state.
 */
class VM_ReportJavaOutOfMemory : public VM_Operation {
 private:
  const char* _message;
 public:
  VM_ReportJavaOutOfMemory(const char* message) { _message = message; }
  VMOp_Type type()                        const { return VMOp_ReportJavaOutOfMemory; }
  void doit();
};

void VM_ReportJavaOutOfMemory::doit() {
  // Don't allocate large buffer on stack
  static char buffer[O_BUFLEN];

  tty->print_cr("#");
  tty->print_cr("# java.lang.OutOfMemoryError: %s", _message);
  tty->print_cr("# -XX:OnOutOfMemoryError=\"%s\"", OnOutOfMemoryError);

  // make heap parsability
  Universe::heap()->ensure_parsability(false);  // no need to retire TLABs

  char* cmd;
  const char* ptr = OnOutOfMemoryError;
  while ((cmd = next_OnError_command(buffer, sizeof(buffer), &ptr)) != NULL) {
    tty->print("#   Executing ");
#if defined(LINUX)
    tty->print  ("/bin/sh -c ");
#endif
    tty->print_cr("\"%s\"...", cmd);

    if (os::fork_and_exec(cmd, true) < 0) {
      tty->print_cr("os::fork_and_exec failed: %s (%s=%d)", os::strerror(errno), os::errno_name(errno), errno);
    }
  }
}

void VMError::report_java_out_of_memory(const char* message) {
  if (OnOutOfMemoryError && OnOutOfMemoryError[0]) {
    MutexLocker ml(Heap_lock);
    VM_ReportJavaOutOfMemory op(message);
    VMThread::execute(&op);
  }
}

// Timeout handling: check if a timeout happened (either a single step did
// timeout or the whole of error reporting hit ErrorLogTimeout). Interrupt
// the reporting thread if that is the case.
bool VMError::check_timeout() {
  if (ErrorLogTimeout == 0) {
    return false;
  }

  // Do not check for timeouts if we still have a message box to show to the
  // user or if there are OnError handlers to be run.
  if ((OnError != NULL && OnError[0] != '\0') || Arguments::abort_hook() != NULL) {
    return false;
  }

  const jlong reporting_start_time_l = get_reporting_start_time();
  const jlong now = get_current_timestamp();
  // Timestamp is stored in nanos.
  if (reporting_start_time_l > 0) {
    const jlong end = reporting_start_time_l + (jlong)ErrorLogTimeout * TIMESTAMP_TO_SECONDS_FACTOR;
    if (end <= now) {
      _reporting_did_timeout = true;
      interrupt_reporting_thread();
      return true; // global timeout
    }
  }

  const jlong step_start_time_l = get_step_start_time();
  if (step_start_time_l > 0) {
    // A step times out after a quarter of the total timeout. Steps are mostly fast unless they
    // hang for some reason, so this simple rule allows for three hanging step and still
    // hopefully leaves time enough for the rest of the steps to finish.
    const jlong end = step_start_time_l + (jlong)ErrorLogTimeout * TIMESTAMP_TO_SECONDS_FACTOR / 4;
    if (end <= now) {
      _step_did_timeout = true;
      interrupt_reporting_thread();
      return false; // (Not a global timeout)
    }
  }

  return false;
}
