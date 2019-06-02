#include "precompiled.hpp"

#include "jvm.h"
#include "aot/aotLoader.hpp"
#include "classfile/classLoader.hpp"
#include "classfile/stringTable.hpp"
#include "classfile/systemDictionary.hpp"
#include "code/codeCache.hpp"
#include "compiler/compileBroker.hpp"
#include "compiler/compilerOracle.hpp"
#include "interpreter/bytecodeHistogram.hpp"
#include "jvmci/jvmciCompiler.hpp"
#include "jvmci/jvmciRuntime.hpp"
#include "logging/log.hpp"
#include "logging/logStream.hpp"
#include "memory/oopFactory.hpp"
#include "memory/resourceArea.hpp"
#include "memory/universe.hpp"
#include "oops/constantPool.hpp"
#include "oops/generateOopMap.hpp"
#include "oops/instanceKlass.hpp"
#include "oops/instanceOop.hpp"
#include "oops/method.hpp"
#include "oops/objArrayOop.hpp"
#include "oops/oop.inline.hpp"
#include "oops/symbol.hpp"
#include "runtime/arguments.hpp"
#include "runtime/biasedLocking.hpp"
#include "runtime/compilationPolicy.hpp"
#include "runtime/deoptimization.hpp"
#include "runtime/flags/flagSetting.hpp"
#include "runtime/init.hpp"
#include "runtime/interfaceSupport.inline.hpp"
#include "runtime/java.hpp"
#include "runtime/memprofiler.hpp"
#include "runtime/sharedRuntime.hpp"
#include "runtime/statSampler.hpp"
#include "runtime/sweeper.hpp"
#include "runtime/task.hpp"
#include "runtime/thread.inline.hpp"
#include "runtime/timer.hpp"
#include "runtime/vm_operations.hpp"
#include "services/memTracker.hpp"
#include "utilities/globalDefinitions.hpp"
#include "utilities/histogram.hpp"
#include "utilities/macros.hpp"
#include "utilities/vmError.hpp"
#include "c1/c1_Compiler.hpp"
#include "c1/c1_Runtime1.hpp"

GrowableArray<Method*>* collected_profiled_methods;

int compare_methods(Method** a, Method** b) {
  // %%% there can be 32-bit overflow here
  return ((*b)->invocation_count() + (*b)->compiled_invocation_count())
       - ((*a)->invocation_count() + (*a)->compiled_invocation_count());
}

void collect_profiled_methods(Method* m) {
  Thread* thread = Thread::current();
  methodHandle mh(thread, m);
  if ((m->method_data() != NULL) && CompilerOracle::should_print(mh)) {
    collected_profiled_methods->push(m);
  }
}

void print_method_profiling_data() {
  if (ProfileInterpreter || C1UpdateMethodData && CompilerOracle::should_print_methods()) {
    ResourceMark rm;
    HandleMark hm;
    collected_profiled_methods = new GrowableArray<Method*>(1024);
    SystemDictionary::methods_do(collect_profiled_methods);
    collected_profiled_methods->sort(&compare_methods);

    int count = collected_profiled_methods->length();
    int total_size = 0;
    if (count > 0) {
      for (int index = 0; index < count; index++) {
        Method* m = collected_profiled_methods->at(index);
        ttyLocker ttyl;
        tty->print_cr("------------------------------------------------------------------------");
        m->print_invocation_count();
        tty->print_cr("  mdo size: %d bytes", m->method_data()->size_in_bytes());
        tty->cr();
        // Dump data on parameters if any
        if (m->method_data() != NULL && m->method_data()->parameters_type_data() != NULL) {
          tty->fill_to(2);
          m->method_data()->parameters_type_data()->print_data_on(tty);
        }
        m->print_codes();
        total_size += m->method_data()->size_in_bytes();
      }
      tty->print_cr("------------------------------------------------------------------------");
      tty->print_cr("Total MDO size: %d bytes", total_size);
    }
  }
}

// Note: before_exit() can be executed only once, if more than one threads
//       are trying to shutdown the VM at the same time, only one thread
//       can run before_exit() and all other threads must wait.
void before_exit(JavaThread* thread) {
  #define BEFORE_EXIT_NOT_RUN 0
  #define BEFORE_EXIT_RUNNING 1
  #define BEFORE_EXIT_DONE    2
  static jint volatile _before_exit_status = BEFORE_EXIT_NOT_RUN;

  // Note: don't use a Mutex to guard the entire before_exit(), as
  // JVMTI post_thread_end_event and post_vm_death_event will run native code.
  // A CAS or OSMutex would work just fine but then we need to manipulate
  // thread state for Safepoint. Here we use Monitor wait() and notify_all()
  // for synchronization.
  { MutexLocker ml(BeforeExit_lock);
    switch (_before_exit_status) {
    case BEFORE_EXIT_NOT_RUN:
      _before_exit_status = BEFORE_EXIT_RUNNING;
      break;
    case BEFORE_EXIT_RUNNING:
      while (_before_exit_status == BEFORE_EXIT_RUNNING) {
        BeforeExit_lock->wait();
      }
      return;
    case BEFORE_EXIT_DONE:
      // need block to avoid SS compiler bug
      {
        return;
      }
    }
  }

  // We are not using CATCH here because we want the exit to continue normally.
  Thread* THREAD = thread;
  JVMCIRuntime::shutdown(THREAD);
  if (HAS_PENDING_EXCEPTION) {
    HandleMark hm(THREAD);
    Handle exception(THREAD, PENDING_EXCEPTION);
    CLEAR_PENDING_EXCEPTION;
    java_lang_Throwable::java_printStackTrace(exception, THREAD);
  }

  EventThreadEnd event;
  if (event.should_commit()) {
    event.set_thread(JFR_THREAD_ID(thread));
    event.commit();
  }

  // Stop the WatcherThread. We do this before disenrolling various
  // PeriodicTasks to reduce the likelihood of races.
  if (PeriodicTask::num_tasks() > 0) {
    WatcherThread::stop();
  }

  // shut down the StatSampler task
  StatSampler::disengage();
  StatSampler::destroy();

  // Stop concurrent GC threads
  Universe::heap()->stop();

  // Print GC/heap related information.
  Log(gc, heap, exit) log;
  if (log.is_info()) {
    ResourceMark rm;
    LogStream ls_info(log.info());
    Universe::print_on(&ls_info);
    if (log.is_trace()) {
      LogStream ls_trace(log.trace());
      ClassLoaderDataGraph::print_on(&ls_trace);
    }
  }

  Threads::shutdown_vm_agents();

  // Terminate the signal thread
  // Note: we don't wait until it actually dies.
  os::terminate_signal_thread();

  { MutexLocker ml(BeforeExit_lock);
    _before_exit_status = BEFORE_EXIT_DONE;
    BeforeExit_lock->notify_all();
  }

  #undef BEFORE_EXIT_NOT_RUN
  #undef BEFORE_EXIT_RUNNING
  #undef BEFORE_EXIT_DONE
}

void vm_exit(int code) {
  Thread* thread = ThreadLocalStorage::is_initialized() ? Thread::current_or_null() : NULL;
  if (thread == NULL) {
    // very early initialization failure -- just exit
    vm_direct_exit(code);
  }

  if (VMThread::vm_thread() != NULL) {
    // Fire off a VM_Exit operation to bring VM to a safepoint and exit
    VM_Exit op(code);
    if (thread->is_Java_thread())
      ((JavaThread*)thread)->set_thread_state(_thread_in_vm);
    VMThread::execute(&op);
    // should never reach here; but in case something wrong with VM Thread.
    vm_direct_exit(code);
  } else {
    // VM thread is gone, just exit
    vm_direct_exit(code);
  }
  ShouldNotReachHere();
}

void notify_vm_shutdown() { }

void vm_direct_exit(int code) {
  notify_vm_shutdown();
  os::wait_for_keypress_at_exit();
  os::exit(code);
}

void vm_perform_shutdown_actions() {
  if (is_init_completed()) {
    Thread* thread = Thread::current_or_null();
    if (thread != NULL && thread->is_Java_thread()) {
      // We are leaving the VM, set state to native (in case any OS exit
      // handlers call back to the VM)
      JavaThread* jt = (JavaThread*)thread;
      // Must always be walkable or have no last_Java_frame when in
      // thread_in_native
      jt->frame_anchor()->make_walkable(jt);
      jt->set_thread_state(_thread_in_native);
    }
  }
  notify_vm_shutdown();
}

void vm_shutdown() {
  vm_perform_shutdown_actions();
  os::wait_for_keypress_at_exit();
  os::shutdown();
}

void vm_abort(bool dump_core) {
  vm_perform_shutdown_actions();
  os::wait_for_keypress_at_exit();

  // Flush stdout and stderr before abort.
  fflush(stdout);
  fflush(stderr);

  os::abort(dump_core);
  ShouldNotReachHere();
}

void vm_notify_during_shutdown(const char* error, const char* message) {
  if (error != NULL) {
    tty->print_cr("Error occurred during initialization of VM");
    tty->print("%s", error);
    if (message != NULL) {
      tty->print_cr(": %s", message);
    } else {
      tty->cr();
    }
  }
}

void vm_exit_during_initialization() {
  vm_notify_during_shutdown(NULL, NULL);

  // Failure during initialization, we don't want to dump core
  vm_abort(false);
}

void vm_exit_during_initialization(Handle exception) {
  tty->print_cr("Error occurred during initialization of VM");
  // If there are exceptions on this thread it must be cleared
  // first and here. Any future calls to EXCEPTION_MARK requires
  // that no pending exceptions exist.
  Thread *THREAD = Thread::current(); // can't be NULL
  if (HAS_PENDING_EXCEPTION) {
    CLEAR_PENDING_EXCEPTION;
  }
  java_lang_Throwable::print_stack_trace(exception, tty);
  tty->cr();
  vm_notify_during_shutdown(NULL, NULL);

  // Failure during initialization, we don't want to dump core
  vm_abort(false);
}

void vm_exit_during_initialization(Symbol* ex, const char* message) {
  ResourceMark rm;
  vm_notify_during_shutdown(ex->as_C_string(), message);

  // Failure during initialization, we don't want to dump core
  vm_abort(false);
}

void vm_exit_during_initialization(const char* error, const char* message) {
  vm_notify_during_shutdown(error, message);

  // Failure during initialization, we don't want to dump core
  vm_abort(false);
}

void vm_shutdown_during_initialization(const char* error, const char* message) {
  vm_notify_during_shutdown(error, message);
  vm_shutdown();
}

JDK_Version JDK_Version::_current;
const char* JDK_Version::_runtime_name;
const char* JDK_Version::_runtime_version;

void JDK_Version::initialize() {
  jdk_version_info info;

  void *lib_handle = os::native_java_library();
  jdk_version_info_fn_t func = CAST_TO_FN_PTR(jdk_version_info_fn_t, os::dll_lookup(lib_handle, "JDK_GetVersionInfo0"));

  (*func)(&info, sizeof(info));

  int major = JDK_VERSION_MAJOR(info.jdk_version);
  int minor = JDK_VERSION_MINOR(info.jdk_version);
  int security = JDK_VERSION_SECURITY(info.jdk_version);
  int build = JDK_VERSION_BUILD(info.jdk_version);

  // Incompatible with pre-4243978 JDK.
  if (info.pending_list_uses_discovered_field == 0) {
    vm_exit_during_initialization("Incompatible JDK is not using Reference.discovered field for pending list");
  }
  _current = JDK_Version(major, minor, security, info.patch_version, build, info.thread_park_blocker == 1, info.post_vm_init_hook_enabled == 1);
}

void JDK_Version_init() {
  JDK_Version::initialize();
}

static int64_t encode_jdk_version(const JDK_Version& v) {
  return
    ((int64_t)v.major_version()          << (BitsPerByte * 4)) |
    ((int64_t)v.minor_version()          << (BitsPerByte * 3)) |
    ((int64_t)v.security_version()       << (BitsPerByte * 2)) |
    ((int64_t)v.patch_version()          << (BitsPerByte * 1)) |
    ((int64_t)v.build_number()           << (BitsPerByte * 0));
}

int JDK_Version::compare(const JDK_Version& other) const {
  uint64_t e = encode_jdk_version(*this);
  uint64_t o = encode_jdk_version(other);
  return (e > o) ? 1 : ((e == o) ? 0 : -1);
}

void JDK_Version::to_string(char* buffer, size_t buflen) const {
  size_t index = 0;

  if (!is_valid()) {
    jio_snprintf(buffer, buflen, "%s", "(uninitialized)");
  } else {
    int rc = jio_snprintf(&buffer[index], buflen - index, "%d.%d", _major, _minor);
    if (rc == -1) return;
    index += rc;
    if (_security > 0) {
      rc = jio_snprintf(&buffer[index], buflen - index, ".%d", _security);
      if (rc == -1) return;
      index += rc;
    }
    if (_patch > 0) {
      rc = jio_snprintf(&buffer[index], buflen - index, ".%d", _patch);
      if (rc == -1) return;
      index += rc;
    }
    if (_build > 0) {
      rc = jio_snprintf(&buffer[index], buflen - index, "+%d", _build);
      if (rc == -1) return;
      index += rc;
    }
  }
}
