#include "precompiled.hpp"

#include "classfile/systemDictionary.hpp"
#include "compiler/compileBroker.hpp"
#include "memory/allocation.inline.hpp"
#include "memory/iterator.hpp"
#include "memory/oopFactory.hpp"
#include "memory/resourceArea.hpp"
#include "oops/klass.hpp"
#include "oops/objArrayKlass.hpp"
#include "oops/objArrayOop.inline.hpp"
#include "oops/oop.inline.hpp"
#include "oops/typeArrayOop.inline.hpp"
#include "runtime/arguments.hpp"
#include "runtime/flags/jvmFlag.hpp"
#include "runtime/globals.hpp"
#include "runtime/handles.inline.hpp"
#include "runtime/interfaceSupport.inline.hpp"
#include "runtime/javaCalls.hpp"
#include "runtime/jniHandles.inline.hpp"
#include "runtime/os.hpp"
#include "runtime/thread.inline.hpp"
#include "runtime/threadSMR.hpp"
#include "services/management.hpp"
#include "services/memoryManager.hpp"
#include "services/memoryPool.hpp"
#include "services/memoryService.hpp"
#include "services/threadService.hpp"
#include "utilities/debug.hpp"
#include "utilities/formatBuffer.hpp"
#include "utilities/macros.hpp"

InstanceKlass* Management::_diagnosticCommandImpl_klass = NULL;
InstanceKlass* Management::_garbageCollectorExtImpl_klass = NULL;
InstanceKlass* Management::_garbageCollectorMXBean_klass = NULL;
InstanceKlass* Management::_gcInfo_klass = NULL;
InstanceKlass* Management::_managementFactoryHelper_klass = NULL;
InstanceKlass* Management::_memoryManagerMXBean_klass = NULL;
InstanceKlass* Management::_memoryPoolMXBean_klass = NULL;
InstanceKlass* Management::_memoryUsage_klass = NULL;
InstanceKlass* Management::_sensor_klass = NULL;
InstanceKlass* Management::_threadInfo_klass = NULL;

TimeStamp Management::_stamp;

void management_init() {
  ThreadService::init();
}

static void validate_thread_id_array(typeArrayHandle ids_ah, TRAPS) {
  int num_threads = ids_ah->length();

  // Validate input thread IDs
  int i = 0;
  for (i = 0; i < num_threads; i++) {
    jlong tid = ids_ah->long_at(i);
    if (tid <= 0) {
      // throw exception if invalid thread id.
      THROW_MSG(vmSymbols::java_lang_IllegalArgumentException(), "Invalid thread ID entry");
    }
  }
}

// Gets an array containing the amount of memory allocated on the Java
// heap for a set of threads (in bytes).  Each element of the array is
// the amount of memory allocated for the thread ID specified in the
// corresponding entry in the given array of thread IDs; or -1 if the
// thread does not exist or has terminated.
JVM_ENTRY(void, jmm_GetThreadAllocatedMemory(JNIEnv *env, jlongArray ids, jlongArray sizeArray))
  // Check if threads is null
  if (ids == NULL || sizeArray == NULL) {
    THROW(vmSymbols::java_lang_NullPointerException());
  }

  ResourceMark rm(THREAD);
  typeArrayOop ta = typeArrayOop(JNIHandles::resolve_non_null(ids));
  typeArrayHandle ids_ah(THREAD, ta);

  typeArrayOop sa = typeArrayOop(JNIHandles::resolve_non_null(sizeArray));
  typeArrayHandle sizeArray_h(THREAD, sa);

  // validate the thread id array
  validate_thread_id_array(ids_ah, CHECK);

  // sizeArray must be of the same length as the given array of thread IDs
  int num_threads = ids_ah->length();
  if (num_threads != sizeArray_h->length()) {
    THROW_MSG(vmSymbols::java_lang_IllegalArgumentException(), "The length of the given long array does not match the length of the given array of thread IDs");
  }

  ThreadsListHandle tlh;
  for (int i = 0; i < num_threads; i++) {
    JavaThread* java_thread = tlh.list()->find_JavaThread_from_java_tid(ids_ah->long_at(i));
    if (java_thread != NULL) {
      sizeArray_h->long_at_put(i, java_thread->cooked_allocated_bytes());
    }
  }
JVM_END

// Returns the CPU time consumed by a given thread (in nanoseconds).
// If thread_id == 0, CPU time for the current thread is returned.
// If user_sys_cpu_time = true, user level and system CPU time of
// a given thread is returned; otherwise, only user level CPU time
// is returned.
JVM_ENTRY(jlong, jmm_GetThreadCpuTimeWithKind(JNIEnv *env, jlong thread_id, jboolean user_sys_cpu_time))
  if (!os::is_thread_cpu_time_supported()) {
    return -1;
  }

  if (thread_id < 0) {
    THROW_MSG_(vmSymbols::java_lang_IllegalArgumentException(), "Invalid thread ID", -1);
  }

  JavaThread* java_thread = NULL;
  if (thread_id == 0) {
    // current thread
    return os::current_thread_cpu_time(user_sys_cpu_time != 0);
  } else {
    ThreadsListHandle tlh;
    java_thread = tlh.list()->find_JavaThread_from_java_tid(thread_id);
    if (java_thread != NULL) {
      return os::thread_cpu_time((Thread*) java_thread, user_sys_cpu_time != 0);
    }
  }
  return -1;
JVM_END

// Gets an array containing the CPU times consumed by a set of threads
// (in nanoseconds).  Each element of the array is the CPU time for the
// thread ID specified in the corresponding entry in the given array
// of thread IDs; or -1 if the thread does not exist or has terminated.
// If user_sys_cpu_time = true, the sum of user level and system CPU time
// for the given thread is returned; otherwise, only user level CPU time
// is returned.
JVM_ENTRY(void, jmm_GetThreadCpuTimesWithKind(JNIEnv *env, jlongArray ids, jlongArray timeArray, jboolean user_sys_cpu_time))
  // Check if threads is null
  if (ids == NULL || timeArray == NULL) {
    THROW(vmSymbols::java_lang_NullPointerException());
  }

  ResourceMark rm(THREAD);
  typeArrayOop ta = typeArrayOop(JNIHandles::resolve_non_null(ids));
  typeArrayHandle ids_ah(THREAD, ta);

  typeArrayOop tia = typeArrayOop(JNIHandles::resolve_non_null(timeArray));
  typeArrayHandle timeArray_h(THREAD, tia);

  // validate the thread id array
  validate_thread_id_array(ids_ah, CHECK);

  // timeArray must be of the same length as the given array of thread IDs
  int num_threads = ids_ah->length();
  if (num_threads != timeArray_h->length()) {
    THROW_MSG(vmSymbols::java_lang_IllegalArgumentException(), "The length of the given long array does not match the length of the given array of thread IDs");
  }

  ThreadsListHandle tlh;
  for (int i = 0; i < num_threads; i++) {
    JavaThread* java_thread = tlh.list()->find_JavaThread_from_java_tid(ids_ah->long_at(i));
    if (java_thread != NULL) {
      timeArray_h->long_at_put(i, os::thread_cpu_time((Thread*)java_thread, user_sys_cpu_time != 0));
    }
  }
JVM_END

void* Management::get_jmm_interface(int version) {
  return NULL;
}
