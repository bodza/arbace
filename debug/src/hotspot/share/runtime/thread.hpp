#ifndef SHARE_VM_RUNTIME_THREAD_HPP
#define SHARE_VM_RUNTIME_THREAD_HPP

#include "jni.h"
#include "gc/shared/gcThreadLocalData.hpp"
#include "gc/shared/threadLocalAllocBuffer.hpp"
#include "memory/allocation.hpp"
#include "oops/oop.hpp"
#include "runtime/frame.hpp"
#include "runtime/globals.hpp"
#include "runtime/handshake.hpp"
#include "runtime/javaFrameAnchor.hpp"
#include "runtime/jniHandles.hpp"
#include "runtime/mutexLocker.hpp"
#include "runtime/os.hpp"
#include "runtime/osThread.hpp"
#include "runtime/park.hpp"
#include "runtime/safepoint.hpp"
#include "runtime/stubRoutines.hpp"
#include "runtime/threadHeapSampler.hpp"
#include "runtime/threadLocalStorage.hpp"
#include "runtime/threadStatisticalInfo.hpp"
#include "utilities/align.hpp"
#include "utilities/exceptions.hpp"
#include "utilities/macros.hpp"

class SafeThreadsListPtr;
class ThreadSafepointState;
class ThreadsList;
class ThreadsSMRSupport;

class ThreadStatistics;
class ConcurrentLocksDump;
class ParkEvent;
class Parker;

class ciEnv;
class CompileThread;
class CompileTask;
class CompileQueue;
class CompilerCounters;
class vframeArray;

class GCTaskQueue;
class ThreadClosure;
class IdealGraphPrinter;

class Metadata;
template <class T, MEMFLAGS F> class ChunkedList;
typedef ChunkedList<Metadata*, mtInternal> MetadataOnStackBuffer;

class WorkerThread;

// Class hierarchy
// - Thread
//   - NamedThread
//     - VMThread
//     - ConcurrentGCThread
//     - WorkerThread
//       - GangWorker
//       - GCTaskThread
//   - JavaThread
//     - various subclasses e.g. CompilerThread
//   - WatcherThread

class Thread: public ThreadShadow {
  friend class VMStructs;
  friend class JVMCIVMStructs;
 private:
#ifndef USE_LIBRARY_BASED_TLS_ONLY
  // Current thread is maintained as a thread-local variable
  static THREAD_LOCAL_DECL Thread* _thr_current;
#endif

 private:
  // Thread local data area available to the GC. The internal
  // structure and contents of this data area is GC-specific.
  // Only GC and GC barrier code should access this data area.
  GCThreadLocalData _gc_data;

 public:
  static ByteSize gc_data_offset() {
    return byte_offset_of(Thread, _gc_data);
  }

  template <typename T> T* gc_data() {
    return reinterpret_cast<T*>(&_gc_data);
  }

  // Exception handling
  // (Note: _pending_exception and friends are in ThreadShadow)
  //oop       _pending_exception;                // pending exception for current thread
  // const char* _exception_file;                   // file information for exception (debugging only)
  // int         _exception_line;                   // line information for exception (debugging only)
 protected:
  // Support for forcing alignment of thread objects for biased locking
  void*       _real_malloc_address;

  // JavaThread lifecycle support:
  friend class SafeThreadsListPtr;  // for _threads_list_ptr, cmpxchg_threads_hazard_ptr(), {dec_,inc_,}nested_threads_hazard_ptr_cnt(), {g,s}et_threads_hazard_ptr(), inc_nested_handle_cnt(), tag_hazard_ptr() access
  friend class ScanHazardPtrGatherProtectedThreadsClosure;  // for cmpxchg_threads_hazard_ptr(), get_threads_hazard_ptr(), is_hazard_ptr_tagged() access
  friend class ScanHazardPtrGatherThreadsListClosure;  // for get_threads_hazard_ptr(), untag_hazard_ptr() access
  friend class ScanHazardPtrPrintMatchingThreadsClosure;  // for get_threads_hazard_ptr(), is_hazard_ptr_tagged() access
  friend class ThreadsSMRSupport;  // for _nested_threads_hazard_ptr_cnt, _threads_hazard_ptr, _threads_list_ptr access

  ThreadsList* volatile _threads_hazard_ptr;
  SafeThreadsListPtr*   _threads_list_ptr;
  ThreadsList*          cmpxchg_threads_hazard_ptr(ThreadsList* exchange_value, ThreadsList* compare_value);
  ThreadsList*          get_threads_hazard_ptr();
  void                  set_threads_hazard_ptr(ThreadsList* new_list);
  static bool           is_hazard_ptr_tagged(ThreadsList* list) {
    return (intptr_t(list) & intptr_t(1)) == intptr_t(1);
  }
  static ThreadsList*   tag_hazard_ptr(ThreadsList* list) {
    return (ThreadsList*)(intptr_t(list) | intptr_t(1));
  }
  static ThreadsList*   untag_hazard_ptr(ThreadsList* list) {
    return (ThreadsList*)(intptr_t(list) & ~intptr_t(1));
  }
  // This field is enabled via -XX:+EnableThreadSMRStatistics:
  uint _nested_threads_hazard_ptr_cnt;
  void dec_nested_threads_hazard_ptr_cnt() {
    _nested_threads_hazard_ptr_cnt--;
  }
  void inc_nested_threads_hazard_ptr_cnt() {
    _nested_threads_hazard_ptr_cnt++;
  }
  uint nested_threads_hazard_ptr_cnt() {
    return _nested_threads_hazard_ptr_cnt;
  }

 public:
  void* operator new(size_t size) throw() { return allocate(size, true); }
  void* operator new(size_t size, const std::nothrow_t& nothrow_constant) throw() { return allocate(size, false); }
  void  operator delete(void* p);

 protected:
  static void* allocate(size_t size, bool throw_excpt, MEMFLAGS flags = mtThread);
 private:
  // ***************************************************************
  // Suspend and resume support
  // ***************************************************************
  //
  // VM suspend/resume no longer exists - it was once used for various
  // things including safepoints but was deprecated and finally removed
  // in Java 7. Because VM suspension was considered "internal" Java-level
  // suspension was considered "external", and this legacy naming scheme
  // remains.
  //
  // External suspend/resume requests come from JVM_SuspendThread,
  // JVM_ResumeThread, JVMTI SuspendThread, and finally JVMTI
  // ResumeThread. External
  // suspend requests cause _external_suspend to be set and external
  // resume requests cause _external_suspend to be cleared.
  // External suspend requests do not nest on top of other external
  // suspend requests. The higher level APIs reject suspend requests
  // for already suspended threads.
  //
  // The external_suspend
  // flag is checked by has_special_runtime_exit_condition() and java thread
  // will self-suspend when handle_special_runtime_exit_condition() is
  // called. Most uses of the _thread_blocked state in JavaThreads are
  // considered the same as being externally suspended; if the blocking
  // condition lifts, the JavaThread will self-suspend. Other places
  // where VM checks for external_suspend include:
  //   + mutex granting (do not enter monitors when thread is suspended)
  //   + state transitions from _thread_in_native
  //
  // In general, java_suspend() does not wait for an external suspend
  // request to complete. When it returns, the only guarantee is that
  // the _external_suspend field is true.
  //
  // wait_for_ext_suspend_completion() is used to wait for an external
  // suspend request to complete. External suspend requests are usually
  // followed by some other interface call that requires the thread to
  // be quiescent, e.g., GetCallTrace(). By moving the "wait time" into
  // the interface that requires quiescence, we give the JavaThread a
  // chance to self-suspend before we need it to be quiescent. This
  // improves overall suspend/query performance.
  //
  // _suspend_flags controls the behavior of java_ suspend/resume.
  // It must be set under the protection of SR_lock. Read from the flag is
  // OK without SR_lock as long as the value is only used as a hint.
  // (e.g., check _external_suspend first without lock and then recheck
  // inside SR_lock and finish the suspension)
  //
  // _suspend_flags is also overloaded for other "special conditions" so
  // that a single check indicates whether any special action is needed
  // eg. for async exceptions.
  // -------------------------------------------------------------------
  // Notes:
  // 1. The suspend/resume logic no longer uses ThreadState in OSThread
  // but we still update its value to keep other part of the system (mainly
  // JVMTI) happy. ThreadState is legacy code (see notes in
  // osThread.hpp).
  //
  // 2. It would be more natural if set_external_suspend() is private and
  // part of java_suspend(), but that probably would affect the suspend/query
  // performance. Need more investigation on this.

  // suspend/resume lock: used for self-suspend
  Monitor* _SR_lock;

 protected:
  enum SuspendFlags {
    // NOTE: avoid using the sign-bit as cc generates different test code
    //       when the sign-bit is used, and sometimes incorrectly - see CR 6398077

    _external_suspend       = 0x20000000U, // thread is asked to self suspend
    _ext_suspended          = 0x40000000U, // thread has self-suspended

    _has_async_exception    = 0x00000001U, // there is a pending async exception
    _critical_native_unlock = 0x00000002U, // Must call back to unlock JNI critical lock

    _trace_flag             = 0x00000004U  // call tracing backend
  };

  // various suspension related flags - atomically updated
  // overloaded for async exception checking in check_special_condition_for_native_trans.
  volatile uint32_t _suspend_flags;

 private:
  int _num_nested_signal;

 public:
  void enter_signal_handler() { _num_nested_signal++; }
  void leave_signal_handler() { _num_nested_signal--; }
  bool is_inside_signal_handler() const { return _num_nested_signal > 0; }

 private:
  // Active_handles points to a block of handles
  JNIHandleBlock* _active_handles;

  // One-element thread local free list
  JNIHandleBlock* _free_handle_block;

  // Point to the last handle mark
  HandleMark* _last_handle_mark;

  // The parity of the last strong_roots iteration in which this thread was
  // claimed as a task.
  int _oops_do_parity;

  // Support for GlobalCounter
 private:
  volatile uintx _rcu_counter;
 public:
  volatile uintx* get_rcu_counter() {
    return &_rcu_counter;
  }

 public:
  void set_last_handle_mark(HandleMark* mark)   { _last_handle_mark = mark; }
  HandleMark* last_handle_mark()          const { return _last_handle_mark; }
 private:
  friend class NoAllocVerifier;
  friend class NoSafepointVerifier;
  friend class PauseNoSafepointVerifier;
  friend class GCLocker;

  volatile void* _polling_page;                 // Thread local polling page

  ThreadLocalAllocBuffer _tlab;                 // Thread-local eden
  jlong _allocated_bytes;                       // Cumulative number of bytes allocated on
                                                // the Java heap
  ThreadHeapSampler _heap_sampler;              // For use when sampling the memory.

  ThreadStatisticalInfo _statistical_info;      // Statistics about the thread

  int   _vm_operation_started_count;            // VM_Operation support
  int   _vm_operation_completed_count;          // VM_Operation support

  ObjectMonitor* _current_pending_monitor;      // ObjectMonitor this thread
                                                // is waiting to lock
  bool _current_pending_monitor_is_from_java;   // locking is from Java code

  // ObjectMonitor on which this thread called Object.wait()
  ObjectMonitor* _current_waiting_monitor;

  // Private thread-local objectmonitor list - a simple cache organized as a SLL.
 public:
  ObjectMonitor* omFreeList;
  int omFreeCount;                              // length of omFreeList
  int omFreeProvision;                          // reload chunk size
  ObjectMonitor* omInUseList;                   // SLL to track monitors in circulation
  int omInUseCount;                             // length of omInUseList

 public:
  enum {
    is_definitely_current_thread = true
  };

  // Constructor
  Thread();
  virtual ~Thread();

  // Manage Thread::current()
  void initialize_thread_current();
  static void clear_thread_current(); // TLS cleanup needed before threads terminate

 protected:
  // To be implemented by children.
  virtual void run() = 0;

 public:
  // invokes <ChildThreadClass>::run(), with common preparations and cleanups.
  void call_run();

  // Testers
  virtual bool is_VM_thread()                  const { return false; }
  virtual bool is_Java_thread()                const { return false; }
  virtual bool is_Compiler_thread()            const { return false; }
  virtual bool is_Code_cache_sweeper_thread()  const { return false; }
  virtual bool is_hidden_from_external_view()  const { return false; }
  virtual bool is_jvmti_agent_thread()         const { return false; }
  // True iff the thread can perform GC operations at a safepoint.
  // Generally will be true only of VM thread and parallel GC WorkGang
  // threads.
  virtual bool is_GC_task_thread()             const { return false; }
  virtual bool is_Watcher_thread()             const { return false; }
  virtual bool is_ConcurrentGC_thread()        const { return false; }
  virtual bool is_Named_thread()               const { return false; }
  virtual bool is_Worker_thread()              const { return false; }

  // Can this thread make Java upcalls
  virtual bool can_call_java()                 const { return false; }

  // Casts
  virtual WorkerThread* as_Worker_thread()     const { return NULL; }

  virtual char* name() const { return (char*)"Unknown thread"; }

  // Returns the current thread (ASSERTS if NULL)
  static inline Thread* current();
  // Returns the current thread, or NULL if not attached
  static inline Thread* current_or_null();
  // Returns the current thread, or NULL if not attached, and is
  // safe for use from signal-handlers
  static inline Thread* current_or_null_safe();

  // Common thread operations
  static void set_priority(Thread* thread, ThreadPriority priority);
  static ThreadPriority get_priority(const Thread* const thread);
  static void start(Thread* thread);
  static void interrupt(Thread* thr);
  static bool is_interrupted(Thread* thr, bool clear_interrupted);

  void set_native_thread_name(const char *name) {
    os::set_native_thread_name(name);
  }

  ObjectMonitor** omInUseList_addr()             { return (ObjectMonitor **)&omInUseList; }
  Monitor* SR_lock()                       const { return _SR_lock; }

  bool has_async_exception() const { return (_suspend_flags & _has_async_exception) != 0; }

  inline void set_suspend_flag(SuspendFlags f);
  inline void clear_suspend_flag(SuspendFlags f);

  inline void set_has_async_exception();
  inline void clear_has_async_exception();

  bool do_critical_native_unlock() const { return (_suspend_flags & _critical_native_unlock) != 0; }

  inline void set_critical_native_unlock();
  inline void clear_critical_native_unlock();

  inline void set_trace_flag();
  inline void clear_trace_flag();

 public:
  // Installs a pending exception to be inserted later
  static void send_async_exception(oop thread_oop, oop java_throwable);

  // Resource area
  ResourceArea* resource_area()            const { return _resource_area; }
  void set_resource_area(ResourceArea* area)     { _resource_area = area; }

  OSThread* osthread()                     const { return _osthread; }
  void set_osthread(OSThread* thread)            { _osthread = thread; }

  // JNI handle support
  JNIHandleBlock* active_handles()         const { return _active_handles; }
  void set_active_handles(JNIHandleBlock* block) { _active_handles = block; }
  JNIHandleBlock* free_handle_block()      const { return _free_handle_block; }
  void set_free_handle_block(JNIHandleBlock* block) { _free_handle_block = block; }

  // Internal handle support
  HandleArea* handle_area()                const { return _handle_area; }
  void set_handle_area(HandleArea* area)         { _handle_area = area; }

  GrowableArray<Metadata*>* metadata_handles()          const { return _metadata_handles; }
  void set_metadata_handles(GrowableArray<Metadata*>* handles) { _metadata_handles = handles; }

  // Thread-Local Allocation Buffer (TLAB) support
  ThreadLocalAllocBuffer& tlab()                 { return _tlab; }
  void initialize_tlab() {
    if (UseTLAB) {
      tlab().initialize();
    }
  }

  jlong allocated_bytes()               { return _allocated_bytes; }
  void set_allocated_bytes(jlong value) { _allocated_bytes = value; }
  void incr_allocated_bytes(jlong size) { _allocated_bytes += size; }
  inline jlong cooked_allocated_bytes();

  ThreadHeapSampler& heap_sampler()     { return _heap_sampler; }

  ThreadStatisticalInfo& statistical_info() { return _statistical_info; }

  bool is_trace_suspend()               { return (_suspend_flags & _trace_flag) != 0; }

  // VM operation support
  int vm_operation_ticket()                      { return ++_vm_operation_started_count; }
  int vm_operation_completed_count()             { return _vm_operation_completed_count; }
  void increment_vm_operation_completed_count()  { _vm_operation_completed_count++; }

  // For tracking the heavyweight monitor the thread is pending on.
  ObjectMonitor* current_pending_monitor() {
    return _current_pending_monitor;
  }
  void set_current_pending_monitor(ObjectMonitor* monitor) {
    _current_pending_monitor = monitor;
  }
  void set_current_pending_monitor_is_from_java(bool from_java) {
    _current_pending_monitor_is_from_java = from_java;
  }
  bool current_pending_monitor_is_from_java() {
    return _current_pending_monitor_is_from_java;
  }

  // For tracking the ObjectMonitor on which this thread called Object.wait()
  ObjectMonitor* current_waiting_monitor() {
    return _current_waiting_monitor;
  }
  void set_current_waiting_monitor(ObjectMonitor* monitor) {
    _current_waiting_monitor = monitor;
  }

  // GC support
  // Apply "f->do_oop" to all root oops in "this".
  //   Used by JavaThread::oops_do.
  // Apply "cf->do_code_blob" (if !NULL) to all code blobs active in frames
  virtual void oops_do(OopClosure* f, CodeBlobClosure* cf);

  // Handles the parallel case for the method below.
 private:
  bool claim_oops_do_par_case(int collection_parity);
 public:
  // Requires that "collection_parity" is that of the current roots
  // iteration.  If "is_par" is false, sets the parity of "this" to
  // "collection_parity", and returns "true".  If "is_par" is true,
  // uses an atomic instruction to set the current threads parity to
  // "collection_parity", if it is not already.  Returns "true" iff the
  // calling thread does the update, this indicates that the calling thread
  // has claimed the thread's stack as a root groop in the current
  // collection.
  bool claim_oops_do(bool is_par, int collection_parity) {
    if (!is_par) {
      _oops_do_parity = collection_parity;
      return true;
    } else {
      return claim_oops_do_par_case(collection_parity);
    }
  }

  // jvmtiRedefineClasses support
  void metadata_handles_do(void f(Metadata*));

  // Used by fast lock support
  virtual bool is_lock_owned(address adr) const;

  // Check if address is in the stack of the thread (not just for locks).
  // Warning: the method can only be used on the running thread
  bool is_in_stack(address adr) const;
  // Check if address is in the usable part of the stack (excludes protected
  // guard pages)
  bool is_in_usable_stack(address adr) const;

  // Sets this thread as starting thread. Returns failure if thread
  // creation fails due to lack of memory, too many threads etc.
  bool set_as_starting_thread();

protected:
  // OS data associated with the thread
  OSThread* _osthread;  // Platform-specific thread information

  // Thread local resource area for temporary allocation within the VM
  ResourceArea* _resource_area;

  // Thread local handle area for allocation of handles within the VM
  HandleArea* _handle_area;
  GrowableArray<Metadata*>* _metadata_handles;

  // Support for stack overflow handling, get_thread, etc.
  address          _stack_base;
  size_t           _stack_size;
  uintptr_t        _self_raw_id;      // used by get_thread (mutable)
  int              _lgrp_id;

  volatile void** polling_page_addr() { return &_polling_page; }

 public:
  // Stack overflow support
  address stack_base()           const { return _stack_base; }
  void    set_stack_base(address base) { _stack_base = base; }
  size_t  stack_size()           const { return _stack_size; }
  void    set_stack_size(size_t size)  { _stack_size = size; }
  address stack_end()            const { return stack_base() - stack_size(); }
  void    record_stack_base_and_size();
  void    register_thread_stack_with_NMT() { };

  bool    on_local_stack(address adr) const {
    // QQQ this has knowledge of direction, ought to be a stack method
    return (_stack_base >= adr && adr >= stack_end());
  }

  uintptr_t self_raw_id()                    { return _self_raw_id; }
  void      set_self_raw_id(uintptr_t value) { _self_raw_id = value; }

  int     lgrp_id()        const { return _lgrp_id; }
  void    set_lgrp_id(int value) { _lgrp_id = value; }

  // Printing
  void print_on(outputStream* st, bool print_extended_info) const;
  virtual void print_on(outputStream* st) const { print_on(st, false); }
  void print() const { print_on(tty); }
  virtual void print_on_error(outputStream* st, char* buf, int buflen) const;
  void print_value_on(outputStream* st) const;

  // Debug-only code

  void check_for_valid_safepoint_state(bool potential_vm_operation) { };

 public:
  // Code generation
  static ByteSize exception_file_offset()        { return byte_offset_of(Thread, _exception_file); }
  static ByteSize exception_line_offset()        { return byte_offset_of(Thread, _exception_line); }
  static ByteSize active_handles_offset()        { return byte_offset_of(Thread, _active_handles); }

  static ByteSize stack_base_offset()            { return byte_offset_of(Thread, _stack_base); }
  static ByteSize stack_size_offset()            { return byte_offset_of(Thread, _stack_size); }

  static ByteSize polling_page_offset()          { return byte_offset_of(Thread, _polling_page); }

#define TLAB_FIELD_OFFSET(name) \
  static ByteSize tlab_##name##_offset()         { return byte_offset_of(Thread, _tlab) + ThreadLocalAllocBuffer::name##_offset(); }

  TLAB_FIELD_OFFSET(start)
  TLAB_FIELD_OFFSET(end)
  TLAB_FIELD_OFFSET(top)
  TLAB_FIELD_OFFSET(pf_top)
  TLAB_FIELD_OFFSET(size)                   // desired_size
  TLAB_FIELD_OFFSET(refill_waste_limit)
  TLAB_FIELD_OFFSET(number_of_refills)
  TLAB_FIELD_OFFSET(fast_refill_waste)
  TLAB_FIELD_OFFSET(slow_allocations)

#undef TLAB_FIELD_OFFSET

  static ByteSize allocated_bytes_offset()       { return byte_offset_of(Thread, _allocated_bytes); }

 public:
  volatile intptr_t _Stalled;
  volatile int _TypeTag;
  ParkEvent * _ParkEvent;                     // for synchronized()
  ParkEvent * _SleepEvent;                    // for Thread.sleep
  ParkEvent * _MutexEvent;                    // for native internal Mutex/Monitor
  ParkEvent * _MuxEvent;                      // for low-level muxAcquire-muxRelease
  int NativeSyncRecursion;                    // diagnostic

  volatile int _OnTrap;                       // Resume-at IP delta
  jint _hashStateW;                           // Marsaglia Shift-XOR thread-local RNG
  jint _hashStateX;                           // thread-specific hashCode generator state
  jint _hashStateY;
  jint _hashStateZ;
  void * _schedctl;

  volatile jint rng[4];                      // RNG for spin loop

  // Low-level leaf-lock primitives used to implement synchronization
  // and native monitor-mutex infrastructure.
  // Not for general synchronization use.
  static void SpinAcquire(volatile int * Lock, const char * Name);
  static void SpinRelease(volatile int * Lock);
  static void muxAcquire(volatile intptr_t * Lock, const char * Name);
  static void muxAcquireW(volatile intptr_t * Lock, ParkEvent * ev);
  static void muxRelease(volatile intptr_t * Lock);
};

// Inline implementation of Thread::current()
inline Thread* Thread::current() {
  Thread* current = current_or_null();
  return current;
}

inline Thread* Thread::current_or_null() {
#ifndef USE_LIBRARY_BASED_TLS_ONLY
  return _thr_current;
#else
  if (ThreadLocalStorage::is_initialized()) {
    return ThreadLocalStorage::thread();
  }
  return NULL;
#endif
}

inline Thread* Thread::current_or_null_safe() {
  if (ThreadLocalStorage::is_initialized()) {
    return ThreadLocalStorage::thread();
  }
  return NULL;
}

// Name support for threads.  non-JavaThread subclasses with multiple
// uniquely named instances should derive from this.
class NamedThread: public Thread {
  friend class VMStructs;
  enum {
    max_name_len = 64
  };
 private:
  char* _name;
  // log JavaThread being processed by oops_do
  JavaThread* _processed_thread;
  uint _gc_id; // The current GC id when a thread takes part in GC

 public:
  NamedThread();
  ~NamedThread();
  // May only be called once per thread.
  void set_name(const char* format, ...)  ATTRIBUTE_PRINTF(2, 3);
  void initialize_named_thread();
  virtual bool is_Named_thread() const { return true; }
  virtual char* name() const { return _name == NULL ? (char*)"Unknown Thread" : _name; }
  JavaThread *processed_thread() { return _processed_thread; }
  void set_processed_thread(JavaThread *thread) { _processed_thread = thread; }
  virtual void print_on(outputStream* st) const;

  void set_gc_id(uint gc_id) { _gc_id = gc_id; }
  uint gc_id() { return _gc_id; }
};

// Worker threads are named and have an id of an assigned work.
class WorkerThread: public NamedThread {
 private:
  uint _id;
 public:
  WorkerThread() : _id(0)               { }
  virtual bool is_Worker_thread() const { return true; }

  virtual WorkerThread* as_Worker_thread() const {
    return (WorkerThread*) this;
  }

  void set_id(uint work_id)             { _id = work_id; }
  uint id()                       const { return _id; }
};

// A single WatcherThread is used for simulating timer interrupts.
class WatcherThread: public Thread {
  friend class VMStructs;
 public:
  virtual void run();

 private:
  static WatcherThread* _watcher_thread;

  static bool _startable;
  // volatile due to at least one lock-free read
  volatile static bool _should_terminate;
 public:
  enum SomeConstants {
    delay_interval = 10                          // interrupt delay in milliseconds
  };

  // Constructor
  WatcherThread();

  // No destruction allowed
  ~WatcherThread() {
    guarantee(false, "WatcherThread deletion must fix the race with VM termination");
  }

  // Tester
  bool is_Watcher_thread()                 const { return true; }

  // Printing
  char* name() const { return (char*)"VM Periodic Task Thread"; }
  void print_on(outputStream* st) const;
  void unpark();

  // Returns the single instance of WatcherThread
  static WatcherThread* watcher_thread()         { return _watcher_thread; }

  // Create and start the single instance of WatcherThread, or stop it on shutdown
  static void start();
  static void stop();
  // Only allow start once the VM is sufficiently initialized
  // Otherwise the first task to enroll will trigger the start
  static void make_startable();
 private:
  int sleep() const;
};

class CompilerThread;

typedef void (*ThreadFunction)(JavaThread*, TRAPS);

class JavaThread: public Thread {
  friend class VMStructs;
  friend class JVMCIVMStructs;
 private:
  JavaThread*    _next;                          // The next thread in the Threads list
  bool           _on_thread_list;                // Is set when this JavaThread is added to the Threads list
  oop            _threadObj;                     // The Java level thread object

  JavaFrameAnchor _anchor;                       // Encapsulation of current java frame and it state

  ThreadFunction _entry_point;

  JNIEnv        _jni_environment;

  intptr_t*      _must_deopt_id;                 // id of frame that needs to be deopted once we
                                                 // transition out of native
  CompiledMethod*       _deopt_nmethod;         // CompiledMethod that is currently being deoptimized
  vframeArray*  _vframe_array_head;              // Holds the heap of the active vframeArrays

  // Handshake value for fixing 6243940. We need a place for the i2c
  // adapter to store the callee Method*. This value is NEVER live
  // across a gc point so it does NOT have to be gc'd
  // The handshake is open ended since we can't be certain that it will
  // be NULLed. This is because we rarely ever see the race and end up
  // in handle_wrong_method which is the backend of the handshake. See
  // code in i2c adapters and handle_wrong_method.

  Method*       _callee_target;

  // Used to pass back results to the interpreter or generated code running Java code.
  oop           _vm_result;    // oop result is GC-preserved
  Metadata*     _vm_result_2;  // non-oop result

  // See ReduceInitialCardMarks: this holds the precise space interval of
  // the most recent slow path allocation for which compiled code has
  // elided card-marks for performance along the fast-path.
  MemRegion     _deferred_card_mark;

  MonitorChunk* _monitor_chunks;                 // Contains the off stack monitors
                                                 // allocated during deoptimization
                                                 // and by JNI_MonitorEnter/Exit

  // Async. requests support
  enum AsyncRequests {
    _no_async_condition = 0,
    _async_exception,
    _async_unsafe_access_error
  };
  AsyncRequests _special_runtime_exit_condition; // Enum indicating pending async. request
  oop           _pending_async_exception;

  // Safepoint support
 public:                                         // Expose _thread_state for SafeFetchInt()
  volatile JavaThreadState _thread_state;
 private:
  ThreadSafepointState *_safepoint_state;        // Holds information about a thread during a safepoint
  address               _saved_exception_pc;     // Saved pc of instruction where last implicit exception happened

  // JavaThread termination support
  enum TerminatedTypes {
    _not_terminated = 0xDEAD - 2,
    _thread_exiting,                             // JavaThread::exit() has been called for this thread
    _thread_terminated,                          // JavaThread is removed from thread list
    _vm_exited                                   // JavaThread is still executing native code, but VM is terminated
                                                 // only VM_Exit can set _vm_exited
  };

  // In general a JavaThread's _terminated field transitions as follows:
  //
  //   _not_terminated => _thread_exiting => _thread_terminated
  //
  // _vm_exited is a special value to cover the case of a JavaThread
  // executing native code after the VM itself is terminated.
  volatile TerminatedTypes _terminated;
  // suspend/resume support
  volatile bool         _suspend_equivalent;     // Suspend equivalent condition
  jint                  _in_deopt_handler;       // count of deoptimization
                                                 // handlers thread is in
  volatile bool         _doing_unsafe_access;    // Thread may fault due to unsafe access
  bool                  _do_not_unlock_if_synchronized;  // Do not unlock the receiver of a synchronized method (since it was
                                                         // never locked) when throwing an exception. Used by interpreter only.

  // JNI attach states:
  enum JNIAttachStates {
    _not_attaching_via_jni = 1,  // thread is not attaching via JNI
    _attaching_via_jni,          // thread is attaching via JNI
    _attached_via_jni            // thread has attached via JNI
  };

  // A regular JavaThread's _jni_attach_state is _not_attaching_via_jni.
  // A native thread that is attaching via JNI starts with a value
  // of _attaching_via_jni and transitions to _attached_via_jni.
  volatile JNIAttachStates _jni_attach_state;

 public:
  // State of the stack guard pages for this thread.
  enum StackGuardState {
    stack_guard_unused,         // not needed
    stack_guard_reserved_disabled,
    stack_guard_yellow_reserved_disabled,// disabled (temporarily) after stack overflow
    stack_guard_enabled         // enabled
  };

 private:
  // The _pending_* fields below are used to communicate extra information
  // from an uncommon trap in JVMCI compiled code to the uncommon trap handler.

  // Specifies whether the uncommon trap is to bci 0 of a synchronized method
  // before the monitor has been acquired.
  bool      _pending_monitorenter;

  // An id of a speculation that JVMCI compiled code can use to further describe and
  // uniquely identify the  speculative optimization guarded by the uncommon trap
  long       _pending_failed_speculation;

  // These fields are mutually exclusive in terms of live ranges.
  union {
    // Communicates the pc at which the most recent implicit exception occurred
    // from the signal handler to a deoptimization stub.
    address   _implicit_exception_pc;

    // Communicates an alternative call target to an i2c stub from a JavaCall .
    address   _alternate_call_target;
  } _jvmci;

 private:
  StackGuardState  _stack_guard_state;

  // Precompute the limit of the stack as used in stack overflow checks.
  // We load it from here to simplify the stack overflow check in assembly.
  address          _stack_overflow_limit;
  address          _reserved_stack_activation;

  // Compiler exception handling (NOTE: The _exception_oop is *NOT* the same as _pending_exception. It is
  // used to temp. parsing values into and out of the runtime system during exception handling for compiled
  // code)
  volatile oop     _exception_oop;               // Exception thrown in compiled code
  volatile address _exception_pc;                // PC where exception happened
  volatile address _exception_handler_pc;        // PC for handler of exception

 private:
  // support for JNI critical regions
  jint    _jni_active_critical;                  // count of entries into JNI critical region

  // Checked JNI: function name requires exception check
  char* _pending_jni_exception_check_fn;

  // For deadlock detection.
  int _depth_first_number;

  // JVMTI PopFrame support
  // This is set to popframe_pending to signal that top Java frame should be popped immediately
  int _popframe_condition;

  // If reallocation of scalar replaced objects fails, we throw OOM
  // and during exception propagation, pop the top
  // _frames_to_pop_failed_realloc frames, the ones that reference
  // failed reallocations.
  int _frames_to_pop_failed_realloc;

  friend class VMThread;
  friend class ThreadWaitTransition;
  friend class VM_Exit;

  void initialize();                             // Initialized the instance variables

 public:
  // Constructor
  JavaThread(bool is_attaching_via_jni = false); // for main thread and JNI attached threads
  JavaThread(ThreadFunction entry_point, size_t stack_size = 0);
  ~JavaThread();

  //JNI functiontable getter/setter for JVMTI jni function table interception API.
  void set_jni_functions(struct JNINativeInterface_* functionTable) {
    _jni_environment.functions = functionTable;
  }
  struct JNINativeInterface_* get_jni_functions() {
    return (struct JNINativeInterface_ *)_jni_environment.functions;
  }

  // This function is called at thread creation to allow
  // platform specific thread variables to be initialized.
  void cache_global_variables();

  // Executes Shutdown.shutdown()
  void invoke_shutdown_hooks();

  // Cleanup on thread exit
  enum ExitType {
    normal_exit,
    jni_detach
  };
  void exit(bool destroy_vm, ExitType exit_type = normal_exit);

  void cleanup_failed_attach_current_thread();

  // Testers
  virtual bool is_Java_thread()            const { return true; }
  virtual bool can_call_java()             const { return true; }

  // Thread chain operations
  JavaThread* next()                       const { return _next; }
  void set_next(JavaThread* p)                   { _next = p; }

  // Thread oop. threadObj() can be NULL for initial JavaThread
  // (or for threads attached via JNI)
  oop threadObj()                          const { return _threadObj; }
  void set_threadObj(oop p)                      { _threadObj = p; }

  ThreadPriority java_priority() const;          // Read from threadObj()

  // Prepare thread and add to priority queue.  If a priority is
  // not specified, use the priority of the thread object. Threads_lock
  // must be held while this function is called.
  void prepare(jobject jni_thread, ThreadPriority prio=NoPriority);

  void set_saved_exception_pc(address pc)        { _saved_exception_pc = pc; }
  address saved_exception_pc()                   { return _saved_exception_pc; }

  ThreadFunction entry_point()             const { return _entry_point; }

  // Allocates a new Java level thread object for this thread. thread_name may be NULL.
  void allocate_threadObj(Handle thread_group, const char* thread_name, bool daemon, TRAPS);

  // Last frame anchor routines

  JavaFrameAnchor* frame_anchor(void)            { return &_anchor; }

  // last_Java_sp
  bool has_last_Java_frame()               const { return _anchor.has_last_Java_frame(); }
  intptr_t* last_Java_sp()                 const { return _anchor.last_Java_sp(); }

  // last_Java_pc

  address last_Java_pc(void)                     { return _anchor.last_Java_pc(); }

  // Safepoint support
#if !defined(AARCH64)
  JavaThreadState thread_state()           const { return _thread_state; }
  void set_thread_state(JavaThreadState s)       { _thread_state = s; }
#else
  // Use membars when accessing volatile _thread_state. See
  // Threads::create_vm() for size checks.
  inline JavaThreadState thread_state() const;
  inline void set_thread_state(JavaThreadState s);
#endif
  ThreadSafepointState *safepoint_state()  const { return _safepoint_state; }
  void set_safepoint_state(ThreadSafepointState *state) { _safepoint_state = state; }
  bool is_at_poll_safepoint()                    { return _safepoint_state->is_at_poll_safepoint(); }

  // JavaThread termination and lifecycle support:
  void smr_delete();
  bool on_thread_list() const { return _on_thread_list; }
  void set_on_thread_list() { _on_thread_list = true; }

  // thread has called JavaThread::exit() or is terminated
  bool is_exiting() const;
  // thread is terminated (no longer on the threads list); we compare
  // against the two non-terminated values so that a freed JavaThread
  // will also be considered terminated.
  bool check_is_terminated(TerminatedTypes l_terminated) const {
    return l_terminated != _not_terminated && l_terminated != _thread_exiting;
  }
  bool is_terminated() const;
  void set_terminated(TerminatedTypes t);
  // special for Threads::remove() which is static:
  void set_terminated_value();
  void block_if_vm_exited();

  bool doing_unsafe_access()                     { return _doing_unsafe_access; }
  void set_doing_unsafe_access(bool val)         { _doing_unsafe_access = val; }

  bool do_not_unlock_if_synchronized()             { return _do_not_unlock_if_synchronized; }
  void set_do_not_unlock_if_synchronized(bool val) { _do_not_unlock_if_synchronized = val; }

  inline void set_polling_page_release(void* poll_value);
  inline void set_polling_page(void* poll_value);
  inline volatile void* get_polling_page();

 private:
  // Support for thread handshake operations
  HandshakeState _handshake;
 public:
  void set_handshake_operation(HandshakeOperation* op) {
    _handshake.set_operation(this, op);
  }

  bool has_handshake() const {
    return _handshake.has_operation();
  }

  void cancel_handshake() {
    _handshake.cancel(this);
  }

  void handshake_process_by_self() {
    _handshake.process_by_self(this);
  }

  void handshake_process_by_vmthread() {
    _handshake.process_by_vmthread(this);
  }

  // Suspend/resume support for JavaThread
 private:
  inline void set_ext_suspended();
  inline void clear_ext_suspended();

 public:
  void java_suspend();
  void java_resume();
  int  java_suspend_self();

  void check_and_wait_while_suspended() {
    bool do_self_suspend;
    do {
      // were we externally suspended while we were waiting?
      do_self_suspend = handle_special_suspend_equivalent_condition();
      if (do_self_suspend) {
        // don't surprise the thread that suspended us by returning
        java_suspend_self();
        set_suspend_equivalent();
      }
    } while (do_self_suspend);
  }
  static void check_safepoint_and_suspend_for_native_trans(JavaThread *thread);
  // Check for async exception in addition to safepoint and suspend request.
  static void check_special_condition_for_native_trans(JavaThread *thread);

  // Same as check_special_condition_for_native_trans but finishes the
  // transition into thread_in_Java mode so that it can potentially
  // block.
  static void check_special_condition_for_native_trans_and_transition(JavaThread *thread);

  bool is_ext_suspend_completed(bool called_by_wait, int delay, uint32_t *bits);
  bool is_ext_suspend_completed_with_lock(uint32_t *bits) {
    MutexLockerEx ml(SR_lock(), Mutex::_no_safepoint_check_flag);
    // Warning: is_ext_suspend_completed() may temporarily drop the
    // SR_lock to allow the thread to reach a stable thread state if
    // it is currently in a transient thread state.
    return is_ext_suspend_completed(false /* !called_by_wait */, SuspendRetryDelay, bits);
  }

  // We cannot allow wait_for_ext_suspend_completion() to run forever or
  // we could hang. SuspendRetryCount and SuspendRetryDelay are normally
  // passed as the count and delay parameters. Experiments with specific
  // calls to wait_for_ext_suspend_completion() can be done by passing
  // other values in the code. Experiments with all calls can be done
  // via the appropriate -XX options.
  bool wait_for_ext_suspend_completion(int count, int delay, uint32_t *bits);

  // test for suspend - most (all?) of these should go away
  bool is_thread_fully_suspended(bool wait_for_suspend, uint32_t *bits);

  inline void set_external_suspend();
  inline void clear_external_suspend();

  bool is_external_suspend() const {
    return (_suspend_flags & _external_suspend) != 0;
  }
  // Whenever a thread transitions from native to vm/java it must suspend
  // if external|deopt suspend is present.
  bool is_suspend_after_native() const {
    return (_suspend_flags & _external_suspend) != 0;
  }

  // external suspend request is completed
  bool is_ext_suspended() const {
    return (_suspend_flags & _ext_suspended) != 0;
  }

  bool is_external_suspend_with_lock() const {
    MutexLockerEx ml(SR_lock(), Mutex::_no_safepoint_check_flag);
    return is_external_suspend();
  }

  // Special method to handle a pending external suspend request
  // when a suspend equivalent condition lifts.
  bool handle_special_suspend_equivalent_condition() {
    MutexLockerEx ml(SR_lock(), Mutex::_no_safepoint_check_flag);
    bool ret = is_external_suspend();
    if (!ret) {
      // not about to self-suspend so clear suspend equivalence
      clear_suspend_equivalent();
    }
    // implied else:
    // We have a pending external suspend request so we leave the
    // suspend_equivalent flag set until java_suspend_self() sets
    // the ext_suspended flag and clears the suspend_equivalent
    // flag. This insures that wait_for_ext_suspend_completion()
    // will return consistent values.
    return ret;
  }

  // utility methods to see if we are doing some kind of suspension
  bool is_being_ext_suspended() const {
    MutexLockerEx ml(SR_lock(), Mutex::_no_safepoint_check_flag);
    return is_ext_suspended() || is_external_suspend();
  }

  bool is_suspend_equivalent()  const { return _suspend_equivalent; }

  void set_suspend_equivalent()       { _suspend_equivalent = true; }
  void clear_suspend_equivalent()     { _suspend_equivalent = false; }

  // Thread.stop support
  void send_thread_stop(oop throwable);
  AsyncRequests clear_special_runtime_exit_condition() {
    AsyncRequests x = _special_runtime_exit_condition;
    _special_runtime_exit_condition = _no_async_condition;
    return x;
  }

  // Are any async conditions present?
  bool has_async_condition() { return (_special_runtime_exit_condition != _no_async_condition); }

  void check_and_handle_async_exceptions(bool check_unsafe_error = true);

  // these next two are also used for self-suspension and async exception support
  void handle_special_runtime_exit_condition(bool check_asyncs = true);

  // Return true if JavaThread has an asynchronous condition or
  // if external suspension is requested.
  bool has_special_runtime_exit_condition() {
    // Because we don't use is_external_suspend_with_lock
    // it is possible that we won't see an asynchronous external suspend
    // request that has just gotten started, i.e., SR_lock grabbed but
    // _external_suspend field change either not made yet or not visible
    // yet. However, this is okay because the request is asynchronous and
    // we will see the new flag value the next time through. It's also
    // possible that the external suspend request is dropped after
    // we have checked is_external_suspend(), we will recheck its value
    // under SR_lock in java_suspend_self().
    return (_special_runtime_exit_condition != _no_async_condition) || is_external_suspend() || is_trace_suspend();
  }

  void set_pending_unsafe_access_error()          { _special_runtime_exit_condition = _async_unsafe_access_error; }

  inline void set_pending_async_exception(oop e);

  // Fast-locking support
  bool is_lock_owned(address adr) const;

  // Accessors for vframe array top
  // The linked list of vframe arrays are sorted on sp. This means when we
  // unpack the head must contain the vframe array to unpack.
  void set_vframe_array_head(vframeArray* value) { _vframe_array_head = value; }
  vframeArray* vframe_array_head()         const { return _vframe_array_head; }

  intptr_t* must_deopt_id()                      { return _must_deopt_id; }
  void     set_must_deopt_id(intptr_t* id)       { _must_deopt_id = id; }
  void     clear_must_deopt_id()                 { _must_deopt_id = NULL; }

  void set_deopt_compiled_method(CompiledMethod* nm)  { _deopt_nmethod = nm; }
  CompiledMethod* deopt_compiled_method()        { return _deopt_nmethod; }

  Method*    callee_target()               const { return _callee_target; }
  void set_callee_target  (Method* x)          { _callee_target   = x; }

  // Oop results of vm runtime calls
  oop  vm_result()                         const { return _vm_result; }
  void set_vm_result  (oop x)                    { _vm_result   = x; }

  Metadata*    vm_result_2()               const { return _vm_result_2; }
  void set_vm_result_2  (Metadata* x)          { _vm_result_2   = x; }

  MemRegion deferred_card_mark()           const { return _deferred_card_mark; }
  void set_deferred_card_mark(MemRegion mr)      { _deferred_card_mark = mr; }

  long pending_failed_speculation()         const { return _pending_failed_speculation; }
  bool has_pending_monitorenter()           const { return _pending_monitorenter; }
  void set_pending_monitorenter(bool b)           { _pending_monitorenter = b; }
  void set_pending_failed_speculation(long failed_speculation) { _pending_failed_speculation = failed_speculation; }
  void set_jvmci_alternate_call_target(address a) { _jvmci._alternate_call_target = a; }
  void set_jvmci_implicit_exception_pc(address a) { _jvmci._implicit_exception_pc = a; }

  // Exception handling for compiled methods
  oop      exception_oop()                 const { return _exception_oop; }
  address  exception_pc()                  const { return _exception_pc; }
  address  exception_handler_pc()          const { return _exception_handler_pc; }

  void set_exception_oop(oop o)                  { (void)const_cast<oop&>(_exception_oop = o); }
  void set_exception_pc(address a)               { _exception_pc = a; }
  void set_exception_handler_pc(address a)       { _exception_handler_pc = a; }

  void clear_exception_oop_and_pc() {
    set_exception_oop(NULL);
    set_exception_pc(NULL);
  }

  // Stack overflow support
  //
  //  (small addresses)
  //
  //  --  <-- stack_end()                   ---
  //  |                                      |
  //  |  red pages                           |
  //  |                                      |
  //  --  <-- stack_red_zone_base()          |
  //  |                                      |
  //  |                                     guard
  //  |  yellow pages                       zone
  //  |                                      |
  //  |                                      |
  //  --  <-- stack_yellow_zone_base()       |
  //  |                                      |
  //  |                                      |
  //  |  reserved pages                      |
  //  |                                      |
  //  --  <-- stack_reserved_zone_base()    ---      ---
  //                                                 /|\  shadow     <--  stack_overflow_limit() (somewhere in here)
  //                                                  |   zone
  //                                                 \|/  size
  //  some untouched memory                          ---
  //
  //
  //  --
  //  |
  //  |  shadow zone
  //  |
  //  --
  //  x    frame n
  //  --
  //  x    frame n-1
  //  x
  //  --
  //  ...
  //
  //  --
  //  x    frame 0
  //  --  <-- stack_base()
  //
  //  (large addresses)
  //

 private:
  // These values are derived from flags StackRedPages, StackYellowPages and StackShadowPages.
  // The zone size is determined ergonomically if page_size > 4K.
  static size_t _stack_red_zone_size;
  static size_t _stack_yellow_zone_size;
  static size_t _stack_reserved_zone_size;
  static size_t _stack_shadow_zone_size;
 public:
  inline size_t stack_available(address cur_sp);

  static size_t stack_red_zone_size() {
    return _stack_red_zone_size;
  }
  static void set_stack_red_zone_size(size_t s) {
    _stack_red_zone_size = s;
  }
  address stack_red_zone_base() {
    return (address)(stack_end() + stack_red_zone_size());
  }
  bool in_stack_red_zone(address a) {
    return a <= stack_red_zone_base() && a >= stack_end();
  }

  static size_t stack_yellow_zone_size() {
    return _stack_yellow_zone_size;
  }
  static void set_stack_yellow_zone_size(size_t s) {
    _stack_yellow_zone_size = s;
  }

  static size_t stack_reserved_zone_size() {
    // _stack_reserved_zone_size may be 0. This indicates the feature is off.
    return _stack_reserved_zone_size;
  }
  address stack_reserved_zone_base() {
    return (address)(stack_end() + (stack_red_zone_size() + stack_yellow_zone_size() + stack_reserved_zone_size()));
  }
  bool in_stack_reserved_zone(address a) {
    return (a <= stack_reserved_zone_base()) && (a >= (address)((intptr_t)stack_reserved_zone_base() - stack_reserved_zone_size()));
  }

  static size_t stack_yellow_reserved_zone_size() {
    return _stack_yellow_zone_size + _stack_reserved_zone_size;
  }
  bool in_stack_yellow_reserved_zone(address a) {
    return (a <= stack_reserved_zone_base()) && (a >= stack_red_zone_base());
  }

  // Size of red + yellow + reserved zones.
  static size_t stack_guard_zone_size() {
    return stack_red_zone_size() + stack_yellow_reserved_zone_size();
  }

  static size_t stack_shadow_zone_size() {
    return _stack_shadow_zone_size;
  }
  static void set_stack_shadow_zone_size(size_t s) {
    // The shadow area is not allocated or protected, so
    // it needs not be page aligned.
    // But the stack bang currently assumes that it is a
    // multiple of page size. This guarantees that the bang
    // loop touches all pages in the shadow zone.
    // This can be guaranteed differently, as well.  E.g., if
    // the page size is a multiple of 4K, banging in 4K steps
    // suffices to touch all pages. (Some pages are banged
    // several times, though.)
    _stack_shadow_zone_size = s;
  }

  void create_stack_guard_pages();
  void remove_stack_guard_pages();

  void enable_stack_reserved_zone();
  void disable_stack_reserved_zone();
  void enable_stack_yellow_reserved_zone();
  void disable_stack_yellow_reserved_zone();
  void enable_stack_red_zone();
  void disable_stack_red_zone();

  inline bool stack_guard_zone_unused();
  inline bool stack_yellow_reserved_zone_disabled();
  inline bool stack_reserved_zone_disabled();
  inline bool stack_guards_enabled();

  address reserved_stack_activation() const { return _reserved_stack_activation; }
  void set_reserved_stack_activation(address addr) { _reserved_stack_activation = addr; }

  // Attempt to reguard the stack after a stack overflow may have occurred.
  // Returns true if (a) guard pages are not needed on this thread, (b) the
  // pages are already guarded, or (c) the pages were successfully reguarded.
  // Returns false if there is not enough stack space to reguard the pages, in
  // which case the caller should unwind a frame and try again.  The argument
  // should be the caller's (approximate) sp.
  bool reguard_stack(address cur_sp);
  // Similar to above but see if current stackpoint is out of the guard area
  // and reguard if possible.
  bool reguard_stack(void);

  address stack_overflow_limit() { return _stack_overflow_limit; }
  void set_stack_overflow_limit() {
    _stack_overflow_limit = stack_end() + MAX2(JavaThread::stack_guard_zone_size(), JavaThread::stack_shadow_zone_size());
  }

  // Misc. accessors/mutators
  void set_do_not_unlock(void)                   { _do_not_unlock_if_synchronized = true; }
  void clr_do_not_unlock(void)                   { _do_not_unlock_if_synchronized = false; }
  bool do_not_unlock(void)                       { return _do_not_unlock_if_synchronized; }

  // For assembly stub generation
  static ByteSize threadObj_offset()             { return byte_offset_of(JavaThread, _threadObj); }
  static ByteSize jni_environment_offset()       { return byte_offset_of(JavaThread, _jni_environment); }
  static ByteSize pending_jni_exception_check_fn_offset() {
    return byte_offset_of(JavaThread, _pending_jni_exception_check_fn);
  }
  static ByteSize last_Java_sp_offset() {
    return byte_offset_of(JavaThread, _anchor) + JavaFrameAnchor::last_Java_sp_offset();
  }
  static ByteSize last_Java_pc_offset() {
    return byte_offset_of(JavaThread, _anchor) + JavaFrameAnchor::last_Java_pc_offset();
  }
  static ByteSize frame_anchor_offset() {
    return byte_offset_of(JavaThread, _anchor);
  }
  static ByteSize callee_target_offset()         { return byte_offset_of(JavaThread, _callee_target); }
  static ByteSize vm_result_offset()             { return byte_offset_of(JavaThread, _vm_result); }
  static ByteSize vm_result_2_offset()           { return byte_offset_of(JavaThread, _vm_result_2); }
  static ByteSize thread_state_offset()          { return byte_offset_of(JavaThread, _thread_state); }
  static ByteSize saved_exception_pc_offset()    { return byte_offset_of(JavaThread, _saved_exception_pc); }
  static ByteSize osthread_offset()              { return byte_offset_of(JavaThread, _osthread); }
  static ByteSize pending_monitorenter_offset()  { return byte_offset_of(JavaThread, _pending_monitorenter); }
  static ByteSize pending_failed_speculation_offset() { return byte_offset_of(JavaThread, _pending_failed_speculation); }
  static ByteSize jvmci_alternate_call_target_offset() { return byte_offset_of(JavaThread, _jvmci._alternate_call_target); }
  static ByteSize jvmci_implicit_exception_pc_offset() { return byte_offset_of(JavaThread, _jvmci._implicit_exception_pc); }
  static ByteSize exception_oop_offset()         { return byte_offset_of(JavaThread, _exception_oop); }
  static ByteSize exception_pc_offset()          { return byte_offset_of(JavaThread, _exception_pc); }
  static ByteSize exception_handler_pc_offset()  { return byte_offset_of(JavaThread, _exception_handler_pc); }
  static ByteSize stack_overflow_limit_offset()  { return byte_offset_of(JavaThread, _stack_overflow_limit); }
  static ByteSize stack_guard_state_offset()     { return byte_offset_of(JavaThread, _stack_guard_state); }
  static ByteSize reserved_stack_activation_offset() { return byte_offset_of(JavaThread, _reserved_stack_activation); }
  static ByteSize suspend_flags_offset()         { return byte_offset_of(JavaThread, _suspend_flags); }

  static ByteSize do_not_unlock_if_synchronized_offset() { return byte_offset_of(JavaThread, _do_not_unlock_if_synchronized); }
  static ByteSize should_post_on_exceptions_flag_offset() {
    return byte_offset_of(JavaThread, _should_post_on_exceptions_flag);
  }

  // Returns the jni environment for this thread
  JNIEnv* jni_environment()                      { return &_jni_environment; }

  static JavaThread* thread_from_jni_environment(JNIEnv* env) {
    JavaThread *thread_from_jni_env = (JavaThread*)((intptr_t)env - in_bytes(jni_environment_offset()));
    // Only return NULL if thread is off the thread list; starting to
    // exit should not return NULL.
    if (thread_from_jni_env->is_terminated()) {
      thread_from_jni_env->block_if_vm_exited();
      return NULL;
    } else {
      return thread_from_jni_env;
    }
  }

  // JNI critical regions. These can nest.
  bool in_critical()    { return _jni_active_critical > 0; }
  bool in_last_critical()  { return _jni_active_critical == 1; }
  void enter_critical() {
    _jni_active_critical++;
  }
  void exit_critical() {
    _jni_active_critical--;
  }

  // Checked JNI: is the programmer required to check for exceptions, if so specify
  // which function name. Returning to a Java frame should implicitly clear the
  // pending check, this is done for Native->Java transitions (i.e. user JNI code).
  // VM->Java transistions are not cleared, it is expected that JNI code enclosed
  // within ThreadToNativeFromVM makes proper exception checks (i.e. VM internal).
  bool is_pending_jni_exception_check() const { return _pending_jni_exception_check_fn != NULL; }
  void clear_pending_jni_exception_check() { _pending_jni_exception_check_fn = NULL; }
  const char* get_pending_jni_exception_check() const { return _pending_jni_exception_check_fn; }
  void set_pending_jni_exception_check(const char* fn_name) { _pending_jni_exception_check_fn = (char*) fn_name; }

  // For deadlock detection
  int depth_first_number() { return _depth_first_number; }
  void set_depth_first_number(int dfn) { _depth_first_number = dfn; }

 private:
  void set_monitor_chunks(MonitorChunk* monitor_chunks) { _monitor_chunks = monitor_chunks; }

 public:
  MonitorChunk* monitor_chunks()           const { return _monitor_chunks; }
  void add_monitor_chunk(MonitorChunk* chunk);
  void remove_monitor_chunk(MonitorChunk* chunk);
  bool in_deopt_handler()                  const { return _in_deopt_handler > 0; }
  void inc_in_deopt_handler()                    { _in_deopt_handler++; }
  void dec_in_deopt_handler() {
    if (_in_deopt_handler > 0) { // robustness
      _in_deopt_handler--;
    }
  }

 private:
  void set_entry_point(ThreadFunction entry_point) { _entry_point = entry_point; }

 public:
  // Frame iteration; calls the function f for all frames on the stack
  void frames_do(void f(frame*, const RegisterMap*));

  // Memory operations
  void oops_do(OopClosure* f, CodeBlobClosure* cf);

  // Sweeper operations
  virtual void nmethods_do(CodeBlobClosure* cf);

  // RedefineClasses Support
  void metadata_do(void f(Metadata*));

  // Misc. operations
  char* name() const { return (char*)get_thread_name(); }
  void print_on(outputStream* st, bool print_extended_info) const;
  void print_on(outputStream* st) const { print_on(st, false); }
  void print_value();
  void print_thread_state_on(outputStream*)      const { };
  void print_thread_state()                      const { };
  void print_on_error(outputStream* st, char* buf, int buflen) const;
  void print_name_on_error(outputStream* st, char* buf, int buflen) const;
  const char* get_thread_name() const;
 private:
  // factor out low-level mechanics for use in both normal and error cases
  const char* get_thread_name_string(char* buf = NULL, int buflen = 0) const;
 public:
  const char* get_threadgroup_name() const;
  const char* get_parent_name() const;

  // Accessing frames
  frame last_frame() {
    _anchor.make_walkable(this);
    return pd_last_frame();
  }
  javaVFrame* last_java_vframe(RegisterMap* reg_map);

  // Returns method at 'depth' java or native frames down the stack
  // Used for security checks
  Klass* security_get_caller_class(int depth);

  // Print stack trace in external format
  void print_stack_on(outputStream* st);
  void print_stack() { print_stack_on(tty); }

  // Print stack traces in various internal formats
  void trace_stack()                             { };
  void trace_stack_from(vframe* start_vf)        { };
  void trace_frames()                            { };
  void trace_oops()                              { };

  // Print an annotated view of the stack frames
  void print_frame_layout(int depth = 0, bool validate_only = false) { };
  void validate_frame_layout() {
    print_frame_layout(0, true);
  }

  // Returns the number of stack frames on the stack
  int depth() const;

 public:
  // Returns the running thread as a JavaThread
  static inline JavaThread* current();

  // Returns the active Java thread.  Do not use this if you know you are calling
  // from a JavaThread, as it's slower than JavaThread::current.  If called from
  // the VMThread, it also returns the JavaThread that instigated the VMThread's
  // operation.  You may not want that either.
  static JavaThread* active();

  inline CompilerThread* as_CompilerThread();

 public:
  virtual void run();
  void thread_main_inner();

 private:
  // PRIVILEGED STACK
  PrivilegedElement*  _privileged_stack_top;
  GrowableArray<oop>* _array_for_gc;
 public:
  // Returns the privileged_stack information.
  PrivilegedElement* privileged_stack_top()       const { return _privileged_stack_top; }
  void set_privileged_stack_top(PrivilegedElement *e)   { _privileged_stack_top = e; }
  void register_array_for_gc(GrowableArray<oop>* array) { _array_for_gc = array; }

 public:
  // JVMTI PopFrame support
  // Setting and clearing popframe_condition
  // All of these enumerated values are bits. popframe_pending
  // indicates that a PopFrame() has been requested and not yet been
  // completed. popframe_processing indicates that that PopFrame() is in
  // the process of being completed. popframe_force_deopt_reexecution_bit
  // indicates that special handling is required when returning to a
  // deoptimized caller.
  enum PopCondition {
    popframe_inactive                      = 0x00,
    popframe_pending_bit                   = 0x01,
    popframe_processing_bit                = 0x02,
    popframe_force_deopt_reexecution_bit   = 0x04
  };
  PopCondition popframe_condition()                   { return (PopCondition) _popframe_condition; }
  void set_popframe_condition(PopCondition c)         { _popframe_condition = c; }
  void set_popframe_condition_bit(PopCondition c)     { _popframe_condition |= c; }
  void clear_popframe_condition()                     { _popframe_condition = popframe_inactive; }
  static ByteSize popframe_condition_offset()         { return byte_offset_of(JavaThread, _popframe_condition); }
  bool has_pending_popframe()                         { return (popframe_condition() & popframe_pending_bit) != 0; }
  bool popframe_forcing_deopt_reexecution()           { return (popframe_condition() & popframe_force_deopt_reexecution_bit) != 0; }
  void clear_popframe_forcing_deopt_reexecution()     { _popframe_condition &= ~popframe_force_deopt_reexecution_bit; }

  int frames_to_pop_failed_realloc()            const { return _frames_to_pop_failed_realloc; }
  void set_frames_to_pop_failed_realloc(int nb)       { _frames_to_pop_failed_realloc = nb; }
  void dec_frames_to_pop_failed_realloc()             { _frames_to_pop_failed_realloc--; }

 private:
  // Saved incoming arguments to popped frame.
  // Used only when popped interpreted frame returns to deoptimized frame.
  void*    _popframe_preserved_args;
  int      _popframe_preserved_args_size;

 public:
  void  popframe_preserve_args(ByteSize size_in_bytes, void* start);
  void* popframe_preserved_args();
  ByteSize popframe_preserved_args_size();
  WordSize popframe_preserved_args_size_in_words();
  void  popframe_free_preserved_args();

 private:
  // Used by the interpreter in fullspeed mode for frame pop, method
  // entry, method exit and single stepping support. This field is
  // only set to non-zero by the VM_EnterInterpOnlyMode VM operation.
  // It can be set to zero asynchronously (i.e., without a VM operation
  // or a lock) so we have to be very careful.
  int               _interp_only_mode;

 public:
  // used by the interpreter for fullspeed debugging support (see above)
  static ByteSize interp_only_mode_offset() { return byte_offset_of(JavaThread, _interp_only_mode); }
  bool is_interp_only_mode()                { return (_interp_only_mode != 0); }
  int get_interp_only_mode()                { return _interp_only_mode; }
  void increment_interp_only_mode()         { ++_interp_only_mode; }
  void decrement_interp_only_mode()         { --_interp_only_mode; }

  // support for cached flag that indicates whether exceptions need to be posted for this thread
  // if this is false, we can avoid deoptimizing when events are thrown
  // this gets set to reflect whether jvmtiExport::post_exception_throw would actually do anything
 private:
  int    _should_post_on_exceptions_flag;

 public:
  int   should_post_on_exceptions_flag()  { return _should_post_on_exceptions_flag; }
  void  set_should_post_on_exceptions_flag(int val)  { _should_post_on_exceptions_flag = val; }

 private:
  ThreadStatistics *_thread_stat;

 public:
  ThreadStatistics* get_thread_stat()    const { return _thread_stat; }

  // Return a blocker object for which this thread is blocked parking.
  oop current_park_blocker();

 private:
  static size_t _stack_size_at_create;

 public:
  static inline size_t stack_size_at_create(void) {
    return _stack_size_at_create;
  }
  static inline void set_stack_size_at_create(size_t value) {
    _stack_size_at_create = value;
  }

  // Machine dependent stuff
#include OS_CPU_HEADER(thread)

 public:
  void set_blocked_on_compilation(bool value) {
    _blocked_on_compilation = value;
  }

  bool blocked_on_compilation() {
    return _blocked_on_compilation;
  }
 protected:
  bool         _blocked_on_compilation;

  // JSR166 per-thread parker
 private:
  Parker*    _parker;
 public:
  Parker*     parker() { return _parker; }

  // Biased locking support
 private:
  GrowableArray<MonitorInfo*>* _cached_monitor_info;
 public:
  GrowableArray<MonitorInfo*>* cached_monitor_info() { return _cached_monitor_info; }
  void set_cached_monitor_info(GrowableArray<MonitorInfo*>* info) { _cached_monitor_info = info; }

  // clearing/querying jni attach status
  bool is_attaching_via_jni() const { return _jni_attach_state == _attaching_via_jni; }
  bool has_attached_via_jni() const { return is_attaching_via_jni() || _jni_attach_state == _attached_via_jni; }
  inline void set_done_attaching_via_jni();
};

// Inline implementation of JavaThread::current
inline JavaThread* JavaThread::current() {
  Thread* thread = Thread::current();
  return (JavaThread*)thread;
}

inline CompilerThread* JavaThread::as_CompilerThread() {
  return (CompilerThread*)this;
}

// Dedicated thread to sweep the code cache
class CodeCacheSweeperThread : public JavaThread {
  CompiledMethod*       _scanned_compiled_method; // nmethod being scanned by the sweeper
 public:
  CodeCacheSweeperThread();
  // Track the nmethod currently being scanned by the sweeper
  void set_scanned_compiled_method(CompiledMethod* cm) {
    _scanned_compiled_method = cm;
  }

  // Hide sweeper thread from external view.
  bool is_hidden_from_external_view() const { return true; }

  bool is_Code_cache_sweeper_thread() const { return true; }

  // Prevent GC from unloading _scanned_compiled_method
  void oops_do(OopClosure* f, CodeBlobClosure* cf);
  void nmethods_do(CodeBlobClosure* cf);
};

// A thread used for Compilation.
class CompilerThread : public JavaThread {
  friend class VMStructs;
 private:
  CompilerCounters* _counters;

  ciEnv*                _env;
  CompileTask* volatile _task;  // print_threads_compiling can read this concurrently.
  CompileQueue*         _queue;
  BufferBlob*           _buffer_blob;

  AbstractCompiler*     _compiler;
  TimeStamp             _idle_time;

 public:
  static CompilerThread* current();

  CompilerThread(CompileQueue* queue, CompilerCounters* counters);
  ~CompilerThread();

  bool is_Compiler_thread()                const { return true; }

  virtual bool can_call_java() const;

  // Hide native compiler threads from external view.
  bool is_hidden_from_external_view()      const { return !can_call_java(); }

  void set_compiler(AbstractCompiler* c)         { _compiler = c; }
  AbstractCompiler* compiler()             const { return _compiler; }

  CompileQueue* queue()                    const { return _queue; }
  CompilerCounters* counters()             const { return _counters; }

  // Get/set the thread's compilation environment.
  ciEnv*        env()                            { return _env; }
  void          set_env(ciEnv* env)              { _env = env; }

  BufferBlob*   get_buffer_blob()          const { return _buffer_blob; }
  void          set_buffer_blob(BufferBlob* b)   { _buffer_blob = b; }

  void start_idle_timer()                        { _idle_time.update(); }
  jlong idle_time_millis() {
    return TimeHelper::counter_to_millis(_idle_time.ticks_since_update());
  }

  // Get/set the thread's current task
  CompileTask* task()                      { return _task; }
  void         set_task(CompileTask* task) { _task = task; }
};

inline CompilerThread* CompilerThread::current() {
  return JavaThread::current()->as_CompilerThread();
}

// The active thread queue. It also keeps track of the current used
// thread priorities.
class Threads: AllStatic {
  friend class VMStructs;
 private:
  static JavaThread* _thread_list;
  static int         _number_of_threads;
  static int         _number_of_non_daemon_threads;
  static int         _return_code;
  static int         _thread_claim_parity;

  static void initialize_java_lang_classes(JavaThread* main_thread, TRAPS);

 public:
  // Thread management
  // force_daemon is a concession to JNI, where we may need to add a
  // thread to the thread list before allocating its thread object
  static void add(JavaThread* p, bool force_daemon = false);
  static void remove(JavaThread* p);
  static void non_java_threads_do(ThreadClosure* tc);
  static void java_threads_do(ThreadClosure* tc);
  static void java_threads_and_vm_thread_do(ThreadClosure* tc);
  static void threads_do(ThreadClosure* tc);
  static void possibly_parallel_threads_do(bool is_par, ThreadClosure* tc);

  // Initializes the vm and creates the vm thread
  static jint create_vm(JavaVMInitArgs* args, bool* canTryAgain);
  static void convert_vm_init_libraries_to_agents();
  static void create_vm_init_libraries();
  static void create_vm_init_agents();
  static void shutdown_vm_agents();
  static bool destroy_vm();
  // Supported VM versions via JNI
  // Includes JNI_VERSION_1_1
  static jboolean is_supported_jni_version_including_1_1(jint version);
  // Does not include JNI_VERSION_1_1
  static jboolean is_supported_jni_version(jint version);

  // The "thread claim parity" provides a way for threads to be claimed
  // by parallel worker tasks.
  //
  // Each thread contains a a "parity" field. A task will claim the
  // thread only if its parity field is the same as the global parity,
  // which is updated by calling change_thread_claim_parity().
  //
  // For this to work change_thread_claim_parity() needs to be called
  // exactly once in sequential code before starting parallel tasks
  // that should claim threads.
  //
  // New threads get their parity set to 0 and change_thread_claim_parity()
  // never set the global parity to 0.
  static int thread_claim_parity() { return _thread_claim_parity; }
  static void change_thread_claim_parity();

  // Apply "f->do_oop" to all root oops in all threads.
  // This version may only be called by sequential code.
  static void oops_do(OopClosure* f, CodeBlobClosure* cf);
  // This version may be called by sequential or parallel code.
  static void possibly_parallel_oops_do(bool is_par, OopClosure* f, CodeBlobClosure* cf);

  // Apply "f->do_oop" to roots in all threads that
  // are part of compiled frames
  static void compiled_frame_oops_do(OopClosure* f, CodeBlobClosure* cf);

  static void convert_hcode_pointers();
  static void restore_hcode_pointers();

  // Sweeper
  static void nmethods_do(CodeBlobClosure* cf);

  // RedefineClasses support
  static void metadata_do(void f(Metadata*));
  static void metadata_handles_do(void f(Metadata*));

  // Verification
  static void print_on(outputStream* st, bool print_stacks, bool internal_format, bool print_concurrent_locks, bool print_extended_info);
  static void print(bool print_stacks, bool internal_format) {
    // this function is only used by debug.cpp
    print_on(tty, print_stacks, internal_format, false /* no concurrent lock printed */, false /* simple format */);
  }
  static void print_on_error(outputStream* st, Thread* current, char* buf, int buflen);
  static void print_on_error(Thread* this_thread, outputStream* st, Thread* current, char* buf, int buflen, bool* found_current);
  static void print_threads_compiling(outputStream* st, char* buf, int buflen);

  // Get Java threads that are waiting to enter a monitor.
  static GrowableArray<JavaThread*>* get_pending_threads(ThreadsList * t_list, int count, address monitor);

  // Get owning Java thread from the monitor's owner field.
  static JavaThread *owning_thread_from_monitor_owner(ThreadsList * t_list, address owner);

  // Number of threads on the active threads list
  static int number_of_threads()                 { return _number_of_threads; }
  // Number of non-daemon threads on the active threads list
  static int number_of_non_daemon_threads()      { return _number_of_non_daemon_threads; }
};

// Thread iterator
class ThreadClosure: public StackObj {
 public:
  virtual void do_thread(Thread* thread) = 0;
};

class SignalHandlerMark: public StackObj {
 private:
  Thread* _thread;
 public:
  SignalHandlerMark(Thread* t) {
    _thread = t;
    if (_thread) _thread->enter_signal_handler();
  }
  ~SignalHandlerMark() {
    if (_thread) _thread->leave_signal_handler();
    _thread = NULL;
  }
};

#endif
