#ifndef SHARE_VM_GC_SHARED_CONCURRENTGCTHREAD_HPP
#define SHARE_VM_GC_SHARED_CONCURRENTGCTHREAD_HPP

#include "runtime/thread.hpp"
#include "utilities/macros.hpp"

class ConcurrentGCThread: public NamedThread {
  friend class VMStructs;

  bool volatile _should_terminate;
  bool _has_terminated;

  // Do initialization steps in the thread: record stack base and size,
  // init thread local storage, set JNI handle block.
  void initialize_in_thread();

  // Wait until Universe::is_fully_initialized();
  void wait_for_universe_init();

  // Record that the current thread is terminating, and will do more
  // concurrent work.
  void terminate();

protected:
  // Create and start the thread (setting it's priority.)
  void create_and_start(ThreadPriority prio = NearMaxPriority);

  // Do the specific GC work. Called by run() after initialization complete.
  virtual void run_service() = 0;

  // Shut down the specific GC work. Called by stop() as part of termination protocol.
  virtual void stop_service()  = 0;

public:
  ConcurrentGCThread();

  // Tester
  bool is_ConcurrentGC_thread() const { return true; }

  virtual void run();

  // shutdown following termination protocol
  virtual void stop();

  bool should_terminate() { return _should_terminate; }
  bool has_terminated()   { return _has_terminated; }
};

#endif
