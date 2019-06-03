#ifndef SHARE_VM_RUNTIME_SERVICETHREAD_HPP
#define SHARE_VM_RUNTIME_SERVICETHREAD_HPP

#include "runtime/thread.hpp"

// A JavaThread for low memory detection support and JVMTI
// compiled-method-load events.
class ServiceThread : public JavaThread {
  friend class VMStructs;
 private:
  static ServiceThread* _instance;

  static void service_thread_entry(JavaThread* thread, TRAPS);
  ServiceThread(ThreadFunction entry_point) : JavaThread(entry_point) { };

 public:
  static void initialize();

  // Hide this thread from external view.
  bool is_hidden_from_external_view() const      { return true; }

  // Returns true if the passed thread is the service thread.
  static bool is_service_thread(Thread* thread);
};

#endif
