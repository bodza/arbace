#ifndef SHARE_VM_RUNTIME_HANDSHAKE_HPP
#define SHARE_VM_RUNTIME_HANDSHAKE_HPP

#include "memory/allocation.hpp"
#include "runtime/flags/flagSetting.hpp"
#include "runtime/semaphore.hpp"

class ThreadClosure;
class JavaThread;

// A handshake operation is a callback that is executed for each JavaThread
// while that thread is in a safepoint safe state. The callback is executed
// either by the thread itself or by the VM thread while keeping the thread
// in a blocked state. A handshake can be performed with a single
// JavaThread as well.
class Handshake : public AllStatic {
 public:
  // Execution of handshake operation
  static void execute(ThreadClosure* thread_cl);
  static bool execute(ThreadClosure* thread_cl, JavaThread* target);
};

class HandshakeOperation;

// The HandshakeState keep tracks of an ongoing handshake for one JavaThread.
// VM thread and JavaThread are serialized with the semaphore making sure
// the operation is only done by either VM thread on behalf of the JavaThread
// or the JavaThread itself.
class HandshakeState {
  HandshakeOperation* volatile _operation;

  Semaphore _semaphore;
  bool _thread_in_process_handshake;

  bool claim_handshake_for_vmthread();
  bool vmthread_can_process_handshake(JavaThread* target);

  void clear_handshake(JavaThread* thread);
  void cancel_inner(JavaThread* thread);

  void process_self_inner(JavaThread* thread);
public:
  HandshakeState();

  void set_operation(JavaThread* thread, HandshakeOperation* op);

  bool has_operation() const {
    return _operation != NULL;
  }

  void cancel(JavaThread* thread) {
    if (!_thread_in_process_handshake) {
      FlagSetting fs(_thread_in_process_handshake, true);
      cancel_inner(thread);
    }
  }

  void process_by_self(JavaThread* thread) {
    if (!_thread_in_process_handshake) {
      FlagSetting fs(_thread_in_process_handshake, true);
      process_self_inner(thread);
    }
  }
  void process_by_vmthread(JavaThread* target);
};

#endif
