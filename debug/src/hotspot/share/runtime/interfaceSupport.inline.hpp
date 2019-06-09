#ifndef SHARE_VM_RUNTIME_INTERFACESUPPORT_INLINE_HPP
#define SHARE_VM_RUNTIME_INTERFACESUPPORT_INLINE_HPP

#include "runtime/handles.inline.hpp"
#include "runtime/mutexLocker.hpp"
#include "runtime/orderAccess.hpp"
#include "runtime/os.hpp"
#include "runtime/safepointMechanism.inline.hpp"
#include "runtime/safepointVerifiers.hpp"
#include "runtime/thread.hpp"
#include "runtime/vm_operations.hpp"
#include "utilities/globalDefinitions.hpp"
#include "utilities/macros.hpp"
#include "utilities/preserveException.hpp"

// Wrapper for all entry points to the virtual machine.

// InterfaceSupport provides functionality used by the
// VM_ENTRY_BASE macro. These macros are used to guard entry points into
// the VM and perform checks upon leave of the VM.

class InterfaceSupport: AllStatic {
 public:
  static void serialize_thread_state_with_handler(JavaThread* thread) {
    serialize_thread_state_internal(thread, true);
  }

  // Should only call this if we know that we have a proper SEH set up.
  static void serialize_thread_state(JavaThread* thread) {
    serialize_thread_state_internal(thread, false);
  }

 private:
  static void serialize_thread_state_internal(JavaThread* thread, bool needs_exception_handler) {
    // Make sure new state is seen by VM thread
    if (os::is_MP()) {
      if (UseMembar) {
        // Force a fence between the write above and read below
        OrderAccess::fence();
      } else {
        // store to serialize page so VM thread can do pseudo remote membar
        if (needs_exception_handler) {
          os::write_memory_serialize_page_with_handler(thread);
        } else {
          os::write_memory_serialize_page(thread);
        }
      }
    }
  }
};

// Basic class for all thread transition classes.

class ThreadStateTransition : public StackObj {
 protected:
  JavaThread* _thread;
 public:
  ThreadStateTransition(JavaThread *thread) {
    _thread = thread;
  }

  // Change threadstate in a manner, so safepoint can detect changes.
  // Time-critical: called on exit from every runtime routine
  static inline void transition(JavaThread *thread, JavaThreadState from, JavaThreadState to) {
    // Change to transition state
    thread->set_thread_state((JavaThreadState)(from + 1));

    InterfaceSupport::serialize_thread_state(thread);

    SafepointMechanism::block_if_requested(thread);
    thread->set_thread_state(to);
  }

  // transition_and_fence must be used on any thread state transition
  // where there might not be a Java call stub on the stack, in
  // particular on Windows where the Structured Exception Handler is
  // set up in the call stub. os::write_memory_serialize_page() can
  // fault and we can't recover from it on Windows without a SEH in
  // place.
  static inline void transition_and_fence(JavaThread *thread, JavaThreadState from, JavaThreadState to) {
    // Change to transition state
    thread->set_thread_state((JavaThreadState)(from + 1));

    InterfaceSupport::serialize_thread_state_with_handler(thread);

    SafepointMechanism::block_if_requested(thread);
    thread->set_thread_state(to);
  }

  // Same as above, but assumes from = _thread_in_Java. This is simpler, since we
  // never block on entry to the VM. This will break the code, since e.g. preserve arguments
  // have not been setup.
  static inline void transition_from_java(JavaThread *thread, JavaThreadState to) {
    thread->set_thread_state(to);
  }

  static inline void transition_from_native(JavaThread *thread, JavaThreadState to) {
    // Change to transition state
    thread->set_thread_state(_thread_in_native_trans);

    InterfaceSupport::serialize_thread_state_with_handler(thread);

    // We never install asynchronous exceptions when coming (back) in
    // to the runtime from native code because the runtime is not set
    // up to handle exceptions floating around at arbitrary points.
    if (SafepointMechanism::poll(thread) || thread->is_suspend_after_native()) {
      JavaThread::check_safepoint_and_suspend_for_native_trans(thread);
    }

    thread->set_thread_state(to);
  }
 protected:
   void trans(JavaThreadState from, JavaThreadState to)  { transition(_thread, from, to); }
   void trans_from_java(JavaThreadState to)              { transition_from_java(_thread, to); }
   void trans_from_native(JavaThreadState to)            { transition_from_native(_thread, to); }
   void trans_and_fence(JavaThreadState from, JavaThreadState to) { transition_and_fence(_thread, from, to); }
};

class ThreadInVMForHandshake : public ThreadStateTransition {
  const JavaThreadState _original_state;

  void transition_back() {
    _thread->set_thread_state(_thread_in_vm_trans);
    InterfaceSupport::serialize_thread_state(_thread);
    SafepointMechanism::block_if_requested(_thread);
    _thread->set_thread_state(_original_state);
  }

 public:
  ThreadInVMForHandshake(JavaThread* thread) : ThreadStateTransition(thread),
      _original_state(thread->thread_state()) {
    if (thread->has_last_Java_frame()) {
      thread->frame_anchor()->make_walkable(thread);
    }

    thread->set_thread_state(_thread_in_vm);
  }

  ~ThreadInVMForHandshake() {
    transition_back();
  }
};

class ThreadInVMfromJava : public ThreadStateTransition {
 public:
  ThreadInVMfromJava(JavaThread* thread) : ThreadStateTransition(thread) {
    trans_from_java(_thread_in_vm);
  }
  ~ThreadInVMfromJava() {
    if (_thread->stack_yellow_reserved_zone_disabled()) {
      _thread->enable_stack_yellow_reserved_zone();
    }
    trans(_thread_in_vm, _thread_in_Java);
    // Check for pending. async. exceptions or suspends.
    if (_thread->has_special_runtime_exit_condition()) _thread->handle_special_runtime_exit_condition();
  }
};

class ThreadInVMfromUnknown {
 private:
  JavaThread* _thread;
 public:
  ThreadInVMfromUnknown() : _thread(NULL) {
    Thread* t = Thread::current();
    if (t->is_Java_thread()) {
      JavaThread* t2 = (JavaThread*) t;
      if (t2->thread_state() == _thread_in_native) {
        _thread = t2;
        ThreadStateTransition::transition_from_native(t2, _thread_in_vm);
        // Used to have a HandleMarkCleaner but that is dangerous as
        // it could free a handle in our (indirect, nested) caller.
        // We expect any handles will be short lived and figure we
        // don't need an actual HandleMark.
      }
    }
  }
  ~ThreadInVMfromUnknown() {
    if (_thread) {
      ThreadStateTransition::transition_and_fence(_thread, _thread_in_vm, _thread_in_native);
    }
  }
};

class ThreadInVMfromNative : public ThreadStateTransition {
 public:
  ThreadInVMfromNative(JavaThread* thread) : ThreadStateTransition(thread) {
    trans_from_native(_thread_in_vm);
  }
  ~ThreadInVMfromNative() {
    trans_and_fence(_thread_in_vm, _thread_in_native);
  }
};

class ThreadToNativeFromVM : public ThreadStateTransition {
 public:
  ThreadToNativeFromVM(JavaThread *thread) : ThreadStateTransition(thread) {
    // We are leaving the VM at this point and going directly to native code.
    // Block, if we are in the middle of a safepoint synchronization.
    thread->frame_anchor()->make_walkable(thread);
    trans_and_fence(_thread_in_vm, _thread_in_native);
    // Check for pending. async. exceptions or suspends.
    if (_thread->has_special_runtime_exit_condition()) _thread->handle_special_runtime_exit_condition(false);
  }

  ~ThreadToNativeFromVM() {
    trans_from_native(_thread_in_vm);
    // We don't need to clear_walkable because it will happen automagically when we return to java
  }
};

class ThreadBlockInVM : public ThreadStateTransition {
 public:
  ThreadBlockInVM(JavaThread *thread)
  : ThreadStateTransition(thread) {
    // Once we are blocked vm expects stack to be walkable
    thread->frame_anchor()->make_walkable(thread);
    trans_and_fence(_thread_in_vm, _thread_blocked);
  }
  ~ThreadBlockInVM() {
    trans_and_fence(_thread_blocked, _thread_in_vm);
    // We don't need to clear_walkable because it will happen automagically when we return to java
  }
};

// This special transition class is only used to prevent asynchronous exceptions
// from being installed on vm exit in situations where we can't tolerate them.
// See bugs: 4324348, 4854693, 4998314, 5040492, 5050705.
class ThreadInVMfromJavaNoAsyncException : public ThreadStateTransition {
 public:
  ThreadInVMfromJavaNoAsyncException(JavaThread* thread) : ThreadStateTransition(thread) {
    trans_from_java(_thread_in_vm);
  }
  ~ThreadInVMfromJavaNoAsyncException() {
    if (_thread->stack_yellow_reserved_zone_disabled()) {
      _thread->enable_stack_yellow_reserved_zone();
    }
    trans(_thread_in_vm, _thread_in_Java);
    // NOTE: We do not check for pending. async. exceptions.
    // If we did and moved the pending async exception over into the
    // pending exception field, we would need to deopt (currently C2
    // only). However, to do so would require that we transition back
    // to the _thread_in_vm state. Instead we postpone the handling of
    // the async exception.

    // Check for pending. suspends only.
    if (_thread->has_special_runtime_exit_condition())
      _thread->handle_special_runtime_exit_condition(false);
  }
};

// Debug class instantiated in JRT_ENTRY and ITR_ENTRY macro.
// Can be used to verify properties on enter/exit of the VM.

// VM-internal runtime interface support

// Definitions for JRT (Java (Compiler/Shared) Runtime)

// JRT_LEAF currently can be called from either _thread_in_Java or
// _thread_in_native mode. In _thread_in_native, it is ok
// for another thread to trigger GC. The rest of the JRT_LEAF
// rules apply.
class JRTLeafVerifier : public NoSafepointVerifier {
  static bool should_verify_GC();
 public:
  JRTLeafVerifier() { }
  ~JRTLeafVerifier() { }
};

// LEAF routines do not lock, GC or throw exceptions

#define VM_ENTRY_BASE_FROM_LEAF(result_type, header, thread) \
  HandleMarkCleaner __hm(thread); \
  Thread* THREAD = thread; \
  /* begin of body */

// ENTRY routines may lock, GC and throw exceptions

#define VM_ENTRY_BASE(result_type, header, thread) \
  HandleMarkCleaner __hm(thread); \
  Thread* THREAD = thread; \
  /* begin of body */

// QUICK_ENTRY routines behave like ENTRY but without a handle mark

#define VM_QUICK_ENTRY_BASE(result_type, header, thread) \
  Thread* THREAD = thread; \
  /* begin of body */

#define JRT_ENTRY(result_type, header) \
  result_type header { \
    ThreadInVMfromJava __tiv(thread); \
    VM_ENTRY_BASE(result_type, header, thread)

#define JRT_LEAF(result_type, header) \
  result_type header { \
  /* begin of body */

#define JRT_ENTRY_NO_ASYNC(result_type, header) \
  result_type header { \
    ThreadInVMfromJavaNoAsyncException __tiv(thread); \
    VM_ENTRY_BASE(result_type, header, thread)

// Same as JRT Entry but allows for return value after the safepoint
// to get back into Java from the VM
#define JRT_BLOCK_ENTRY(result_type, header) \
  result_type header { \
    HandleMarkCleaner __hm(thread);

#define JRT_BLOCK \
    { \
    ThreadInVMfromJava __tiv(thread); \
    Thread* THREAD = thread;

#define JRT_BLOCK_NO_ASYNC \
    { \
    ThreadInVMfromJavaNoAsyncException __tiv(thread); \
    Thread* THREAD = thread;

#define JRT_BLOCK_END }

#define JRT_END }

// Definitions for JNI

#define JNI_ENTRY(result_type, header) \
    JNI_ENTRY_NO_PRESERVE(result_type, header) \
    WeakPreserveExceptionMark __wem(thread);

#define JNI_ENTRY_NO_PRESERVE(result_type, header) \
extern "C" { \
  result_type JNICALL header { \
    JavaThread* thread=JavaThread::thread_from_jni_environment(env); \
    ThreadInVMfromNative __tiv(thread); \
    VM_ENTRY_BASE(result_type, header, thread)

// Ensure that the VMNativeEntryWrapper constructor, which can cause
// a GC, is called outside the NoHandleMark (set via VM_QUICK_ENTRY_BASE).
#define JNI_QUICK_ENTRY(result_type, header) \
extern "C" { \
  result_type JNICALL header { \
    JavaThread* thread=JavaThread::thread_from_jni_environment(env); \
    ThreadInVMfromNative __tiv(thread); \
    VM_QUICK_ENTRY_BASE(result_type, header, thread)

#define JNI_LEAF(result_type, header) \
extern "C" { \
  result_type JNICALL header { \
    JavaThread* thread=JavaThread::thread_from_jni_environment(env); \
    /* begin of body */

// Close the routine and the extern "C"
#define JNI_END } }

// Definitions for JVM

#define JVM_ENTRY(result_type, header) \
extern "C" { \
  result_type JNICALL header { \
    JavaThread* thread=JavaThread::thread_from_jni_environment(env); \
    ThreadInVMfromNative __tiv(thread); \
    VM_ENTRY_BASE(result_type, header, thread)

#define JVM_ENTRY_NO_ENV(result_type, header) \
extern "C" { \
  result_type JNICALL header { \
    JavaThread* thread = JavaThread::current(); \
    ThreadInVMfromNative __tiv(thread); \
    VM_ENTRY_BASE(result_type, header, thread)

#define JVM_QUICK_ENTRY(result_type, header) \
extern "C" { \
  result_type JNICALL header { \
    JavaThread* thread=JavaThread::thread_from_jni_environment(env); \
    ThreadInVMfromNative __tiv(thread); \
    VM_QUICK_ENTRY_BASE(result_type, header, thread)

#define JVM_LEAF(result_type, header) \
extern "C" { \
  result_type JNICALL header { \
    VM_Exit::block_if_vm_exited(); \
    /* begin of body */

#define JVM_ENTRY_FROM_LEAF(env, result_type, header) \
  { { \
    JavaThread* thread=JavaThread::thread_from_jni_environment(env); \
    ThreadInVMfromNative __tiv(thread); \
    VM_ENTRY_BASE_FROM_LEAF(result_type, header, thread)

#define JVM_END } }

#endif
