#ifndef SHARE_VM_RUNTIME_SEMAPHORE_INLINE_HPP
#define SHARE_VM_RUNTIME_SEMAPHORE_INLINE_HPP

#include "runtime/interfaceSupport.inline.hpp"
#include "runtime/semaphore.hpp"
#include "runtime/thread.inline.hpp"

inline void Semaphore::wait_with_safepoint_check(JavaThread* thread) {
  // Prepare to block and allow safepoints while blocked
  ThreadBlockInVM tbivm(thread);
  OSThreadWaitState osts(thread->osthread(), false /* not in Object.wait() */);

  // Wait for value
  _impl.wait();
}

#endif
