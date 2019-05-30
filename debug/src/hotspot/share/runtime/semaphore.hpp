#ifndef SHARE_VM_RUNTIME_SEMAPHORE_HPP
#define SHARE_VM_RUNTIME_SEMAPHORE_HPP

#include "memory/allocation.hpp"

#if defined(LINUX)
# include "semaphore_posix.hpp"
#elif defined(BSD)
# include "semaphore_bsd.hpp"
#else
# error "No semaphore implementation provided for this OS"
#endif

class JavaThread;

// Implements the limited, platform independent Semaphore API.
class Semaphore : public CHeapObj<mtInternal> {
  SemaphoreImpl _impl;

  // Prevent copying and assignment of Semaphore instances.
  Semaphore(const Semaphore&);
  Semaphore& operator=(const Semaphore&);

 public:
  Semaphore(uint value = 0) : _impl(value) {}
  ~Semaphore() {}

  void signal(uint count = 1) { _impl.signal(count); }

  void wait()                 { _impl.wait(); }

  bool trywait()              { return _impl.trywait(); }

  void wait_with_safepoint_check(JavaThread* thread);
};

#endif
