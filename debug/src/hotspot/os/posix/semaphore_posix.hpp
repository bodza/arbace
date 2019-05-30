#ifndef OS_POSIX_VM_SEMAPHORE_POSIX_HPP
#define OS_POSIX_VM_SEMAPHORE_POSIX_HPP

#include "memory/allocation.hpp"

#include <semaphore.h>

class PosixSemaphore : public CHeapObj<mtInternal> {
  sem_t _semaphore;

  // Prevent copying and assignment.
  PosixSemaphore(const PosixSemaphore&);
  PosixSemaphore& operator=(const PosixSemaphore&);

 public:
  PosixSemaphore(uint value = 0);
  ~PosixSemaphore();

  void signal(uint count = 1);

  void wait();

  bool trywait();
  bool timedwait(struct timespec ts);
};

typedef PosixSemaphore SemaphoreImpl;

#endif
