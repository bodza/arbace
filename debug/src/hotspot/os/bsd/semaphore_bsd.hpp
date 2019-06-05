#ifndef OS_BSD_VM_SEMAPHORE_BSD_HPP
#define OS_BSD_VM_SEMAPHORE_BSD_HPP

#ifndef __APPLE__
// Use POSIX semaphores.
# include "semaphore_posix.hpp"

#else
// OS X doesn't support unamed POSIX semaphores, so the implementation in os_posix.cpp can't be used.
# include "memory/allocation.hpp"

# include <mach/semaphore.h>

class OSXSemaphore : public CHeapObj<mtInternal>{
  semaphore_t _semaphore;

  // Prevent copying and assignment.
  OSXSemaphore(const OSXSemaphore&);
  OSXSemaphore& operator=(const OSXSemaphore&);

 public:
  OSXSemaphore(uint value = 0);
  ~OSXSemaphore();

  void signal(uint count = 1);

  void wait();

  bool trywait();
  bool timedwait(unsigned int sec, int nsec);

 private:
  static int64_t currenttime();
};

typedef OSXSemaphore SemaphoreImpl;

#endif

#endif
