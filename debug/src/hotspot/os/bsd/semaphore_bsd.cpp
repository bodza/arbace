#include "precompiled/precompiled.hpp"

#include "semaphore_bsd.hpp"
#include "utilities/debug.hpp"

#include <semaphore.h>

#ifdef __APPLE__
// OS X doesn't support unamed POSIX semaphores, so the implementation in os_posix.cpp can't be used.

static const char* sem_init_strerror(kern_return_t value) {
  switch (value) {
    case KERN_INVALID_ARGUMENT:  return "Invalid argument";
    case KERN_RESOURCE_SHORTAGE: return "Resource shortage";
    default:                     return "Unknown";
  }
}

OSXSemaphore::OSXSemaphore(uint value) {
  kern_return_t ret = semaphore_create(mach_task_self(), &_semaphore, SYNC_POLICY_FIFO, value);

  guarantee(ret == KERN_SUCCESS, "Failed to create semaphore: %s", sem_init_strerror(ret));
}

OSXSemaphore::~OSXSemaphore() {
  semaphore_destroy(mach_task_self(), _semaphore);
}

void OSXSemaphore::signal(uint count) {
  for (uint i = 0; i < count; i++) {
    kern_return_t ret = semaphore_signal(_semaphore);
  }
}

void OSXSemaphore::wait() {
  kern_return_t ret;
  while ((ret = semaphore_wait(_semaphore)) == KERN_ABORTED) {
    // Semaphore was interrupted. Retry.
  }
}

int64_t OSXSemaphore::currenttime() {
  struct timeval tv;
  gettimeofday(&tv, NULL);
  return (tv.tv_sec * NANOSECS_PER_SEC) + (tv.tv_usec * 1000);
}

bool OSXSemaphore::trywait() {
  return timedwait(0, 0);
}

bool OSXSemaphore::timedwait(unsigned int sec, int nsec) {
  kern_return_t kr = KERN_ABORTED;
  mach_timespec_t waitspec;
  waitspec.tv_sec = sec;
  waitspec.tv_nsec = nsec;

  int64_t starttime = currenttime();

  kr = semaphore_timedwait(_semaphore, waitspec);
  while (kr == KERN_ABORTED) {
    int64_t totalwait = (sec * NANOSECS_PER_SEC) + nsec;

    int64_t current = currenttime();
    int64_t passedtime = current - starttime;

    if (passedtime >= totalwait) {
      waitspec.tv_sec = 0;
      waitspec.tv_nsec = 0;
    } else {
      int64_t waittime = totalwait - (current - starttime);
      waitspec.tv_sec = waittime / NANOSECS_PER_SEC;
      waitspec.tv_nsec = waittime % NANOSECS_PER_SEC;
    }

    kr = semaphore_timedwait(_semaphore, waitspec);
  }

  return kr == KERN_SUCCESS;
}
#endif
