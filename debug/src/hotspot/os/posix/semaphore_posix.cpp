#include "precompiled/precompiled.hpp"

#ifndef __APPLE__
#include "runtime/os.hpp"
// POSIX unamed semaphores are not supported on OS X.
#include "semaphore_posix.hpp"
#include <semaphore.h>

#define check_with_errno(check_type, cond, msg) \
  do { \
    int err = errno; \
    check_type(cond, "%s; error='%s' (errno=%s)", msg, os::strerror(err), os::errno_name(err)); \
} while (false)

#define assert_with_errno(cond, msg)    check_with_errno(assert, cond, msg)
#define guarantee_with_errno(cond, msg) check_with_errno(guarantee, cond, msg)

PosixSemaphore::PosixSemaphore(uint value) {
  int ret = sem_init(&_semaphore, 0, value);

  guarantee_with_errno(ret == 0, "Failed to initialize semaphore");
}

PosixSemaphore::~PosixSemaphore() {
  sem_destroy(&_semaphore);
}

void PosixSemaphore::signal(uint count) {
  for (uint i = 0; i < count; i++) {
    int ret = sem_post(&_semaphore);

    assert_with_errno(ret == 0, "sem_post failed");
  }
}

void PosixSemaphore::wait() {
  int ret;

  do {
    ret = sem_wait(&_semaphore);
  } while (ret != 0 && errno == EINTR);

  assert_with_errno(ret == 0, "sem_wait failed");
}

bool PosixSemaphore::trywait() {
  int ret;

  do {
    ret = sem_trywait(&_semaphore);
  } while (ret != 0 && errno == EINTR);

  assert_with_errno(ret == 0 || errno == EAGAIN, "trywait failed");

  return ret == 0;
}

bool PosixSemaphore::timedwait(struct timespec ts) {
  while (true) {
    int result = sem_timedwait(&_semaphore, &ts);
    if (result == 0) {
      return true;
    } else if (errno == EINTR) {
      continue;
    } else if (errno == ETIMEDOUT) {
      return false;
    } else {
      assert_with_errno(false, "timedwait failed");
      return false;
    }
  }
}
#endif
