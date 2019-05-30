#include "precompiled.hpp"
#include "runtime/thread.inline.hpp"
#include "runtime/threadCritical.hpp"

// put OS-includes here
# include <pthread.h>

//
// See threadCritical.hpp for details of this class.
//

static pthread_t             tc_owner = 0;
static pthread_mutex_t       tc_mutex = PTHREAD_MUTEX_INITIALIZER;
static int                   tc_count = 0;

ThreadCritical::ThreadCritical() {
  pthread_t self = pthread_self();
  if (self != tc_owner) {
    int ret = pthread_mutex_lock(&tc_mutex);
    guarantee(ret == 0, "fatal error with pthread_mutex_lock()");
    assert(tc_count == 0, "Lock acquired with illegal reentry count.");
    tc_owner = self;
  }
  tc_count++;
}

ThreadCritical::~ThreadCritical() {
  assert(tc_owner == pthread_self(), "must have correct owner");
  assert(tc_count > 0, "must have correct count");

  tc_count--;
  if (tc_count == 0) {
    tc_owner = 0;
    int ret = pthread_mutex_unlock(&tc_mutex);
    guarantee(ret == 0, "fatal error with pthread_mutex_unlock()");
  }
}
