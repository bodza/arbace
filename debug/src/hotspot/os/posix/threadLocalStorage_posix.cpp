#include "runtime/threadLocalStorage.hpp"

#include <pthread.h>

static pthread_key_t _thread_key;
static bool _initialized = false;

// Restore the thread pointer if the destructor is called. This is in case
// someone from JNI code sets up a destructor with pthread_key_create to run
// detachCurrentThread on thread death. Unless we restore the thread pointer we
// will hang or crash. When detachCurrentThread is called the key will be set
// to null and we will not be called again. If detachCurrentThread is never
// called we could loop forever depending on the pthread implementation.
extern "C" void restore_thread_pointer(void* p) {
  ThreadLocalStorage::set_thread((Thread*) p);
}

void ThreadLocalStorage::init() {
  int rslt = pthread_key_create(&_thread_key, restore_thread_pointer);
  _initialized = true;
}

bool ThreadLocalStorage::is_initialized() {
  return _initialized;
}

Thread* ThreadLocalStorage::thread() {
  return (Thread*) pthread_getspecific(_thread_key); // may be NULL
}

void ThreadLocalStorage::set_thread(Thread* current) {
  int rslt = pthread_setspecific(_thread_key, current);
}
