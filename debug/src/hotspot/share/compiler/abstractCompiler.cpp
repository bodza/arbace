#include "precompiled.hpp"

#include "compiler/abstractCompiler.hpp"
#include "compiler/compileBroker.hpp"
#include "runtime/mutexLocker.hpp"

bool AbstractCompiler::should_perform_init() {
  if (_compiler_state != initialized) {
    MutexLocker only_one(CompileThread_lock);

    if (_compiler_state == uninitialized) {
      _compiler_state = initializing;
      return true;
    } else {
      while (_compiler_state == initializing) {
        CompileThread_lock->wait();
      }
    }
  }
  return false;
}

bool AbstractCompiler::should_perform_shutdown() {
  // Since this method can be called by multiple threads, the lock ensures atomicity of
  // decrementing '_num_compiler_threads' and the following operations.
  MutexLocker only_one(CompileThread_lock);
  _num_compiler_threads--;

  // Only the last thread will perform shutdown operations
  if (_num_compiler_threads == 0) {
    return true;
  }
  return false;
}

void AbstractCompiler::set_state(int state) {
  // Ensure that state is only set by one thread at a time
  MutexLocker only_one(CompileThread_lock);
  _compiler_state =  state;
  CompileThread_lock->notify_all();
}
