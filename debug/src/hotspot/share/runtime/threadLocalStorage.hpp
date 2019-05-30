#ifndef SHARE_VM_RUNTIME_THREADLOCALSTORAGE_HPP
#define SHARE_VM_RUNTIME_THREADLOCALSTORAGE_HPP

#include "memory/allocation.hpp"

// forward-decl as we can't have an include cycle
class Thread;

// Wrapper class for library-based (as opposed to compiler-based)
// thread-local storage (TLS). All platforms require this for
// signal-handler based TLS access (which while not strictly async-signal
// safe in theory, is and has-been for a long time, in practice).
// Platforms without compiler-based TLS (i.e. __thread storage-class modifier)
// will use this implementation for all TLS access - see thread.hpp/cpp

class ThreadLocalStorage : AllStatic {

 // Exported API
 public:
  static Thread* thread(); // return current thread, if attached
  static void    set_thread(Thread* thread); // set current thread
  static void    init();
  static bool    is_initialized(); // can't use TLS prior to initialization
};

#endif
