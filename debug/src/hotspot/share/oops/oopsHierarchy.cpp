#include "precompiled.hpp"

#include "gc/shared/collectedHeap.hpp"
#include "gc/shared/collectedHeap.inline.hpp"
#include "oops/oopsHierarchy.hpp"
#include "runtime/thread.inline.hpp"
#include "utilities/globalDefinitions.hpp"

#ifdef CHECK_UNHANDLED_OOPS

void oop::register_oop() {
  if (!Universe::is_fully_initialized()) return;
  // This gets expensive, which is why checking unhandled oops is on a switch.
  Thread* t = Thread::current_or_null();
  if (t != NULL && t->is_Java_thread()) {
     frame fr = os::current_frame();
     // This points to the oop creator, I guess current frame points to caller
     t->unhandled_oops()->register_unhandled_oop(this, fr.pc());
  }
}

void oop::unregister_oop() {
  if (!Universe::is_fully_initialized()) return;
  // This gets expensive, which is why checking unhandled oops is on a switch.
  Thread* t = Thread::current_or_null();
  if (t != NULL && t->is_Java_thread()) {
    t->unhandled_oops()->unregister_unhandled_oop(this);
  }
}
#endif
