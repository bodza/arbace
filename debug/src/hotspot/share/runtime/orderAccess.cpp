#include "precompiled.hpp"
#include "runtime/orderAccess.hpp"
#include "runtime/stubRoutines.hpp"
#include "runtime/thread.hpp"

void OrderAccess::StubRoutines_fence() {
  // Use a stub if it exists.  It may not exist during bootstrap so do
  // nothing in that case but assert if no fence code exists after threads have been created
  void (*func)() = CAST_TO_FN_PTR(void (*)(), StubRoutines::fence_entry());

  if (func != NULL) {
    (*func)();
    return;
  }
  assert(Threads::number_of_threads() == 0, "for bootstrap only");
}
