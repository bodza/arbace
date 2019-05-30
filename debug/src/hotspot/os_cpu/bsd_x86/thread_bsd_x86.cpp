#include "precompiled.hpp"
#include "memory/metaspaceShared.hpp"
#include "runtime/frame.inline.hpp"
#include "runtime/thread.inline.hpp"

frame JavaThread::pd_last_frame() {
  assert(has_last_Java_frame(), "must have last_Java_sp() when suspended");
  vmassert(_anchor.last_Java_pc() != NULL, "not walkable");
  return frame(_anchor.last_Java_sp(), _anchor.last_Java_fp(), _anchor.last_Java_pc());
}

// For Forte Analyzer AsyncGetCallTrace profiling support - thread is
// currently interrupted by SIGPROF
bool JavaThread::pd_get_top_frame_for_signal_handler(frame* fr_addr,
  void* ucontext, bool isInJava) {
  assert(Thread::current() == this, "caller must be current thread");
  return pd_get_top_frame(fr_addr, ucontext, isInJava);
}

bool JavaThread::pd_get_top_frame_for_profiling(frame* fr_addr, void* ucontext, bool isInJava) {
  return pd_get_top_frame(fr_addr, ucontext, isInJava);
}

bool JavaThread::pd_get_top_frame(frame* fr_addr, void* ucontext, bool isInJava) {
  assert(this->is_Java_thread(), "must be JavaThread");
  JavaThread* jt = (JavaThread *)this;

  // If we have a last_Java_frame, then we should use it even if
  // isInJava == true.  It should be more reliable than ucontext info.
  if (jt->has_last_Java_frame() && jt->frame_anchor()->walkable()) {
    *fr_addr = jt->pd_last_frame();
    return true;
  }

  // At this point, we don't have a last_Java_frame, so
  // we try to glean some information out of the ucontext
  // if we were running Java code when SIGPROF came in.
  if (isInJava) {
    ucontext_t* uc = (ucontext_t*) ucontext;

    intptr_t* ret_fp;
    intptr_t* ret_sp;
    ExtendedPC addr = os::Bsd::fetch_frame_from_ucontext(this, uc,
      &ret_sp, &ret_fp);
    if (addr.pc() == NULL || ret_sp == NULL ) {
      // ucontext wasn't useful
      return false;
    }

    if (MetaspaceShared::is_in_trampoline_frame(addr.pc())) {
      // In the middle of a trampoline call. Bail out for safety.
      // This happens rarely so shouldn't affect profiling.
      return false;
    }

    frame ret_frame(ret_sp, ret_fp, addr.pc());
    if (!ret_frame.safe_for_sender(jt)) {
      // C2 and JVMCI use ebp as a general register see if NULL fp helps
      frame ret_frame2(ret_sp, NULL, addr.pc());
      if (!ret_frame2.safe_for_sender(jt)) {
        // nothing else to try if the frame isn't good
        return false;
      }
      ret_frame = ret_frame2;
    }
    *fr_addr = ret_frame;
    return true;
  }

  // nothing else to try
  return false;
}

void JavaThread::cache_global_variables() { }
