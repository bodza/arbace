#include "precompiled.hpp"
#include "runtime/frame.inline.hpp"
#include "runtime/thread.inline.hpp"

frame JavaThread::pd_last_frame() {
  assert(has_last_Java_frame(), "must have last_Java_sp() when suspended");
  return frame(last_Java_fp(), last_Java_sp());
}

void JavaThread::cache_global_variables() {
  // nothing to do
}
