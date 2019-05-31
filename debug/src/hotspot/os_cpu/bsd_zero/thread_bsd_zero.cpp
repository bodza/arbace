#include "precompiled.hpp"
#include "runtime/frame.inline.hpp"
#include "runtime/thread.inline.hpp"

frame JavaThread::pd_last_frame() {
  return frame(last_Java_fp(), last_Java_sp());
}

void JavaThread::cache_global_variables() {
  // nothing to do
}
