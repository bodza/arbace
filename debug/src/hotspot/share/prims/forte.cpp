#include "precompiled.hpp"
#include "code/debugInfoRec.hpp"
#include "code/pcDesc.hpp"
#include "gc/shared/collectedHeap.inline.hpp"
#include "memory/universe.hpp"
#include "oops/oop.inline.hpp"
#include "prims/forte.hpp"
#include "runtime/frame.inline.hpp"
#include "runtime/javaCalls.hpp"
#include "runtime/thread.inline.hpp"
#include "runtime/vframe.inline.hpp"
#include "runtime/vframeArray.hpp"

// call frame copied from old .h file and renamed
typedef struct {
    jint lineno;                      // line number in the source file
    jmethodID method_id;              // method executed in this frame
} ASGCT_CallFrame;

// call trace copied from old .h file and renamed
typedef struct {
    JNIEnv *env_id;                   // Env where trace was recorded
    jint num_frames;                  // number of frames in this trace
    ASGCT_CallFrame *frames;          // frames
} ASGCT_CallTrace;

// These name match the names reported by the forte quality kit
enum {
  ticks_no_Java_frame         =  0,
  ticks_no_class_load         = -1,
  ticks_GC_active             = -2,
  ticks_unknown_not_Java      = -3,
  ticks_not_walkable_not_Java = -4,
  ticks_unknown_Java          = -5,
  ticks_not_walkable_Java     = -6,
  ticks_unknown_state         = -7,
  ticks_thread_exit           = -8,
  ticks_deopt                 = -9,
  ticks_safepoint             = -10
};

extern "C" {
  JNIEXPORT
  void AsyncGetCallTrace(ASGCT_CallTrace *trace, jint depth, void* ucontext) {
    trace->num_frames = ticks_no_class_load; // -1
  }
}
