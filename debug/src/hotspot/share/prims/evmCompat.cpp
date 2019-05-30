#include "precompiled.hpp"
#include "utilities/debug.hpp"

// This file contains definitions for functions that exist
// in the ExactVM, but not in HotSpot. They are stubbed out
// here to prevent linker errors when attempting to use HotSpot
// with the ExactVM jdk.

extern "C" void JVM_Process_DestroyProcess(void);
extern "C" void JVM_Process_ForkAndExec(void);
extern "C" void JVM_Process_WaitForProcessExit(void);
extern "C" void gc(void);

void JVM_Process_DestroyProcess(void) {
  ShouldNotReachHere();
}

void JVM_Process_ForkAndExec(void) {
  ShouldNotReachHere();
}

void JVM_Process_WaitForProcessExit(void) {
  ShouldNotReachHere();
}

void gc(void) {
  ShouldNotReachHere();
}
