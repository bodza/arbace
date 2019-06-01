#include "precompiled.hpp"

#include "c1/c1_Instruction.hpp"
#include "c1/c1_LinearScan.hpp"
#include "utilities/bitMap.inline.hpp"

void LinearScan::allocate_fpu_stack() {
  // No FPU stack on AArch64
}
