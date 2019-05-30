#ifndef CPU_AARCH64_VM_GLOBALDEFINITIONS_AARCH64_HPP
#define CPU_AARCH64_VM_GLOBALDEFINITIONS_AARCH64_HPP

const int StackAlignmentInBytes  = 16;

// Indicates whether the C calling conventions require that
// 32-bit integer argument values are extended to 64 bits.
const bool CCallingConventionRequiresIntsAsLongs = false;

#define SUPPORTS_NATIVE_CX8

// According to the ARMv8 ARM, "Concurrent modification and execution
// of instructions can lead to the resulting instruction performing
// any behavior that can be achieved by executing any sequence of
// instructions that can be executed from the same Exception level,
// except where the instruction before modification and the
// instruction after modification is a B, BL, NOP, BKPT, SVC, HVC, or
// SMC instruction."
//
// This makes the games we play when patching difficult, so when we
// come across an access that needs patching we deoptimize.  There are
// ways we can avoid this, but these would slow down C1-compiled code
// in the defauilt case.  We could revisit this decision if we get any
// evidence that it's worth doing.
#define DEOPTIMIZE_WHEN_PATCHING

#define SUPPORT_RESERVED_STACK_AREA

#define THREAD_LOCAL_POLL

#endif
