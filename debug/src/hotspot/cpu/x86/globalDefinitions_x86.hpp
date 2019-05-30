#ifndef CPU_X86_VM_GLOBALDEFINITIONS_X86_HPP
#define CPU_X86_VM_GLOBALDEFINITIONS_X86_HPP

const int StackAlignmentInBytes  = 16;

// Indicates whether the C calling conventions require that
// 32-bit integer argument values are extended to 64 bits.
const bool CCallingConventionRequiresIntsAsLongs = false;

#define SUPPORTS_NATIVE_CX8

// The expected size in bytes of a cache line, used to pad data structures.
// pure C1, 32-bit, small machine
// i486 was the last Intel chip with 16-byte cache line size
#define DEFAULT_CACHE_LINE_SIZE 32

#if defined(LINUX) || defined(__APPLE__)
#define SUPPORT_RESERVED_STACK_AREA
#endif

#define THREAD_LOCAL_POLL

#endif
