#ifndef CPU_ZERO_VM_GLOBALDEFINITIONS_ZERO_HPP
#define CPU_ZERO_VM_GLOBALDEFINITIONS_ZERO_HPP

#define SUPPORTS_NATIVE_CX8

#include <ffi.h>

// Indicates whether the C calling conventions require that
// 32-bit integer argument values are extended to 64 bits.
const bool CCallingConventionRequiresIntsAsLongs = false;

#endif
