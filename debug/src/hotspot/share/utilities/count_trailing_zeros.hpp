#ifndef SHARE_VM_UTILITIES_COUNTTRAILINGZEROS_HPP
#define SHARE_VM_UTILITIES_COUNTTRAILINGZEROS_HPP

#include "utilities/debug.hpp"
#include "utilities/globalDefinitions.hpp"

// unsigned count_trailing_zeros(uintx x)
// Return the number of trailing zeros in x, e.g. the zero-based index
// of the least significant set bit in x.
// Precondition: x != 0.

// Dispatch on toolchain to select implementation.

/*****************************************************************************
 * GCC and compatible (including Clang)
 *****************************************************************************/
#if defined(TARGET_COMPILER_gcc)

inline unsigned count_trailing_zeros(uintx x) {
  STATIC_ASSERT(sizeof(unsigned long) == sizeof(uintx));
  assert(x != 0, "precondition");
  return __builtin_ctzl(x);
}

/*****************************************************************************
 * Microsoft Visual Studio
 *****************************************************************************/
#elif defined(TARGET_COMPILER_visCPP)

#include <intrin.h>

#pragma intrinsic(_BitScanForward64)

inline unsigned count_trailing_zeros(uintx x) {
  assert(x != 0, "precondition");
  unsigned long index;
  _BitScanForward64(&index, x);
  return index;
}

/*****************************************************************************
 * IBM XL C/C++
 *****************************************************************************/
#elif defined(TARGET_COMPILER_xlc)

#include <builtins.h>

inline unsigned count_trailing_zeros(uintx x) {
  assert(x != 0, "precondition");
  return __cnttz8(x);
}

/*****************************************************************************
 * Oracle Studio
 *****************************************************************************/
#elif defined(TARGET_COMPILER_sparcWorks)

// No compiler built-in / intrinsic, so use inline assembler.

#include "utilities/macros.hpp"

#include OS_CPU_HEADER(count_trailing_zeros)

/*****************************************************************************
 * Unknown toolchain
 *****************************************************************************/
#else
#error Unknown TARGET_COMPILER

#endif

#endif
