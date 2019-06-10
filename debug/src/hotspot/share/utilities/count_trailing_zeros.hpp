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
  return __builtin_ctzl(x);
}

/*****************************************************************************
 * Unknown toolchain
 *****************************************************************************/
#else
#error Unknown TARGET_COMPILER

#endif

#endif
