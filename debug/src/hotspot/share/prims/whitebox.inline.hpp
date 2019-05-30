#ifndef SHARE_VM_PRIMS_WHITEBOX_INLINE_HPP
#define SHARE_VM_PRIMS_WHITEBOX_INLINE_HPP

#include "prims/whitebox.hpp"
#include "runtime/interfaceSupport.inline.hpp"

// Entry macro to transition from JNI to VM state.

#define WB_ENTRY(result_type, header) JNI_ENTRY(result_type, header) \
  ClearPendingJniExcCheck _clearCheck(env);

#define WB_END JNI_END

#endif
