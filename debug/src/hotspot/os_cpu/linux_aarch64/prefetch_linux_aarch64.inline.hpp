#ifndef OS_CPU_LINUX_AARCH64_VM_PREFETCH_LINUX_AARCH64_INLINE_HPP
#define OS_CPU_LINUX_AARCH64_VM_PREFETCH_LINUX_AARCH64_INLINE_HPP

#include "runtime/prefetch.hpp"

inline void Prefetch::read (void *loc, intx interval) {
#ifndef BUILTIN_SIM
  if (interval >= 0)
    asm("prfm PLDL1KEEP, [%0, %1]" : : "r"(loc), "r"(interval));
#endif
}

inline void Prefetch::write(void *loc, intx interval) {
#ifndef BUILTIN_SIM
  if (interval >= 0)
    asm("prfm PSTL1KEEP, [%0, %1]" : : "r"(loc), "r"(interval));
#endif
}

#endif
