#ifndef OS_CPU_LINUX_X86_VM_PREFETCH_LINUX_X86_INLINE_HPP
#define OS_CPU_LINUX_X86_VM_PREFETCH_LINUX_X86_INLINE_HPP

#include "runtime/prefetch.hpp"

inline void Prefetch::read(void *loc, intx interval) {
#ifdef AMD64
  __asm__ ("prefetcht0 (%0,%1,1)" : : "r" (loc), "r" (interval));
#endif
}

inline void Prefetch::write(void *loc, intx interval) {
#ifdef AMD64

  // Do not use the 3dnow prefetchw instruction.  It isn't supported on em64t.
  //  __asm__ ("prefetchw (%0,%1,1)" : : "r" (loc), "r" (interval));
  __asm__ ("prefetcht0 (%0,%1,1)" : : "r" (loc), "r" (interval));

#endif
}

#endif
