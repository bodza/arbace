#ifndef OS_CPU_BSD_X86_VM_OS_BSD_X86_INLINE_HPP
#define OS_CPU_BSD_X86_VM_OS_BSD_X86_INLINE_HPP

#include "runtime/os.hpp"

// See http://www.technovelty.org/code/c/reading-rdtsc.htl for details
inline jlong os::rdtsc() {
#ifndef AMD64
  // 64 bit result in edx:eax
  uint64_t res;
  __asm__ __volatile__ ("rdtsc" : "=A" (res));
  return (jlong)res;
#else
  uint64_t res;
  uint32_t ts1, ts2;
  __asm__ __volatile__ ("rdtsc" : "=a" (ts1), "=d" (ts2));
  res = ((uint64_t)ts1 | (uint64_t)ts2 << 32);
  return (jlong)res;
#endif
}

#endif
