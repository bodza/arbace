#ifndef OS_CPU_LINUX_ZERO_VM_OS_LINUX_ZERO_HPP
#define OS_CPU_LINUX_ZERO_VM_OS_LINUX_ZERO_HPP

  static void setup_fpu() { }

  static bool is_allocatable(size_t bytes);

  // Used to register dynamic code cache area with the OS
  // Note: Currently only used in 64 bit Windows implementations
  static bool register_code_area(char *low, char *high) { return true; }

  // Atomically copy 64 bits of data
  static void atomic_copy64(const volatile void *src, volatile void *dst) {
#if defined(PPC32) && !defined(__SPE__)
    double tmp;
    asm volatile ("lfd  %0, %2\n"
                  "stfd %0, %1\n"
                  : "=&f"(tmp), "=Q"(*(volatile double*)dst)
                  : "Q"(*(volatile double*)src));
#elif defined(PPC32) && defined(__SPE__)
    long tmp;
    asm volatile ("evldd  %0, %2\n"
                  "evstdd %0, %1\n"
                  : "=&r"(tmp), "=Q"(*(volatile long*)dst)
                  : "Q"(*(volatile long*)src));
#elif defined(__ARM_ARCH_7A__)
    // Note that a ldrexd + clrex combination is only needed for
    // correctness on the OS level (context-switches). In this
    // case, clrex *may* be beneficial for performance. For now
    // don't bother with clrex as this is Zero.
    jlong tmp;
    asm volatile ("ldrexd  %0, [%1]\n"
                  : "=r"(tmp)
                  : "r"(src), "m"(src));
    *(jlong *) dst = tmp;
#else
    *(jlong *) dst = *(const jlong *) src;
#endif
  }

#endif
