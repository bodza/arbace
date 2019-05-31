#ifndef OS_CPU_BSD_ZERO_VM_OS_BSD_ZERO_HPP
#define OS_CPU_BSD_ZERO_VM_OS_BSD_ZERO_HPP

  static void setup_fpu() { }

  static bool is_allocatable(size_t bytes);

  // Used to register dynamic code cache area with the OS
  // Note: Currently only used in 64 bit Windows implementations
  static bool register_code_area(char *low, char *high) { return true; }

  // Atomically copy 64 bits of data
  static void atomic_copy64(const volatile void *src, volatile void *dst) {
#if defined(PPC32)
    double tmp;
    asm volatile ("lfd  %0, 0(%1)\n"
                  "stfd %0, 0(%2)\n"
                  : "=f"(tmp)
                  : "b"(src), "b"(dst));
#else
    *(jlong *) dst = *(const jlong *) src;
#endif
  }

#endif
