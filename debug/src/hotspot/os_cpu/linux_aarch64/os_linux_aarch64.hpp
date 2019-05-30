#ifndef OS_CPU_LINUX_AARCH64_VM_OS_LINUX_AARCH64_HPP
#define OS_CPU_LINUX_AARCH64_VM_OS_LINUX_AARCH64_HPP

  static void setup_fpu();
  static bool supports_sse();

  static jlong rdtsc();

  static bool is_allocatable(size_t bytes);

  // Used to register dynamic code cache area with the OS
  // Note: Currently only used in 64 bit Windows implementations
  static bool register_code_area(char *low, char *high) { return true; }

  // Atomically copy 64 bits of data
  static void atomic_copy64(const volatile void *src, volatile void *dst) {
    *(jlong *) dst = *(const jlong *) src;
  }

#endif
