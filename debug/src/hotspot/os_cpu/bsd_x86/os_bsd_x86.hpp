#ifndef OS_CPU_BSD_X86_VM_OS_BSD_X86_HPP
#define OS_CPU_BSD_X86_VM_OS_BSD_X86_HPP

  static void setup_fpu();
  static bool supports_sse();

  static jlong rdtsc();

  static bool is_allocatable(size_t bytes);

  // Used to register dynamic code cache area with the OS
  // Note: Currently only used in 64 bit Windows implementations
  static bool register_code_area(char *low, char *high) { return true; }

#endif
