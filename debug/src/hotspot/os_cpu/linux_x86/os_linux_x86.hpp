#ifndef OS_CPU_LINUX_X86_VM_OS_LINUX_X86_HPP
#define OS_CPU_LINUX_X86_VM_OS_LINUX_X86_HPP

  static void setup_fpu();
  static bool supports_sse();

  static jlong rdtsc();

  static bool is_allocatable(size_t bytes);

  // Used to register dynamic code cache area with the OS
  // Note: Currently only used in 64 bit Windows implementations
  static bool register_code_area(char *low, char *high) { return true; }

  /*
   * Work-around for broken NX emulation using CS limit, Red Hat patch "Exec-Shield"
   * (IA32 only).
   *
   * Map and execute at a high VA to prevent CS lazy updates race with SMP MM
   * invalidation.Further code generation by the JVM will no longer cause CS limit
   * updates.
   *
   * Affects IA32: RHEL 5 & 6, Ubuntu 10.04 (LTS), 10.10, 11.04, 11.10, 12.04.
   * @see JDK-8023956
   */
  static void workaround_expand_exec_shield_cs_limit();

#endif
