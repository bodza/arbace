#ifndef OS_LINUX_VM_GLOBALS_LINUX_HPP
#define OS_LINUX_VM_GLOBALS_LINUX_HPP

//
// Defines Linux specific flags. They are not available on other platforms.
//
#define RUNTIME_OS_FLAGS(develop, \
                         develop_pd, \
                         product, \
                         product_pd, \
                         diagnostic, \
                         diagnostic_pd, \
                         notproduct, \
                         range, \
                         constraint, \
                         writeable) \
 \
  product(bool, UseOprofile, false, \
        "enable support for Oprofile profiler") \
 \
  /*  NB: The default value of UseLinuxPosixThreadCPUClocks may be   */ \
  /* overridden in Arguments::parse_each_vm_init_arg.                */ \
  product(bool, UseLinuxPosixThreadCPUClocks, true, \
          "enable fast Linux Posix clocks where available") \
 \
  product(bool, UseHugeTLBFS, false, \
          "Use MAP_HUGETLB for large pages") \
 \
  product(bool, UseTransparentHugePages, false, \
          "Use MADV_HUGEPAGE for large pages") \
 \
  product(bool, LoadExecStackDllInVMThread, true, \
          "Load DLLs with executable-stack attribute in the VM Thread") \
 \
  product(bool, UseSHM, false, \
          "Use SYSV shared memory for large pages") \
 \
  product(bool, UseContainerSupport, true, \
          "Enable detection and runtime container configuration support") \
 \
  product(bool, PreferContainerQuotaForCPUCount, true, \
          "Calculate the container CPU availability based on the value of quotas (if set), when true. Otherwise, use the CPU shares value, provided it is less than quota.") \
 \
  diagnostic(bool, UseCpuAllocPath, false, \
          "Use CPU_ALLOC code path in os::active_processor_count ")

//
// Defines Linux-specific default values. The flags are available on all
// platforms, but they may have different default values on other platforms.
//
define_pd_global(bool, UseLargePages, false);
define_pd_global(bool, UseLargePagesIndividualAllocation, false);
define_pd_global(bool, UseOSErrorReporting, false);
define_pd_global(bool, UseThreadPriorities, true);

#endif
