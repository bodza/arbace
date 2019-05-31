#ifndef OS_BSD_VM_GLOBALS_BSD_HPP
#define OS_BSD_VM_GLOBALS_BSD_HPP

//
// Defines Bsd specific flags. They are not available on other platforms.
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
  /*  NB: The default value of UseBsdPosixThreadCPUClocks may be  */ \
  /*  overridden in Arguments::parse_each_vm_init_arg.            */ \
  product(bool, UseBsdPosixThreadCPUClocks, true, \
          "enable fast Bsd Posix clocks where available") \
 \
  product(bool, UseHugeTLBFS, false, \
          "Use MAP_HUGETLB for large pages") \
 \
  product(bool, UseSHM, false, \
          "Use SYSV shared memory for large pages")

//
// Defines Bsd-specific default values. The flags are available on all
// platforms, but they may have different default values on other platforms.
//
define_pd_global(bool, UseLargePages, false);
define_pd_global(bool, UseLargePagesIndividualAllocation, false);
define_pd_global(bool, UseOSErrorReporting, false);
define_pd_global(bool, UseThreadPriorities, true);

#endif
