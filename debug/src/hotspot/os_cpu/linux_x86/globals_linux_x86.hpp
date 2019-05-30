#ifndef OS_CPU_LINUX_X86_VM_GLOBALS_LINUX_X86_HPP
#define OS_CPU_LINUX_X86_VM_GLOBALS_LINUX_X86_HPP

// Sets the default values for platform dependent flags used by the runtime system.
// (see globals.hpp)

define_pd_global(bool, DontYieldALot,            false);
#ifdef AMD64
define_pd_global(intx, CompilerThreadStackSize,  1024);
define_pd_global(intx, ThreadStackSize,          1024); // 0 => use system default
define_pd_global(intx, VMThreadStackSize,        1024);
#else
define_pd_global(intx, CompilerThreadStackSize,  512);
// ThreadStackSize 320 allows a couple of test cases to run while
// keeping the number of threads that can be created high.  System
// default ThreadStackSize appears to be 512 which is too big.
define_pd_global(intx, ThreadStackSize,          320);
define_pd_global(intx, VMThreadStackSize,        512);
#endif

define_pd_global(size_t, JVMInvokeMethodSlack,   8192);

// Used on 64 bit platforms for UseCompressedOops base address
define_pd_global(size_t, HeapBaseMinAddress,     2*G);

#endif
