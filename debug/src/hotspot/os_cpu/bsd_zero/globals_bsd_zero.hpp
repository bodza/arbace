#ifndef OS_CPU_BSD_ZERO_VM_GLOBALS_BSD_ZERO_HPP
#define OS_CPU_BSD_ZERO_VM_GLOBALS_BSD_ZERO_HPP

//
// Set the default values for platform dependent flags used by the
// runtime system.  See globals.hpp for details of what they do.
//

define_pd_global(bool,  DontYieldALot,           false);
define_pd_global(intx,  ThreadStackSize,         1536);
define_pd_global(intx,  VMThreadStackSize,       1024);
define_pd_global(intx,  CompilerThreadStackSize, 0);
define_pd_global(size_t, JVMInvokeMethodSlack,   8192);

// Used on 64 bit platforms for UseCompressedOops base address
define_pd_global(size_t, HeapBaseMinAddress,     2*G);

#endif
