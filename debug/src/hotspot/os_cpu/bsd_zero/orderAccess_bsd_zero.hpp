#ifndef OS_CPU_BSD_ZERO_VM_ORDERACCESS_BSD_ZERO_HPP
#define OS_CPU_BSD_ZERO_VM_ORDERACCESS_BSD_ZERO_HPP

// Included in orderAccess.hpp header file.

#ifdef ARM

/*
 * ARM Kernel helper for memory barrier.
 * Using __asm __volatile ("":::"memory") does not work reliable on ARM
 * and gcc __sync_synchronize(); implementation does not use the kernel
 * helper for all gcc versions so it is unreliable to use as well.
 */
typedef void (__kernel_dmb_t) (void);
#define __kernel_dmb (*(__kernel_dmb_t *) 0xffff0fa0)

#define FULL_MEM_BARRIER __kernel_dmb()
#define LIGHT_MEM_BARRIER __kernel_dmb()

#else

#define FULL_MEM_BARRIER __sync_synchronize()

#ifdef PPC

#ifdef __NO_LWSYNC__
#define LIGHT_MEM_BARRIER __asm __volatile ("sync":::"memory")
#else
#define LIGHT_MEM_BARRIER __asm __volatile ("lwsync":::"memory")
#endif

#else

#define LIGHT_MEM_BARRIER __asm __volatile ("":::"memory")

#endif

#endif

// Note: What is meant by LIGHT_MEM_BARRIER is a barrier which is sufficient
// to provide TSO semantics, i.e. StoreStore | LoadLoad | LoadStore.

inline void OrderAccess::loadload()   { LIGHT_MEM_BARRIER; }
inline void OrderAccess::storestore() { LIGHT_MEM_BARRIER; }
inline void OrderAccess::loadstore()  { LIGHT_MEM_BARRIER; }
inline void OrderAccess::storeload()  { FULL_MEM_BARRIER;  }

inline void OrderAccess::acquire()    { LIGHT_MEM_BARRIER; }
inline void OrderAccess::release()    { LIGHT_MEM_BARRIER; }
inline void OrderAccess::fence()      { FULL_MEM_BARRIER;  }

#endif
