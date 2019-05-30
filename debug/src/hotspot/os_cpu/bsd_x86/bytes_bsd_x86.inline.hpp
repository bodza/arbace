#ifndef OS_CPU_BSD_X86_VM_BYTES_BSD_X86_INLINE_HPP
#define OS_CPU_BSD_X86_VM_BYTES_BSD_X86_INLINE_HPP

#ifdef __APPLE__
#include <libkern/OSByteOrder.h>
#endif

#if defined(AMD64)
#  if defined(__APPLE__)
#    define bswap_16(x) OSSwapInt16(x)
#    define bswap_32(x) OSSwapInt32(x)
#    define bswap_64(x) OSSwapInt64(x)
#  elif defined(__OpenBSD__)
#    define bswap_16(x) swap16(x)
#    define bswap_32(x) swap32(x)
#    define bswap_64(x) swap64(x)
#  elif defined(__NetBSD__)
#    define bswap_16(x) bswap16(x)
#    define bswap_32(x) bswap32(x)
#    define bswap_64(x) bswap64(x)
#  else
#    define bswap_16(x) __bswap16(x)
#    define bswap_32(x) __bswap32(x)
#    define bswap_64(x) __bswap64(x)
#  endif
#endif

// Efficient swapping of data bytes from Java byte
// ordering to native byte ordering and vice versa.
inline u2   Bytes::swap_u2(u2 x) {
#ifdef AMD64
  return bswap_16(x);
#else
  u2 ret;
  __asm__ __volatile__ (
    "movw %0, %%ax;"
    "xchg %%al, %%ah;"
    "movw %%ax, %0"
    :"=r" (ret)      // output : register 0 => ret
    :"0"  (x)        // input  : x => register 0
    :"ax", "0"       // clobbered registers
  );
  return ret;
#endif
}

inline u4   Bytes::swap_u4(u4 x) {
#ifdef AMD64
  return bswap_32(x);
#else
  u4 ret;
  __asm__ __volatile__ (
    "bswap %0"
    :"=r" (ret)      // output : register 0 => ret
    :"0"  (x)        // input  : x => register 0
    :"0"             // clobbered register
  );
  return ret;
#endif
}

#ifdef AMD64
inline u8 Bytes::swap_u8(u8 x) {
#ifdef SPARC_WORKS
  // workaround for SunStudio12 CR6615391
  __asm__ __volatile__ (
    "bswapq %0"
    :"=r" (x)        // output : register 0 => x
    :"0"  (x)        // input  : x => register 0
    :"0"             // clobbered register
  );
  return x;
#else
  return bswap_64(x);
#endif
}
#else
// Helper function for swap_u8
inline u8   Bytes::swap_u8_base(u4 x, u4 y) {
  return (((u8)swap_u4(x))<<32) | swap_u4(y);
}

inline u8 Bytes::swap_u8(u8 x) {
  return swap_u8_base(*(u4*)&x, *(((u4*)&x)+1));
}
#endif

#endif
