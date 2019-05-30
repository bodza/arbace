#ifndef OS_CPU_BSD_ZERO_VM_BYTES_BSD_ZERO_INLINE_HPP
#define OS_CPU_BSD_ZERO_VM_BYTES_BSD_ZERO_INLINE_HPP

// Efficient swapping of data bytes from Java byte
// ordering to native byte ordering and vice versa.

#ifdef __APPLE__
#  include <libkern/OSByteOrder.h>
#else
#  include <sys/endian.h>
#endif

#if defined(__APPLE__)
#  define bswap_16(x)   OSSwapInt16(x)
#  define bswap_32(x)   OSSwapInt32(x)
#  define bswap_64(x)   OSSwapInt64(x)
#elif defined(__OpenBSD__)
#  define bswap_16(x)   swap16(x)
#  define bswap_32(x)   swap32(x)
#  define bswap_64(x)   swap64(x)
#elif defined(__NetBSD__)
#  define bswap_16(x)   bswap16(x)
#  define bswap_32(x)   bswap32(x)
#  define bswap_64(x)   bswap64(x)
#else
#  define bswap_16(x) __bswap16(x)
#  define bswap_32(x) __bswap32(x)
#  define bswap_64(x) __bswap64(x)
#endif

inline u2 Bytes::swap_u2(u2 x) {
  return bswap_16(x);
}

inline u4 Bytes::swap_u4(u4 x) {
  return bswap_32(x);
}

inline u8 Bytes::swap_u8(u8 x) {
  return bswap_64(x);
}

#endif
