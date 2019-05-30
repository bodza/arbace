#ifndef OS_CPU_LINUX_ZERO_VM_BYTES_LINUX_ZERO_INLINE_HPP
#define OS_CPU_LINUX_ZERO_VM_BYTES_LINUX_ZERO_INLINE_HPP

// Efficient swapping of data bytes from Java byte
// ordering to native byte ordering and vice versa.

#include <byteswap.h>

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
