#ifndef CPU_AARCH64_VM_BYTES_AARCH64_HPP
#define CPU_AARCH64_VM_BYTES_AARCH64_HPP

#include "memory/allocation.hpp"

class Bytes: AllStatic {
 public:
  // Efficient reading and writing of unaligned unsigned data in platform-specific byte ordering
  // (no special code is needed since x86 CPUs can access unaligned data)
  static inline u2   get_native_u2(address p)         { return *(u2*)p; }
  static inline u4   get_native_u4(address p)         { return *(u4*)p; }
  static inline u8   get_native_u8(address p)         { return *(u8*)p; }

  static inline void put_native_u2(address p, u2 x)   { *(u2*)p = x; }
  static inline void put_native_u4(address p, u4 x)   { *(u4*)p = x; }
  static inline void put_native_u8(address p, u8 x)   { *(u8*)p = x; }

  // Efficient reading and writing of unaligned unsigned data in Java
  // byte ordering (i.e. big-endian ordering). Byte-order reversal is
  // needed since x86 CPUs use little-endian format.
  static inline u2   get_Java_u2(address p)           { return swap_u2(get_native_u2(p)); }
  static inline u4   get_Java_u4(address p)           { return swap_u4(get_native_u4(p)); }
  static inline u8   get_Java_u8(address p)           { return swap_u8(get_native_u8(p)); }

  static inline void put_Java_u2(address p, u2 x)     { put_native_u2(p, swap_u2(x)); }
  static inline void put_Java_u4(address p, u4 x)     { put_native_u4(p, swap_u4(x)); }
  static inline void put_Java_u8(address p, u8 x)     { put_native_u8(p, swap_u8(x)); }

  // Efficient swapping of byte ordering
  static inline u2   swap_u2(u2 x);                   // compiler-dependent implementation
  static inline u4   swap_u4(u4 x);                   // compiler-dependent implementation
  static inline u8   swap_u8(u8 x);
};

// The following header contains the implementations of swap_u2, swap_u4, and swap_u8[_base]
#include OS_CPU_HEADER_INLINE(bytes)

#endif
