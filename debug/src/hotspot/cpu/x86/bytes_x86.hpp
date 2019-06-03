#ifndef CPU_X86_VM_BYTES_X86_HPP
#define CPU_X86_VM_BYTES_X86_HPP

#include "memory/allocation.hpp"
#include "utilities/align.hpp"
#include "utilities/macros.hpp"

class Bytes: AllStatic {
 private:
#ifndef AMD64
  // Helper function for swap_u8
  static inline u8   swap_u8_base(u4 x, u4 y);        // compiler-dependent implementation
#endif

 public:
  // Efficient reading and writing of unaligned unsigned data in platform-specific byte ordering
  template <typename T>
  static inline T get_native(const void* p) {
    T x;

    if (is_aligned(p, sizeof(T))) {
      x = *(T*)p;
    } else {
      memcpy(&x, p, sizeof(T));
    }

    return x;
  }

  template <typename T>
  static inline void put_native(void* p, T x) {
    if (is_aligned(p, sizeof(T))) {
      *(T*)p = x;
    } else {
      memcpy(p, &x, sizeof(T));
    }
  }

  static inline u2   get_native_u2(address p)         { return get_native<u2>((void*)p); }
  static inline u4   get_native_u4(address p)         { return get_native<u4>((void*)p); }
  static inline u8   get_native_u8(address p)         { return get_native<u8>((void*)p); }
  static inline void put_native_u2(address p, u2 x)   { put_native<u2>((void*)p, x); }
  static inline void put_native_u4(address p, u4 x)   { put_native<u4>((void*)p, x); }
  static inline void put_native_u8(address p, u8 x)   { put_native<u8>((void*)p, x); }

  // Efficient reading and writing of unaligned unsigned data in Java
  // byte ordering (i.e. big-endian ordering). Byte-order reversal is
  // needed since x86 CPUs use little-endian format.
  template <typename T>
  static inline T get_Java(const address p) {
    T x = get_native<T>(p);

    if (Endian::is_Java_byte_ordering_different()) {
      x = swap<T>(x);
    }

    return x;
  }

  template <typename T>
  static inline void put_Java(address p, T x) {
    if (Endian::is_Java_byte_ordering_different()) {
      x = swap<T>(x);
    }

    put_native<T>(p, x);
  }

  static inline u2   get_Java_u2(address p)           { return get_Java<u2>(p); }
  static inline u4   get_Java_u4(address p)           { return get_Java<u4>(p); }
  static inline u8   get_Java_u8(address p)           { return get_Java<u8>(p); }

  static inline void put_Java_u2(address p, u2 x)     { put_Java<u2>(p, x); }
  static inline void put_Java_u4(address p, u4 x)     { put_Java<u4>(p, x); }
  static inline void put_Java_u8(address p, u8 x)     { put_Java<u8>(p, x); }

  // Efficient swapping of byte ordering
  template <typename T>
  static T swap(T x) {
    switch (sizeof(T)) {
    case sizeof(u1): return x;
    case sizeof(u2): return swap_u2(x);
    case sizeof(u4): return swap_u4(x);
    case sizeof(u8): return swap_u8(x);
    default:
      guarantee(false, "invalid size: " SIZE_FORMAT "\n", sizeof(T));
      return 0;
    }
  }

  static inline u2   swap_u2(u2 x);                   // compiler-dependent implementation
  static inline u4   swap_u4(u4 x);                   // compiler-dependent implementation
  static inline u8   swap_u8(u8 x);
};

// The following header contains the implementations of swap_u2, swap_u4, and swap_u8[_base]
#include OS_CPU_HEADER_INLINE(bytes)

#endif
