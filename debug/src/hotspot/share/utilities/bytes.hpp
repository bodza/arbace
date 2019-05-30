#ifndef SHARE_VM_UTILITIES_BYTES_HPP
#define SHARE_VM_UTILITIES_BYTES_HPP

#include "utilities/macros.hpp"

class Endian : AllStatic {
public:
  enum Order {
    LITTLE,
    BIG,
    JAVA = BIG,
    NATIVE =
#ifdef VM_LITTLE_ENDIAN
    LITTLE
#else
    BIG
#endif
  };

  // Returns true, if the byte ordering used by Java is different from
  // the native byte ordering of the underlying machine.
  static inline bool is_Java_byte_ordering_different() {
    return NATIVE != JAVA;
  }
};

#include CPU_HEADER(bytes)

#endif
