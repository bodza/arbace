#ifndef SHARE_VM_SERVICES_MEMORYUSAGE_HPP
#define SHARE_VM_SERVICES_MEMORYUSAGE_HPP

#include "utilities/globalDefinitions.hpp"

// A memory usage contains the following attributes about memory usage:
//  initSize - represents the initial amount of memory (in bytes) that
//     the Java virtual machine requests from the operating system
//     for memory management.  The Java virtual machine may request
//     additional memory from the operating system later when appropriate.
//     Its value may be undefined.
//  used      - represents the amount of memory currently used (in bytes).
//  committed - represents the amount of memory (in bytes) that is
//     guaranteed to be available for use by the Java virtual machine.
//     The amount of committed memory may change over time (increase
//     or decrease).  It is guaranteed to be greater than or equal
//     to initSize.
//  maxSize   - represents the maximum amount of memory (in bytes)
//     that can be used for memory management. The maximum amount of
//     memory for memory management could be less than the amount of
//     committed memory.  Its value may be undefined.

class MemoryUsage {
private:
  size_t _initSize;
  size_t _used;
  size_t _committed;
  size_t _maxSize;

public:
  // Constructors
  MemoryUsage(size_t i, size_t u, size_t c, size_t m) :
    _initSize(i), _used(u), _committed(c), _maxSize(m) {};
  MemoryUsage() :
    _initSize(0), _used(0), _committed(0), _maxSize(0) {};

  size_t init_size() const { return _initSize; }
  size_t used()      const { return _used; }
  size_t committed() const { return _committed; }
  size_t max_size()  const { return _maxSize; }

  static size_t undefined_size() { return (size_t) -1; }

  inline static jlong convert_to_jlong(size_t val) {
    // In the 64-bit vm, a size_t can overflow a jlong (which is signed).
    jlong ret;
    if (val == undefined_size()) {
      ret = -1L;
    } else {
      ret = MIN2(val, (size_t)max_jlong);
    }
    return ret;
  }

  jlong init_size_as_jlong() const { return convert_to_jlong(_initSize); }
  jlong used_as_jlong()      const { return convert_to_jlong(_used); }
  jlong committed_as_jlong() const { return convert_to_jlong(_committed); }
  jlong max_size_as_jlong()  const { return convert_to_jlong(_maxSize); }
};

#endif
