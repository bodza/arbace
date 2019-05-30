#ifndef OS_CPU_LINUX_AARCH64_VM_ORDERACCESS_LINUX_AARCH64_HPP
#define OS_CPU_LINUX_AARCH64_VM_ORDERACCESS_LINUX_AARCH64_HPP

// Included in orderAccess.hpp header file.

#include "vm_version_aarch64.hpp"

// Implementation of class OrderAccess.

inline void OrderAccess::loadload()   { acquire(); }
inline void OrderAccess::storestore() { release(); }
inline void OrderAccess::loadstore()  { acquire(); }
inline void OrderAccess::storeload()  { fence(); }

inline void OrderAccess::acquire() {
  READ_MEM_BARRIER;
}

inline void OrderAccess::release() {
  WRITE_MEM_BARRIER;
}

inline void OrderAccess::fence() {
  FULL_MEM_BARRIER;
}

template<size_t byte_size>
struct OrderAccess::PlatformOrderedLoad<byte_size, X_ACQUIRE>
{
  template <typename T>
  T operator()(const volatile T* p) const { T data; __atomic_load(p, &data, __ATOMIC_ACQUIRE); return data; }
};

template<size_t byte_size>
struct OrderAccess::PlatformOrderedStore<byte_size, RELEASE_X>
{
  template <typename T>
  void operator()(T v, volatile T* p) const { __atomic_store(p, &v, __ATOMIC_RELEASE); }
};

template<size_t byte_size>
struct OrderAccess::PlatformOrderedStore<byte_size, RELEASE_X_FENCE>
{
  template <typename T>
  void operator()(T v, volatile T* p) const { release_store(p, v); fence(); }
};

#endif
