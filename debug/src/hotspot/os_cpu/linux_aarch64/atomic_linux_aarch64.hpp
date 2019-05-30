#ifndef OS_CPU_LINUX_AARCH64_VM_ATOMIC_LINUX_AARCH64_HPP
#define OS_CPU_LINUX_AARCH64_VM_ATOMIC_LINUX_AARCH64_HPP

#include "vm_version_aarch64.hpp"

// Implementation of class atomic

#define FULL_MEM_BARRIER  __sync_synchronize()
#define READ_MEM_BARRIER  __atomic_thread_fence(__ATOMIC_ACQUIRE);
#define WRITE_MEM_BARRIER __atomic_thread_fence(__ATOMIC_RELEASE);

template<size_t byte_size>
struct Atomic::PlatformAdd
  : Atomic::AddAndFetch<Atomic::PlatformAdd<byte_size> >
{
  template<typename I, typename D>
  D add_and_fetch(I add_value, D volatile* dest, atomic_memory_order order) const {
    return __sync_add_and_fetch(dest, add_value);
  }
};

template<size_t byte_size>
template<typename T>
inline T Atomic::PlatformXchg<byte_size>::operator()(T exchange_value,
                                                     T volatile* dest,
                                                     atomic_memory_order order) const {
  STATIC_ASSERT(byte_size == sizeof(T));
  T res = __sync_lock_test_and_set(dest, exchange_value);
  FULL_MEM_BARRIER;
  return res;
}

template<size_t byte_size>
template<typename T>
inline T Atomic::PlatformCmpxchg<byte_size>::operator()(T exchange_value,
                                                        T volatile* dest,
                                                        T compare_value,
                                                        atomic_memory_order order) const {
  STATIC_ASSERT(byte_size == sizeof(T));
  if (order == memory_order_relaxed) {
    T value = compare_value;
    __atomic_compare_exchange(dest, &value, &exchange_value, /*weak*/false,
                              __ATOMIC_RELAXED, __ATOMIC_RELAXED);
    return value;
  } else {
    return __sync_val_compare_and_swap(dest, compare_value, exchange_value);
  }
}

#endif
