#ifndef SHARE_VM_MEMORY_ALLOCATION_INLINE_HPP
#define SHARE_VM_MEMORY_ALLOCATION_INLINE_HPP

#include "runtime/atomic.hpp"
#include "runtime/os.hpp"
#include "services/memTracker.hpp"
#include "utilities/align.hpp"
#include "utilities/globalDefinitions.hpp"

// Explicit C-heap memory management

template <class E>
size_t MmapArrayAllocator<E>::size_for(size_t length) {
  size_t size = length * sizeof(E);
  int alignment = os::vm_allocation_granularity();
  return align_up(size, alignment);
}

template <class E>
E* MmapArrayAllocator<E>::allocate_or_null(size_t length, MEMFLAGS flags) {
  size_t size = size_for(length);
  int alignment = os::vm_allocation_granularity();

  char* addr = os::reserve_memory(size, NULL, alignment, flags);
  if (addr == NULL) {
    return NULL;
  }

  if (os::commit_memory(addr, size, !ExecMem)) {
    return (E*)addr;
  } else {
    os::release_memory(addr, size);
    return NULL;
  }
}

template <class E>
E* MmapArrayAllocator<E>::allocate(size_t length, MEMFLAGS flags) {
  size_t size = size_for(length);
  int alignment = os::vm_allocation_granularity();

  char* addr = os::reserve_memory(size, NULL, alignment, flags);
  if (addr == NULL) {
    vm_exit_out_of_memory(size, OOM_MMAP_ERROR, "Allocator (reserve)");
  }

  os::commit_memory_or_exit(addr, size, !ExecMem, "Allocator (commit)");

  return (E*)addr;
}

template <class E>
void MmapArrayAllocator<E>::free(E* addr, size_t length) {
  bool result = os::release_memory((char*)addr, size_for(length));
}

template <class E>
size_t MallocArrayAllocator<E>::size_for(size_t length) {
  return length * sizeof(E);
}

template <class E>
E* MallocArrayAllocator<E>::allocate(size_t length, MEMFLAGS flags) {
  return (E*)AllocateHeap(size_for(length), flags);
}

template<class E>
void MallocArrayAllocator<E>::free(E* addr) {
  FreeHeap(addr);
}

template <class E>
bool ArrayAllocator<E>::should_use_malloc(size_t length) {
  return MallocArrayAllocator<E>::size_for(length) < ArrayAllocatorMallocLimit;
}

template <class E>
E* ArrayAllocator<E>::allocate_malloc(size_t length, MEMFLAGS flags) {
  return MallocArrayAllocator<E>::allocate(length, flags);
}

template <class E>
E* ArrayAllocator<E>::allocate_mmap(size_t length, MEMFLAGS flags) {
  return MmapArrayAllocator<E>::allocate(length, flags);
}

template <class E>
E* ArrayAllocator<E>::allocate(size_t length, MEMFLAGS flags) {
  if (should_use_malloc(length)) {
    return allocate_malloc(length, flags);
  }

  return allocate_mmap(length, flags);
}

template <class E>
E* ArrayAllocator<E>::reallocate(E* old_addr, size_t old_length, size_t new_length, MEMFLAGS flags) {
  E* new_addr = (new_length > 0)
      ? allocate(new_length, flags)
      : NULL;

  if (new_addr != NULL && old_addr != NULL) {
    memcpy(new_addr, old_addr, MIN2(old_length, new_length) * sizeof(E));
  }

  if (old_addr != NULL) {
    free(old_addr, old_length);
  }

  return new_addr;
}

template<class E>
void ArrayAllocator<E>::free_malloc(E* addr, size_t length) {
  MallocArrayAllocator<E>::free(addr);
}

template<class E>
void ArrayAllocator<E>::free_mmap(E* addr, size_t length) {
  MmapArrayAllocator<E>::free(addr, length);
}

template<class E>
void ArrayAllocator<E>::free(E* addr, size_t length) {
  if (addr != NULL) {
    if (should_use_malloc(length)) {
      free_malloc(addr, length);
    } else {
      free_mmap(addr, length);
    }
  }
}

#endif
