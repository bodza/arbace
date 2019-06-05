#include "precompiled.hpp"

#include "memory/allocation.hpp"
#include "memory/allocation.inline.hpp"
#include "memory/arena.hpp"
#include "memory/resourceArea.hpp"
#include "memory/universe.hpp"
#include "runtime/atomic.hpp"
#include "runtime/os.hpp"
#include "runtime/task.hpp"
#include "runtime/threadCritical.hpp"
#include "utilities/ostream.hpp"

// allocate using malloc; will fail if no memory available
char* AllocateHeap(size_t size, MEMFLAGS flags, AllocFailType alloc_failmode /* = AllocFailStrategy::EXIT_OOM*/) {
  char* p = (char*) os::malloc(size, flags);
  if (p == NULL && alloc_failmode == AllocFailStrategy::EXIT_OOM) {
    vm_exit_out_of_memory(size, OOM_MALLOC_ERROR, "AllocateHeap");
  }
  return p;
}

char* ReallocateHeap(char *old, size_t size, MEMFLAGS flag, AllocFailType alloc_failmode) {
  char* p = (char*) os::realloc(old, size, flag);
  if (p == NULL && alloc_failmode == AllocFailStrategy::EXIT_OOM) {
    vm_exit_out_of_memory(size, OOM_MALLOC_ERROR, "ReallocateHeap");
  }
  return p;
}

void FreeHeap(void* p) {
  os::free(p);
}

void* MetaspaceObj::_shared_metaspace_base = NULL;
void* MetaspaceObj::_shared_metaspace_top  = NULL;

void* StackObj::operator new(size_t size) throw() { ShouldNotCallThis(); return 0; }
void StackObj::operator delete(void* p) { ShouldNotCallThis(); }
void* StackObj::operator new [](size_t size) throw() { ShouldNotCallThis(); return 0; }
void StackObj::operator delete [](void* p) { ShouldNotCallThis(); }

void* MetaspaceObj::operator new(size_t size, ClassLoaderData* loader_data, size_t word_size, MetaspaceObj::Type type, TRAPS) throw() {
  // Klass has it's own operator new
  return Metaspace::allocate(loader_data, word_size, type, THREAD);
}

bool MetaspaceObj::is_metaspace_object() const {
  return Metaspace::contains((void*)this);
}

void MetaspaceObj::print_address_on(outputStream* st) const {
  st->print(" {" INTPTR_FORMAT "}", p2i(this));
}

void* ResourceObj::operator new(size_t size, Arena *arena) throw() {
  address res = (address)arena->Amalloc(size);
  return res;
}

void* ResourceObj::operator new [](size_t size, Arena *arena) throw() {
  address res = (address)arena->Amalloc(size);
  return res;
}

void* ResourceObj::operator new(size_t size, allocation_type type, MEMFLAGS flags) throw() {
  address res = NULL;
  switch (type) {
   case C_HEAP:
    res = (address)AllocateHeap(size, flags);
    break;
   case RESOURCE_AREA:
    // new(size) sets allocation type RESOURCE_AREA.
    res = (address)operator new(size);
    break;
   default:
    ShouldNotReachHere();
  }
  return res;
}

void* ResourceObj::operator new [](size_t size, allocation_type type, MEMFLAGS flags) throw() {
  return (address) operator new(size, type, flags);
}

void* ResourceObj::operator new(size_t size, const std::nothrow_t&  nothrow_constant,
    allocation_type type, MEMFLAGS flags) throw() {
  // should only call this with std::nothrow, use other operator new() otherwise
  address res = NULL;
  switch (type) {
   case C_HEAP:
    res = (address)AllocateHeap(size, flags, AllocFailStrategy::RETURN_NULL);
    break;
   case RESOURCE_AREA:
    // new(size) sets allocation type RESOURCE_AREA.
    res = (address)operator new(size, std::nothrow);
    break;
   default:
    ShouldNotReachHere();
  }
  return res;
}

void* ResourceObj::operator new [](size_t size, const std::nothrow_t&  nothrow_constant,
    allocation_type type, MEMFLAGS flags) throw() {
  return (address)operator new(size, nothrow_constant, type, flags);
}

void ResourceObj::operator delete(void* p) {
  FreeHeap(p);
}

void ResourceObj::operator delete [](void* p) {
  operator delete(p);
}
