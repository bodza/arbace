#ifndef SHARE_GC_SHARED_MEM_ALLOCATOR_HPP
#define SHARE_GC_SHARED_MEM_ALLOCATOR_HPP

#include "gc/shared/collectedHeap.hpp"
#include "memory/memRegion.hpp"
#include "oops/oopsHierarchy.hpp"
#include "utilities/exceptions.hpp"
#include "utilities/macros.hpp"

// These fascilities are used for allocating, and initializing newly allocated objects.

class MemAllocator: StackObj {
  class Allocation;

protected:
  CollectedHeap* const _heap;
  Thread* const        _thread;
  Klass* const         _klass;
  const size_t         _word_size;

private:
  // Allocate from the current thread's TLAB, with broken-out slow path.
  HeapWord* allocate_inside_tlab(Allocation& allocation) const;
  HeapWord* allocate_inside_tlab_slow(Allocation& allocation) const;
  HeapWord* allocate_outside_tlab(Allocation& allocation) const;

protected:
  MemAllocator(Klass* klass, size_t word_size, Thread* thread)
    : _heap(Universe::heap()),
      _thread(thread),
      _klass(klass),
      _word_size(word_size)
  { }

  // This function clears the memory of the object
  void mem_clear(HeapWord* mem) const;
  // This finish constructing an oop by installing the mark word and the Klass* pointer
  // last. At the point when the Klass pointer is initialized, this is a constructed object
  // that must be parseable as an oop by concurrent collectors.
  oop finish(HeapWord* mem) const;

  // Raw memory allocation. This may or may not use TLAB allocations to satisfy the
  // allocation. A GC implementation may override this function to satisfy the allocation
  // in any way. But the default is to try a TLAB allocation, and otherwise perform
  // mem_allocate.
  virtual HeapWord* mem_allocate(Allocation& allocation) const;

  virtual MemRegion obj_memory_range(oop obj) const {
    return MemRegion((HeapWord*)obj, _word_size);
  }

public:
  oop allocate() const;
  virtual oop initialize(HeapWord* mem) const = 0;
};

class ObjAllocator: public MemAllocator {
public:
  ObjAllocator(Klass* klass, size_t word_size, Thread* thread = Thread::current())
    : MemAllocator(klass, word_size, thread) {}
  virtual oop initialize(HeapWord* mem) const;
};

class ObjArrayAllocator: public MemAllocator {
  const int  _length;
  const bool _do_zero;
protected:
  virtual MemRegion obj_memory_range(oop obj) const;

public:
  ObjArrayAllocator(Klass* klass, size_t word_size, int length, bool do_zero,
                    Thread* thread = Thread::current())
    : MemAllocator(klass, word_size, thread),
      _length(length),
      _do_zero(do_zero) {}
  virtual oop initialize(HeapWord* mem) const;
};

class ClassAllocator: public MemAllocator {
public:
  ClassAllocator(Klass* klass, size_t word_size, Thread* thread = Thread::current())
    : MemAllocator(klass, word_size, thread) {}
  virtual oop initialize(HeapWord* mem) const;
};

#endif
