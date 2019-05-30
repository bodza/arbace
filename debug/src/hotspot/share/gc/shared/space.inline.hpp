#ifndef SHARE_VM_GC_SHARED_SPACE_INLINE_HPP
#define SHARE_VM_GC_SHARED_SPACE_INLINE_HPP

#include "gc/shared/blockOffsetTable.inline.hpp"
#include "gc/shared/collectedHeap.hpp"
#include "gc/shared/generation.hpp"
#include "gc/shared/space.hpp"
#include "gc/shared/spaceDecorator.hpp"
#include "memory/universe.hpp"
#include "oops/oopsHierarchy.hpp"
#include "oops/oop.inline.hpp"
#include "runtime/prefetch.inline.hpp"
#include "runtime/safepoint.hpp"

inline HeapWord* Space::block_start(const void* p) {
  return block_start_const(p);
}

inline HeapWord* OffsetTableContigSpace::allocate(size_t size) {
  HeapWord* res = ContiguousSpace::allocate(size);
  if (res != NULL) {
    _offsets.alloc_block(res, size);
  }
  return res;
}

// Because of the requirement of keeping "_offsets" up to date with the
// allocations, we sequentialize these with a lock.  Therefore, best if
// this is used for larger LAB allocations only.
inline HeapWord* OffsetTableContigSpace::par_allocate(size_t size) {
  MutexLocker x(&_par_alloc_lock);
  // This ought to be just "allocate", because of the lock above, but that
  // ContiguousSpace::allocate asserts that either the allocating thread
  // holds the heap lock or it is the VM thread and we're at a safepoint.
  // The best I (dld) could figure was to put a field in ContiguousSpace
  // meaning "locking at safepoint taken care of", and set/reset that
  // here.  But this will do for now, especially in light of the comment
  // above.  Perhaps in the future some lock-free manner of keeping the
  // coordination.
  HeapWord* res = ContiguousSpace::par_allocate(size);
  if (res != NULL) {
    _offsets.alloc_block(res, size);
  }
  return res;
}

inline HeapWord*
OffsetTableContigSpace::block_start_const(const void* p) const {
  return _offsets.block_start(p);
}

size_t CompactibleSpace::obj_size(const HeapWord* addr) const {
  return oop(addr)->size();
}

size_t ContiguousSpace::scanned_block_size(const HeapWord* addr) const {
  return oop(addr)->size();
}

template <typename OopClosureType>
void ContiguousSpace::oop_since_save_marks_iterate(OopClosureType* blk) {
  HeapWord* t;
  HeapWord* p = saved_mark_word();
  assert(p != NULL, "expected saved mark");

  const intx interval = PrefetchScanIntervalInBytes;
  do {
    t = top();
    while (p < t) {
      Prefetch::write(p, interval);
      oop m = oop(p);
      p += m->oop_iterate_size(blk);
    }
  } while (t < top());

  set_saved_mark_word(p);
}

template <typename OopClosureType>
void ContiguousSpace::par_oop_iterate(MemRegion mr, OopClosureType* blk) {
  HeapWord* obj_addr = mr.start();
  HeapWord* limit = mr.end();
  while (obj_addr < limit) {
    assert(oopDesc::is_oop(oop(obj_addr)), "Should be an oop");
    obj_addr += oop(obj_addr)->oop_iterate_size(blk);
  }
}

#endif
