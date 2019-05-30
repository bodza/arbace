#ifndef SHARE_VM_GC_SHARED_BLOCKOFFSETTABLE_INLINE_HPP
#define SHARE_VM_GC_SHARED_BLOCKOFFSETTABLE_INLINE_HPP

#include "gc/shared/blockOffsetTable.hpp"
#include "gc/shared/space.hpp"
#include "runtime/safepoint.hpp"

//////////////////////////////////////////////////////////////////////////
// BlockOffsetTable inlines
//////////////////////////////////////////////////////////////////////////
inline HeapWord* BlockOffsetTable::block_start(const void* addr) const {
  if (addr >= _bottom && addr < _end) {
    return block_start_unsafe(addr);
  } else {
    return NULL;
  }
}

//////////////////////////////////////////////////////////////////////////
// BlockOffsetSharedArray inlines
//////////////////////////////////////////////////////////////////////////
inline size_t BlockOffsetSharedArray::index_for(const void* p) const {
  char* pc = (char*)p;
  assert(pc >= (char*)_reserved.start() && pc <  (char*)_reserved.end(), "p not in range.");
  size_t delta = pointer_delta(pc, _reserved.start(), sizeof(char));
  size_t result = delta >> BOTConstants::LogN;
  assert(result < _vs.committed_size(), "bad index from address");
  return result;
}

inline HeapWord* BlockOffsetSharedArray::address_for_index(size_t index) const {
  assert(index < _vs.committed_size(), "bad index");
  HeapWord* result = _reserved.start() + (index << BOTConstants::LogN_words);
  assert(result >= _reserved.start() && result < _reserved.end(), "bad address from index");
  return result;
}

inline void BlockOffsetSharedArray::check_reducing_assertion(bool reducing) {
    assert(reducing || !SafepointSynchronize::is_at_safepoint() || init_to_zero() || Thread::current()->is_VM_thread() || Thread::current()->is_ConcurrentGC_thread() || ((!Thread::current()->is_ConcurrentGC_thread()) && ParGCRareEvent_lock->owned_by_self()), "Crack");
}

//////////////////////////////////////////////////////////////////////////
// BlockOffsetArrayNonContigSpace inlines
//////////////////////////////////////////////////////////////////////////
inline void BlockOffsetArrayNonContigSpace::freed(HeapWord* blk,
                                                  size_t size) {
  freed(blk, blk + size);
}

inline void BlockOffsetArrayNonContigSpace::freed(HeapWord* blk_start,
                                                  HeapWord* blk_end) {
  // Verify that the BOT shows [blk_start, blk_end) to be one block.
  verify_single_block(blk_start, blk_end);
  // adjust _unallocated_block upward or downward
  // as appropriate
  if (BlockOffsetArrayUseUnallocatedBlock) {
    assert(_unallocated_block <= _end, "Inconsistent value for _unallocated_block");
    if (blk_end >= _unallocated_block && blk_start <= _unallocated_block) {
      // CMS-specific note: a block abutting _unallocated_block to
      // its left is being freed, a new block is being added or
      // we are resetting following a compaction
      _unallocated_block = blk_start;
    }
  }
}

#endif
