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
  size_t delta = pointer_delta(pc, _reserved.start(), sizeof(char));
  size_t result = delta >> BOTConstants::LogN;
  return result;
}

inline HeapWord* BlockOffsetSharedArray::address_for_index(size_t index) const {
  HeapWord* result = _reserved.start() + (index << BOTConstants::LogN_words);
  return result;
}

inline void BlockOffsetSharedArray::check_reducing_assertion(bool reducing) { }

//////////////////////////////////////////////////////////////////////////
// BlockOffsetArrayNonContigSpace inlines
//////////////////////////////////////////////////////////////////////////
inline void BlockOffsetArrayNonContigSpace::freed(HeapWord* blk, size_t size) {
  freed(blk, blk + size);
}

inline void BlockOffsetArrayNonContigSpace::freed(HeapWord* blk_start, HeapWord* blk_end) {
  // Verify that the BOT shows [blk_start, blk_end) to be one block.
  verify_single_block(blk_start, blk_end);
  // adjust _unallocated_block upward or downward
  // as appropriate
  if (BlockOffsetArrayUseUnallocatedBlock) {
    if (blk_end >= _unallocated_block && blk_start <= _unallocated_block) {
      // CMS-specific note: a block abutting _unallocated_block to
      // its left is being freed, a new block is being added or
      // we are resetting following a compaction
      _unallocated_block = blk_start;
    }
  }
}

#endif
