#ifndef SHARE_VM_GC_G1_G1BLOCKOFFSETTABLE_INLINE_HPP
#define SHARE_VM_GC_G1_G1BLOCKOFFSETTABLE_INLINE_HPP

#include "gc/g1/g1BlockOffsetTable.hpp"
#include "gc/g1/heapRegion.hpp"
#include "gc/shared/memset_with_concurrent_readers.hpp"
#include "gc/shared/space.hpp"

inline HeapWord* G1BlockOffsetTablePart::block_start(const void* addr) {
  if (addr >= _space->bottom() && addr < _space->end()) {
    HeapWord* q = block_at_or_preceding(addr, true, _next_offset_index-1);
    return forward_to_block_containing_addr(q, addr);
  } else {
    return NULL;
  }
}

inline HeapWord* G1BlockOffsetTablePart::block_start_const(const void* addr) const {
  if (addr >= _space->bottom() && addr < _space->end()) {
    HeapWord* q = block_at_or_preceding(addr, true, _next_offset_index-1);
    HeapWord* n = q + block_size(q);
    return forward_to_block_containing_addr_const(q, n, addr);
  } else {
    return NULL;
  }
}

u_char G1BlockOffsetTable::offset_array(size_t index) const {
  check_index(index, "index out of range");
  return _offset_array[index];
}

void G1BlockOffsetTable::set_offset_array(size_t index, u_char offset) {
  check_index(index, "index out of range");
  set_offset_array_raw(index, offset);
}

void G1BlockOffsetTable::set_offset_array(size_t index, HeapWord* high, HeapWord* low) {
  check_index(index, "index out of range");
  size_t offset = pointer_delta(high, low);
  check_offset(offset, "offset too large");
  set_offset_array(index, (u_char)offset);
}

void G1BlockOffsetTable::set_offset_array(size_t left, size_t right, u_char offset) {
  check_index(right, "right index out of range");
  size_t num_cards = right - left + 1;
  memset_with_concurrent_readers(&_offset_array[left], offset, num_cards);
}

// Variant of index_for that does not check the index for validity.
inline size_t G1BlockOffsetTable::index_for_raw(const void* p) const {
  return pointer_delta((char*)p, _reserved.start(), sizeof(char)) >> BOTConstants::LogN;
}

inline size_t G1BlockOffsetTable::index_for(const void* p) const {
  char* pc = (char*)p;
  size_t result = index_for_raw(p);
  check_index(result, "bad index from address");
  return result;
}

inline HeapWord* G1BlockOffsetTable::address_for_index(size_t index) const {
  check_index(index, "index out of range");
  HeapWord* result = address_for_index_raw(index);
  return result;
}

inline size_t G1BlockOffsetTablePart::block_size(const HeapWord* p) const {
  return _space->block_size(p);
}

inline HeapWord* G1BlockOffsetTablePart::block_at_or_preceding(const void* addr,
                                                               bool has_max_index,
                                                               size_t max_index) const {
  size_t index = _bot->index_for(addr);
  // We must make sure that the offset table entry we use is valid.  If
  // "addr" is past the end, start at the last known one and go forward.
  if (has_max_index) {
    index = MIN2(index, max_index);
  }
  HeapWord* q = _bot->address_for_index(index);

  uint offset = _bot->offset_array(index);  // Extend u_char to uint.
  while (offset >= BOTConstants::N_words) {
    // The excess of the offset from N_words indicates a power of Base
    // to go back by.
    size_t n_cards_back = BOTConstants::entry_to_cards_back(offset);
    q -= (BOTConstants::N_words * n_cards_back);
    index -= n_cards_back;
    offset = _bot->offset_array(index);
  }
  q -= offset;
  return q;
}

inline HeapWord* G1BlockOffsetTablePart::forward_to_block_containing_addr_const(HeapWord* q, HeapWord* n,
                                                                                const void* addr) const {
  if (addr >= _space->top()) return _space->top();
  while (n <= addr) {
    q = n;
    oop obj = oop(q);
    if (obj->klass_or_null_acquire() == NULL) {
      return q;
    }
    n += block_size(q);
  }
  return q;
}

inline HeapWord* G1BlockOffsetTablePart::forward_to_block_containing_addr(HeapWord* q,
                                                                          const void* addr) {
  if (oop(q)->klass_or_null_acquire() == NULL) {
    return q;
  }
  HeapWord* n = q + block_size(q);
  // In the normal case, where the query "addr" is a card boundary, and the
  // offset table chunks are the same size as cards, the block starting at
  // "q" will contain addr, so the test below will fail, and we'll fall
  // through quickly.
  if (n <= addr) {
    q = forward_to_block_containing_addr_slow(q, n, addr);
  }
  return q;
}

#endif
