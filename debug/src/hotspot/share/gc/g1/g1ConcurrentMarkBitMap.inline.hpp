#ifndef SHARE_VM_GC_G1_G1CONCURRENTMARKBITMAP_INLINE_HPP
#define SHARE_VM_GC_G1_G1CONCURRENTMARKBITMAP_INLINE_HPP

#include "gc/g1/g1ConcurrentMarkBitMap.hpp"
#include "memory/memRegion.hpp"
#include "utilities/align.hpp"
#include "utilities/bitMap.inline.hpp"

inline bool G1CMBitMap::iterate(G1CMBitMapClosure* cl, MemRegion mr) {
  BitMap::idx_t const end_offset = addr_to_offset(mr.end());
  BitMap::idx_t offset = _bm.get_next_one_offset(addr_to_offset(mr.start()), end_offset);

  while (offset < end_offset) {
    HeapWord* const addr = offset_to_addr(offset);
    if (!cl->do_addr(addr)) {
      return false;
    }
    size_t const obj_size = (size_t)((oop)addr)->size();
    offset = _bm.get_next_one_offset(offset + (obj_size >> _shifter), end_offset);
  }
  return true;
}

inline HeapWord* G1CMBitMap::get_next_marked_addr(const HeapWord* addr,
                                                  const HeapWord* limit) const {
  // Round addr up to a possible object boundary to be safe.
  size_t const addr_offset = addr_to_offset(align_up(addr, HeapWordSize << _shifter));
  size_t const limit_offset = addr_to_offset(limit);
  size_t const nextOffset = _bm.get_next_one_offset(addr_offset, limit_offset);
  return offset_to_addr(nextOffset);
}

inline void G1CMBitMap::mark(HeapWord* addr) {
  check_mark(addr);
  _bm.set_bit(addr_to_offset(addr));
}

inline void G1CMBitMap::clear(HeapWord* addr) {
  check_mark(addr);
  _bm.clear_bit(addr_to_offset(addr));
}

inline bool G1CMBitMap::par_mark(HeapWord* addr) {
  check_mark(addr);
  return _bm.par_set_bit(addr_to_offset(addr));
}

inline bool G1CMBitMap::par_mark(oop obj) {
  return par_mark((HeapWord*) obj);
}

inline bool G1CMBitMap::is_marked(oop obj) const{
  return is_marked((HeapWord*) obj);
}

inline void G1CMBitMap::clear(oop obj) {
  clear((HeapWord*) obj);
}

#endif
