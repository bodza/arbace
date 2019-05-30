#ifndef SHARE_VM_GC_SHARED_CARDGENERATION_INLINE_HPP
#define SHARE_VM_GC_SHARED_CARDGENERATION_INLINE_HPP

#include "gc/shared/cardGeneration.hpp"
#include "gc/shared/space.hpp"

inline size_t CardGeneration::capacity() const {
  return space()->capacity();
}

inline size_t CardGeneration::used() const {
  return space()->used();
}

inline size_t CardGeneration::free() const {
  return space()->free();
}

inline MemRegion CardGeneration::used_region() const {
  return space()->used_region();
}

inline bool CardGeneration::is_in(const void* p) const {
  return space()->is_in(p);
}

inline CompactibleSpace* CardGeneration::first_compaction_space() const {
  return space();
}

#endif
