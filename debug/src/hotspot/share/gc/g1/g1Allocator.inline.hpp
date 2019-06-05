#ifndef SHARE_VM_GC_G1_G1ALLOCATOR_INLINE_HPP
#define SHARE_VM_GC_G1_G1ALLOCATOR_INLINE_HPP

#include "gc/g1/g1Allocator.hpp"
#include "gc/g1/g1AllocRegion.inline.hpp"
#include "gc/shared/plab.inline.hpp"

inline MutatorAllocRegion* G1Allocator::mutator_alloc_region() {
  return &_mutator_alloc_region;
}

inline SurvivorGCAllocRegion* G1Allocator::survivor_gc_alloc_region() {
  return &_survivor_gc_alloc_region;
}

inline OldGCAllocRegion* G1Allocator::old_gc_alloc_region() {
  return &_old_gc_alloc_region;
}

inline HeapWord* G1Allocator::attempt_allocation(size_t min_word_size, size_t desired_word_size, size_t* actual_word_size) {
  HeapWord* result = mutator_alloc_region()->attempt_retained_allocation(min_word_size, desired_word_size, actual_word_size);
  if (result != NULL) {
    return result;
  }
  return mutator_alloc_region()->attempt_allocation(min_word_size, desired_word_size, actual_word_size);
}

inline HeapWord* G1Allocator::attempt_allocation_locked(size_t word_size) {
  HeapWord* result = mutator_alloc_region()->attempt_allocation_locked(word_size);
  return result;
}

inline HeapWord* G1Allocator::attempt_allocation_force(size_t word_size) {
  return mutator_alloc_region()->attempt_allocation_force(word_size);
}

inline PLAB* G1PLABAllocator::alloc_buffer(InCSetState dest) {
  return _alloc_buffers[dest.value()];
}

inline HeapWord* G1PLABAllocator::plab_allocate(InCSetState dest, size_t word_sz) {
  PLAB* buffer = alloc_buffer(dest);
  if (_survivor_alignment_bytes == 0 || !dest.is_young()) {
    return buffer->allocate(word_sz);
  } else {
    return buffer->allocate_aligned(word_sz, _survivor_alignment_bytes);
  }
}

inline HeapWord* G1PLABAllocator::allocate(InCSetState dest, size_t word_sz, bool* refill_failed) {
  HeapWord* const obj = plab_allocate(dest, word_sz);
  if (obj != NULL) {
    return obj;
  }
  return allocate_direct_or_new_plab(dest, word_sz, refill_failed);
}

// Create the maps which is used to identify archive objects.
inline void G1ArchiveAllocator::enable_archive_object_check() {
  if (_archive_check_enabled) {
    return;
  }

  _archive_check_enabled = true;
  size_t length = Universe::heap()->max_capacity();
  _closed_archive_region_map.initialize((HeapWord*)Universe::heap()->base(),
                                        (HeapWord*)Universe::heap()->base() + length,
                                        HeapRegion::GrainBytes);
  _open_archive_region_map.initialize((HeapWord*)Universe::heap()->base(),
                                      (HeapWord*)Universe::heap()->base() + length,
                                      HeapRegion::GrainBytes);
}

// Set the regions containing the specified address range as archive.
inline void G1ArchiveAllocator::set_range_archive(MemRegion range, bool open) {
  if (open) {
    _open_archive_region_map.set_by_address(range, true);
  } else {
    _closed_archive_region_map.set_by_address(range, true);
  }
}

// Clear the archive regions map containing the specified address range.
inline void G1ArchiveAllocator::clear_range_archive(MemRegion range, bool open) {
  if (open) {
    _open_archive_region_map.set_by_address(range, false);
  } else {
    _closed_archive_region_map.set_by_address(range, false);
  }
}

// Check if an object is in a closed archive region using the _archive_region_map.
inline bool G1ArchiveAllocator::in_closed_archive_range(oop object) {
  // This is the out-of-line part of is_closed_archive_object test, done separately
  // to avoid additional performance impact when the check is not enabled.
  return _closed_archive_region_map.get_by_address((HeapWord*)object);
}

inline bool G1ArchiveAllocator::in_open_archive_range(oop object) {
  return _open_archive_region_map.get_by_address((HeapWord*)object);
}

// Check if archive object checking is enabled, to avoid calling in_open/closed_archive_range
// unnecessarily.
inline bool G1ArchiveAllocator::archive_check_enabled() {
  return _archive_check_enabled;
}

inline bool G1ArchiveAllocator::is_closed_archive_object(oop object) {
  return (archive_check_enabled() && in_closed_archive_range(object));
}

inline bool G1ArchiveAllocator::is_open_archive_object(oop object) {
  return (archive_check_enabled() && in_open_archive_range(object));
}

inline bool G1ArchiveAllocator::is_archive_object(oop object) {
  return (archive_check_enabled() && (in_closed_archive_range(object) || in_open_archive_range(object)));
}

#endif
