#ifndef SHARE_VM_GC_G1_G1FULLGCOOPCLOSURES_INLINE_HPP
#define SHARE_VM_GC_G1_G1FULLGCOOPCLOSURES_INLINE_HPP

#include "gc/g1/g1Allocator.hpp"
#include "gc/g1/g1ConcurrentMarkBitMap.inline.hpp"
#include "gc/g1/g1FullGCMarker.inline.hpp"
#include "gc/g1/g1FullGCOopClosures.hpp"
#include "gc/g1/heapRegionRemSet.hpp"
#include "memory/iterator.inline.hpp"
#include "oops/access.inline.hpp"
#include "oops/compressedOops.inline.hpp"
#include "oops/oop.inline.hpp"

template <typename T>
inline void G1MarkAndPushClosure::do_oop_work(T* p) {
  _marker->mark_and_push(p);
}

inline void G1MarkAndPushClosure::do_oop(oop* p) {
  do_oop_work(p);
}

inline void G1MarkAndPushClosure::do_oop(narrowOop* p) {
  do_oop_work(p);
}

inline bool G1MarkAndPushClosure::do_metadata() {
  return true;
}

inline void G1MarkAndPushClosure::do_klass(Klass* k) {
  _marker->follow_klass(k);
}

inline void G1MarkAndPushClosure::do_cld(ClassLoaderData* cld) {
  _marker->follow_cld(cld);
}

template <class T> inline void G1AdjustClosure::adjust_pointer(T* p) {
  T heap_oop = RawAccess<>::oop_load(p);
  if (CompressedOops::is_null(heap_oop)) {
    return;
  }

  oop obj = CompressedOops::decode_not_null(heap_oop);
  if (G1ArchiveAllocator::is_archive_object(obj)) {
    // We never forward archive objects.
    return;
  }

  oop forwardee = obj->forwardee();
  if (forwardee == NULL) {
    // Not forwarded, return current reference.
    // Correct mark // Will be restored by PreservedMarksSet // Will be restored by BiasedLocking
    return;
  }

  // Forwarded, just update.
  RawAccess<IS_NOT_NULL>::oop_store(p, forwardee);
}

inline void G1AdjustClosure::do_oop(oop* p) { do_oop_work(p); }
inline void G1AdjustClosure::do_oop(narrowOop* p) { do_oop_work(p); }

inline bool G1IsAliveClosure::do_object_b(oop p) {
  return _bitmap->is_marked(p) || G1ArchiveAllocator::is_closed_archive_object(p);
}

template<typename T>
inline void G1FullKeepAliveClosure::do_oop_work(T* p) {
  _marker->mark_and_push(p);
}

#endif
