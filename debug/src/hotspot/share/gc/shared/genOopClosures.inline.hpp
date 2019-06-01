#ifndef SHARE_VM_GC_SHARED_GENOOPCLOSURES_INLINE_HPP
#define SHARE_VM_GC_SHARED_GENOOPCLOSURES_INLINE_HPP

#include "gc/shared/cardTableRS.hpp"
#include "gc/shared/genCollectedHeap.hpp"
#include "gc/shared/genOopClosures.hpp"
#include "gc/shared/generation.hpp"
#include "gc/shared/space.hpp"
#include "oops/access.inline.hpp"
#include "oops/compressedOops.inline.hpp"
#include "oops/oop.inline.hpp"

inline OopsInGenClosure::OopsInGenClosure(Generation* gen) :
  OopIterateClosure(gen->ref_processor()), _orig_gen(gen), _rs(NULL) {
  set_generation(gen);
}

inline void OopsInGenClosure::set_generation(Generation* gen) {
  _gen = gen;
  _gen_boundary = _gen->reserved().start();
  // Barrier set for the heap, must be set after heap is initialized
  if (_rs == NULL) {
    _rs = GenCollectedHeap::heap()->rem_set();
  }
}

template <class T> inline void OopsInGenClosure::do_barrier(T* p) {
  T heap_oop = RawAccess<>::oop_load(p);
  oop obj = CompressedOops::decode_not_null(heap_oop);
  // If p points to a younger generation, mark the card.
  if ((HeapWord*)obj < _gen_boundary) {
    _rs->inline_write_ref_field_gc(p, obj);
  }
}

template <class T> inline void OopsInGenClosure::par_do_barrier(T* p) {
  T heap_oop = RawAccess<>::oop_load(p);
  oop obj = CompressedOops::decode_not_null(heap_oop);
  // If p points to a younger generation, mark the card.
  if ((HeapWord*)obj < gen_boundary()) {
    rs()->write_ref_field_gc_par(p, obj);
  }
}

inline BasicOopsInGenClosure::BasicOopsInGenClosure(Generation* gen) : OopsInGenClosure(gen) { }

inline void OopsInClassLoaderDataOrGenClosure::do_cld_barrier() {
  if (!_scanned_cld->has_modified_oops()) {
    _scanned_cld->record_modified_oops();
  }
}

template <class T> void FilteringClosure::do_oop_work(T* p) {
  T heap_oop = RawAccess<>::oop_load(p);
  if (!CompressedOops::is_null(heap_oop)) {
    oop obj = CompressedOops::decode_not_null(heap_oop);
    if ((HeapWord*)obj < _boundary) {
      _cl->do_oop(p);
    }
  }
}

inline void FilteringClosure::do_oop(oop* p) { FilteringClosure::do_oop_work(p); }
inline void FilteringClosure::do_oop(narrowOop* p) { FilteringClosure::do_oop_work(p); }

#endif
