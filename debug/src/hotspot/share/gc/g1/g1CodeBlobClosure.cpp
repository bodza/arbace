#include "precompiled.hpp"
#include "code/nmethod.hpp"
#include "gc/g1/g1CodeBlobClosure.hpp"
#include "gc/g1/g1CollectedHeap.inline.hpp"
#include "gc/g1/heapRegion.hpp"
#include "gc/g1/heapRegionRemSet.hpp"
#include "oops/access.inline.hpp"
#include "oops/compressedOops.inline.hpp"
#include "oops/oop.inline.hpp"

template <typename T>
void G1CodeBlobClosure::HeapRegionGatheringOopClosure::do_oop_work(T* p) {
  _work->do_oop(p);
  T oop_or_narrowoop = RawAccess<>::oop_load(p);
  if (!CompressedOops::is_null(oop_or_narrowoop)) {
    oop o = CompressedOops::decode_not_null(oop_or_narrowoop);
    HeapRegion* hr = _g1h->heap_region_containing(o);
    hr->add_strong_code_root(_nm);
  }
}

void G1CodeBlobClosure::HeapRegionGatheringOopClosure::do_oop(oop* o) {
  do_oop_work(o);
}

void G1CodeBlobClosure::HeapRegionGatheringOopClosure::do_oop(narrowOop* o) {
  do_oop_work(o);
}

void G1CodeBlobClosure::do_code_blob(CodeBlob* cb) {
  nmethod* nm = cb->as_nmethod_or_null();
  if (nm != NULL) {
    if (!nm->test_set_oops_do_mark()) {
      _oc.set_nm(nm);
      nm->oops_do(&_oc);
      nm->fix_oop_relocations();
    }
  }
}
