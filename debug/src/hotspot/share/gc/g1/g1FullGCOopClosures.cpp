#include "precompiled.hpp"

#include "gc/g1/g1CollectedHeap.hpp"
#include "gc/g1/g1FullGCMarker.inline.hpp"
#include "gc/g1/g1FullGCOopClosures.inline.hpp"
#include "memory/iterator.inline.hpp"
#include "oops/access.inline.hpp"
#include "oops/compressedOops.inline.hpp"
#include "oops/oop.inline.hpp"

void G1FollowStackClosure::do_void() { _marker->drain_stack(); }

void G1FullKeepAliveClosure::do_oop(oop* p) { do_oop_work(p); }
void G1FullKeepAliveClosure::do_oop(narrowOop* p) { do_oop_work(p); }

G1VerifyOopClosure::G1VerifyOopClosure(VerifyOption option) :
   _g1h(G1CollectedHeap::heap()),
   _containing_obj(NULL),
   _verify_option(option),
   _cc(0),
   _failures(false) {
}

void G1VerifyOopClosure::print_object(outputStream* out, oop obj) {
  Klass* k = obj->klass();
  const char* class_name = InstanceKlass::cast(k)->external_name();
  out->print_cr("class name %s", class_name);
}

template <class T> void G1VerifyOopClosure::do_oop_work(T* p) {
  T heap_oop = RawAccess<>::oop_load(p);
  if (!CompressedOops::is_null(heap_oop)) {
    _cc++;
    oop obj = CompressedOops::decode_not_null(heap_oop);
    if (!_g1h->is_in_closed_subset(obj) || _g1h->is_obj_dead_cond(obj, _verify_option)) {
      MutexLockerEx x(ParGCRareEvent_lock, Mutex::_no_safepoint_check_flag);
      _failures = true;
    }
  }
}

template void G1VerifyOopClosure::do_oop_work(oop*);
template void G1VerifyOopClosure::do_oop_work(narrowOop*);
