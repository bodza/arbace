#include "precompiled.hpp"

#include "gc/g1/g1Allocator.inline.hpp"
#include "gc/g1/g1CollectedHeap.hpp"
#include "gc/g1/g1CollectedHeap.inline.hpp"
#include "gc/g1/g1ConcurrentMarkThread.hpp"
#include "gc/g1/g1HeapVerifier.hpp"
#include "gc/g1/g1Policy.hpp"
#include "gc/g1/g1RemSet.hpp"
#include "gc/g1/g1RootProcessor.hpp"
#include "gc/g1/heapRegion.hpp"
#include "gc/g1/heapRegion.inline.hpp"
#include "gc/g1/heapRegionRemSet.hpp"
#include "gc/g1/g1StringDedup.hpp"
#include "logging/log.hpp"
#include "logging/logStream.hpp"
#include "memory/iterator.inline.hpp"
#include "memory/resourceArea.hpp"
#include "oops/access.inline.hpp"
#include "oops/compressedOops.inline.hpp"
#include "oops/oop.inline.hpp"
#include "runtime/handles.inline.hpp"

int G1HeapVerifier::_enabled_verification_types = G1HeapVerifier::G1VerifyAll;

class VerifyRootsClosure: public OopClosure {
private:
  G1CollectedHeap* _g1h;
  VerifyOption     _vo;
  bool             _failures;
public:
  // _vo == UsePrevMarking -> use "prev" marking information,
  // _vo == UseNextMarking -> use "next" marking information,
  // _vo == UseFullMarking -> use "next" marking bitmap but no TAMS
  VerifyRootsClosure(VerifyOption vo) :
    _g1h(G1CollectedHeap::heap()),
    _vo(vo),
    _failures(false) { }

  bool failures() { return _failures; }

  template <class T> void do_oop_work(T* p) {
    T heap_oop = RawAccess<>::oop_load(p);
    if (!CompressedOops::is_null(heap_oop)) {
      oop obj = CompressedOops::decode_not_null(heap_oop);
      if (_g1h->is_obj_dead_cond(obj, _vo)) {
        Log(gc, verify) log;
        log.error("Root location " PTR_FORMAT " points to dead obj " PTR_FORMAT, p2i(p), p2i(obj));
        ResourceMark rm;
        LogStream ls(log.error());
        obj->print_on(&ls);
        _failures = true;
      }
    }
  }

  void do_oop(oop* p)       { do_oop_work(p); }
  void do_oop(narrowOop* p) { do_oop_work(p); }
};

class G1VerifyCodeRootOopClosure: public OopClosure {
  G1CollectedHeap* _g1h;
  OopClosure* _root_cl;
  nmethod* _nm;
  VerifyOption _vo;
  bool _failures;

  template <class T> void do_oop_work(T* p) {
    // First verify that this root is live
    _root_cl->do_oop(p);

    if (!G1VerifyHeapRegionCodeRoots) {
      // We're not verifying the code roots attached to heap region.
      return;
    }

    // Don't check the code roots during marking verification in a full GC
    if (_vo == VerifyOption_G1UseFullMarking) {
      return;
    }

    // Now verify that the current nmethod (which contains p) is
    // in the code root list of the heap region containing the
    // object referenced by p.

    T heap_oop = RawAccess<>::oop_load(p);
    if (!CompressedOops::is_null(heap_oop)) {
      oop obj = CompressedOops::decode_not_null(heap_oop);

      // Now fetch the region containing the object
      HeapRegion* hr = _g1h->heap_region_containing(obj);
      HeapRegionRemSet* hrrs = hr->rem_set();
      // Verify that the strong code root list for this region
      // contains the nmethod
      if (!hrrs->strong_code_roots_list_contains(_nm)) {
        _failures = true;
      }
    }
  }

public:
  G1VerifyCodeRootOopClosure(G1CollectedHeap* g1h, OopClosure* root_cl, VerifyOption vo) :
    _g1h(g1h), _root_cl(root_cl), _vo(vo), _nm(NULL), _failures(false) { }

  void do_oop(oop* p) { do_oop_work(p); }
  void do_oop(narrowOop* p) { do_oop_work(p); }

  void set_nmethod(nmethod* nm) { _nm = nm; }
  bool failures() { return _failures; }
};

class G1VerifyCodeRootBlobClosure: public CodeBlobClosure {
  G1VerifyCodeRootOopClosure* _oop_cl;

public:
  G1VerifyCodeRootBlobClosure(G1VerifyCodeRootOopClosure* oop_cl) :
    _oop_cl(oop_cl) { }

  void do_code_blob(CodeBlob* cb) {
    nmethod* nm = cb->as_nmethod_or_null();
    if (nm != NULL) {
      _oop_cl->set_nmethod(nm);
      nm->oops_do(_oop_cl);
    }
  }
};

class YoungRefCounterClosure : public OopClosure {
  G1CollectedHeap* _g1h;
  int              _count;
 public:
  YoungRefCounterClosure(G1CollectedHeap* g1h) : _g1h(g1h), _count(0) { }
  void do_oop(oop* p)       { if (_g1h->is_in_young(*p)) { _count++; } }
  void do_oop(narrowOop* p) { ShouldNotReachHere(); }

  int count() { return _count; }
  void reset_count() { _count = 0; };
};

class VerifyCLDClosure: public CLDClosure {
  YoungRefCounterClosure _young_ref_counter_closure;
  OopClosure *_oop_closure;
 public:
  VerifyCLDClosure(G1CollectedHeap* g1h, OopClosure* cl) : _young_ref_counter_closure(g1h), _oop_closure(cl) { }
  void do_cld(ClassLoaderData* cld) {
    cld->oops_do(_oop_closure, false);

    _young_ref_counter_closure.reset_count();
    cld->oops_do(&_young_ref_counter_closure, false);
    if (_young_ref_counter_closure.count() > 0) {
      guarantee(cld->has_modified_oops(), "CLD " PTR_FORMAT ", has young %d refs but is not dirty.", p2i(cld), _young_ref_counter_closure.count());
    }
  }
};

class VerifyLivenessOopClosure: public BasicOopIterateClosure {
  G1CollectedHeap* _g1h;
  VerifyOption _vo;
public:
  VerifyLivenessOopClosure(G1CollectedHeap* g1h, VerifyOption vo) : _g1h(g1h), _vo(vo) { }
  void do_oop(narrowOop *p) { do_oop_work(p); }
  void do_oop(      oop *p) { do_oop_work(p); }

  template <class T> void do_oop_work(T *p) {
    oop obj = RawAccess<>::oop_load(p);
    guarantee(obj == NULL || !_g1h->is_obj_dead_cond(obj, _vo), "Dead object referenced by a not dead object");
  }
};

class VerifyObjsInRegionClosure: public ObjectClosure {
private:
  G1CollectedHeap* _g1h;
  size_t _live_bytes;
  HeapRegion *_hr;
  VerifyOption _vo;
public:
  // _vo == UsePrevMarking -> use "prev" marking information,
  // _vo == UseNextMarking -> use "next" marking information,
  // _vo == UseFullMarking -> use "next" marking bitmap but no TAMS.
  VerifyObjsInRegionClosure(HeapRegion *hr, VerifyOption vo)
    : _live_bytes(0), _hr(hr), _vo(vo) {
    _g1h = G1CollectedHeap::heap();
  }
  void do_object(oop o) {
    VerifyLivenessOopClosure isLive(_g1h, _vo);
    if (!_g1h->is_obj_dead_cond(o, _vo)) {
      // If the object is alive according to the full gc mark,
      // then verify that the marking information agrees.
      // Note we can't verify the contra-positive of the
      // above: if the object is dead (according to the mark
      // word), it may not be marked, or may have been marked
      // but has since became dead, or may have been allocated
      // since the last marking.
      if (_vo == VerifyOption_G1UseFullMarking) {
        guarantee(!_g1h->is_obj_dead(o), "Full GC marking and concurrent mark mismatch");
      }

      o->oop_iterate(&isLive);
      if (!_hr->obj_allocated_since_prev_marking(o)) {
        size_t obj_size = o->size();    // Make sure we don't overflow
        _live_bytes += (obj_size * HeapWordSize);
      }
    }
  }
  size_t live_bytes() { return _live_bytes; }
};

class VerifyArchiveOopClosure: public BasicOopIterateClosure {
  HeapRegion* _hr;
public:
  VerifyArchiveOopClosure(HeapRegion *hr)
    : _hr(hr) { }
  void do_oop(narrowOop *p) { do_oop_work(p); }
  void do_oop(      oop *p) { do_oop_work(p); }

  template <class T> void do_oop_work(T *p) {
    oop obj = RawAccess<>::oop_load(p);

    if (_hr->is_open_archive()) {
      guarantee(obj == NULL || G1ArchiveAllocator::is_archive_object(obj), "Archive object at " PTR_FORMAT " references a non-archive object at " PTR_FORMAT, p2i(p), p2i(obj));
    } else {
      guarantee(obj == NULL || G1ArchiveAllocator::is_closed_archive_object(obj), "Archive object at " PTR_FORMAT " references a non-archive object at " PTR_FORMAT, p2i(p), p2i(obj));
    }
  }
};

class VerifyObjectInArchiveRegionClosure: public ObjectClosure {
  HeapRegion* _hr;
public:
  VerifyObjectInArchiveRegionClosure(HeapRegion *hr, bool verbose)
    : _hr(hr) { }
  // Verify that all object pointers are to archive regions.
  void do_object(oop o) {
    VerifyArchiveOopClosure checkOop(_hr);
    o->oop_iterate(&checkOop);
  }
};

// Should be only used at CDS dump time
class VerifyArchivePointerRegionClosure: public HeapRegionClosure {
private:
  G1CollectedHeap* _g1h;
public:
  VerifyArchivePointerRegionClosure(G1CollectedHeap* g1h) { }
  virtual bool do_heap_region(HeapRegion* r) {
   if (r->is_archive()) {
      VerifyObjectInArchiveRegionClosure verify_oop_pointers(r, false);
      r->object_iterate(&verify_oop_pointers);
    }
    return false;
  }
};

void G1HeapVerifier::verify_archive_regions() {
  G1CollectedHeap*  g1h = G1CollectedHeap::heap();
  VerifyArchivePointerRegionClosure cl(NULL);
  g1h->heap_region_iterate(&cl);
}

class VerifyRegionClosure: public HeapRegionClosure {
private:
  bool             _par;
  VerifyOption     _vo;
  bool             _failures;
public:
  // _vo == UsePrevMarking -> use "prev" marking information,
  // _vo == UseNextMarking -> use "next" marking information,
  // _vo == UseFullMarking -> use "next" marking bitmap but no TAMS
  VerifyRegionClosure(bool par, VerifyOption vo)
    : _par(par),
      _vo(vo),
      _failures(false) { }

  bool failures() {
    return _failures;
  }

  bool do_heap_region(HeapRegion* r) {
    guarantee(!r->is_young() || r->rem_set()->is_complete(), "Remembered set for Young region %u must be complete, is %s", r->hrm_index(), r->rem_set()->get_state_str());
    // Humongous and old regions regions might be of any state, so can't check here.
    guarantee(!r->is_free() || !r->rem_set()->is_tracked(), "Remembered set for free region %u must be untracked, is %s", r->hrm_index(), r->rem_set()->get_state_str());
    // Verify that the continues humongous regions' remembered set state matches the
    // one from the starts humongous region.
    if (r->is_continues_humongous()) {
      if (r->rem_set()->get_state_str() != r->humongous_start_region()->rem_set()->get_state_str()) {
         _failures = true;
      }
    }
    // For archive regions, verify there are no heap pointers to
    // non-pinned regions. For all others, verify liveness info.
    if (r->is_closed_archive()) {
      VerifyObjectInArchiveRegionClosure verify_oop_pointers(r, false);
      r->object_iterate(&verify_oop_pointers);
      return true;
    } else if (r->is_open_archive()) {
      VerifyObjsInRegionClosure verify_open_archive_oop(r, _vo);
      r->object_iterate(&verify_open_archive_oop);
      return true;
    } else if (!r->is_continues_humongous()) {
      bool failures = false;
      r->verify(_vo, &failures);
      if (failures) {
        _failures = true;
      } else if (!r->is_starts_humongous()) {
        VerifyObjsInRegionClosure not_dead_yet_cl(r, _vo);
        r->object_iterate(&not_dead_yet_cl);
        if (_vo != VerifyOption_G1UseNextMarking) {
          if (r->max_live_bytes() < not_dead_yet_cl.live_bytes()) {
            _failures = true;
          }
        } else {
          // When vo == UseNextMarking we cannot currently do a sanity
          // check on the live bytes as the calculation has not been
          // finalized yet.
        }
      }
    }
    return false; // stop the region iteration if we hit a failure
  }
};

// This is the task used for parallel verification of the heap regions

class G1ParVerifyTask: public AbstractGangTask {
private:
  G1CollectedHeap*  _g1h;
  VerifyOption      _vo;
  bool              _failures;
  HeapRegionClaimer _hrclaimer;

public:
  // _vo == UsePrevMarking -> use "prev" marking information,
  // _vo == UseNextMarking -> use "next" marking information,
  // _vo == UseFullMarking -> use "next" marking bitmap but no TAMS
  G1ParVerifyTask(G1CollectedHeap* g1h, VerifyOption vo) :
      AbstractGangTask("Parallel verify task"),
      _g1h(g1h),
      _vo(vo),
      _failures(false),
      _hrclaimer(g1h->workers()->active_workers()) { }

  bool failures() {
    return _failures;
  }

  void work(uint worker_id) {
    HandleMark hm;
    VerifyRegionClosure blk(true, _vo);
    _g1h->heap_region_par_iterate_from_worker_offset(&blk, &_hrclaimer, worker_id);
    if (blk.failures()) {
      _failures = true;
    }
  }
};

void G1HeapVerifier::enable_verification_type(G1VerifyType type) {
  // First enable will clear _enabled_verification_types.
  if (_enabled_verification_types == G1VerifyAll) {
    _enabled_verification_types = type;
  } else {
    _enabled_verification_types |= type;
  }
}

bool G1HeapVerifier::should_verify(G1VerifyType type) {
  return (_enabled_verification_types & type) == type;
}

void G1HeapVerifier::verify(VerifyOption vo) {
  if (!SafepointSynchronize::is_at_safepoint()) {
  }

  VerifyRootsClosure rootsCl(vo);
  VerifyCLDClosure cldCl(_g1h, &rootsCl);

  // We apply the relevant closures to all the oops in the
  // system dictionary, class loader data graph, the string table
  // and the nmethods in the code cache.
  G1VerifyCodeRootOopClosure codeRootsCl(_g1h, &rootsCl, vo);
  G1VerifyCodeRootBlobClosure blobsCl(&codeRootsCl);

  {
    G1RootProcessor root_processor(_g1h, 1);
    root_processor.process_all_roots(&rootsCl,
                                     &cldCl,
                                     &blobsCl);
  }

  bool failures = rootsCl.failures() || codeRootsCl.failures();

  if (!_g1h->g1_policy()->collector_state()->in_full_gc()) {
    // If we're verifying during a full GC then the region sets
    // will have been torn down at the start of the GC. Therefore
    // verifying the region sets will fail. So we only verify
    // the region sets when not in a full GC.
    verify_region_sets();
  }

  if (GCParallelVerificationEnabled && ParallelGCThreads > 1) {

    G1ParVerifyTask task(_g1h, vo);
    _g1h->workers()->run_task(&task);
    if (task.failures()) {
      failures = true;
    }

  } else {
    VerifyRegionClosure blk(false, vo);
    _g1h->heap_region_iterate(&blk);
    if (blk.failures()) {
      failures = true;
    }
  }

  if (G1StringDedup::is_enabled()) {
    G1StringDedup::verify();
  }

  if (failures) {
    // It helps to have the per-region information in the output to
    // help us track down what went wrong. This is why we call
    // print_extended_on() instead of print_on().
    Log(gc, verify) log;
    ResourceMark rm;
    LogStream ls(log.error());
    _g1h->print_extended_on(&ls);
  }
  guarantee(!failures, "there should not have been any failures");
}

// Heap region set verification

class VerifyRegionListsClosure : public HeapRegionClosure {
private:
  HeapRegionSet*   _old_set;
  HeapRegionSet*   _humongous_set;
  HeapRegionManager*   _hrm;

public:
  uint _old_count;
  uint _humongous_count;
  uint _free_count;

  VerifyRegionListsClosure(HeapRegionSet* old_set,
                           HeapRegionSet* humongous_set,
                           HeapRegionManager* hrm) :
    _old_set(old_set), _humongous_set(humongous_set), _hrm(hrm),
    _old_count(), _humongous_count(), _free_count() { }

  bool do_heap_region(HeapRegion* hr) {
    if (hr->is_young()) {
      // TODO
    } else if (hr->is_humongous()) {
      _humongous_count++;
    } else if (hr->is_empty()) {
      _free_count++;
    } else if (hr->is_old()) {
      _old_count++;
    } else {
      // There are no other valid region types. Check for one invalid
      // one we can identify: pinned without old or humongous set.
      ShouldNotReachHere();
    }
    return false;
  }

  void verify_counts(HeapRegionSet* old_set, HeapRegionSet* humongous_set, HeapRegionManager* free_list) {
    guarantee(old_set->length() == _old_count, "Old set count mismatch. Expected %u, actual %u.", old_set->length(), _old_count);
    guarantee(humongous_set->length() == _humongous_count, "Hum set count mismatch. Expected %u, actual %u.", humongous_set->length(), _humongous_count);
    guarantee(free_list->num_free_regions() == _free_count, "Free list count mismatch. Expected %u, actual %u.", free_list->num_free_regions(), _free_count);
  }
};

void G1HeapVerifier::verify_region_sets() {
  // First, check the explicit lists.
  _g1h->_hrm.verify();

  // Finally, make sure that the region accounting in the lists is
  // consistent with what we see in the heap.

  VerifyRegionListsClosure cl(&_g1h->_old_set, &_g1h->_humongous_set, &_g1h->_hrm);
  _g1h->heap_region_iterate(&cl);
  cl.verify_counts(&_g1h->_old_set, &_g1h->_humongous_set, &_g1h->_hrm);
}

void G1HeapVerifier::prepare_for_verify() {
  if (SafepointSynchronize::is_at_safepoint() || ! UseTLAB) {
    _g1h->ensure_parsability(false);
  }
}

double G1HeapVerifier::verify(G1VerifyType type, VerifyOption vo, const char* msg) {
  double verify_time_ms = 0.0;

  if (should_verify(type) && _g1h->total_collections() >= VerifyGCStartAt) {
    double verify_start = os::elapsedTime();
    HandleMark hm;  // Discard invalid handles created during verification
    prepare_for_verify();
    Universe::verify(vo, msg);
    verify_time_ms = (os::elapsedTime() - verify_start) * 1000;
  }

  return verify_time_ms;
}

void G1HeapVerifier::verify_before_gc(G1VerifyType type) {
  if (VerifyBeforeGC) {
    double verify_time_ms = verify(type, VerifyOption_G1UsePrevMarking, "Before GC");
    _g1h->g1_policy()->phase_times()->record_verify_before_time_ms(verify_time_ms);
  }
}

void G1HeapVerifier::verify_after_gc(G1VerifyType type) {
  if (VerifyAfterGC) {
    double verify_time_ms = verify(type, VerifyOption_G1UsePrevMarking, "After GC");
    _g1h->g1_policy()->phase_times()->record_verify_after_time_ms(verify_time_ms);
  }
}
