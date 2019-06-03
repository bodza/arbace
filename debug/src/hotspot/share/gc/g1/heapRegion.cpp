#include "precompiled.hpp"

#include "code/nmethod.hpp"
#include "gc/g1/g1BlockOffsetTable.inline.hpp"
#include "gc/g1/g1CollectedHeap.inline.hpp"
#include "gc/g1/g1HeapRegionTraceType.hpp"
#include "gc/g1/g1OopClosures.inline.hpp"
#include "gc/g1/heapRegion.inline.hpp"
#include "gc/g1/heapRegionBounds.inline.hpp"
#include "gc/g1/heapRegionManager.inline.hpp"
#include "gc/g1/heapRegionRemSet.hpp"
#include "gc/g1/heapRegionTracer.hpp"
#include "gc/shared/genOopClosures.inline.hpp"
#include "gc/shared/space.inline.hpp"
#include "memory/iterator.inline.hpp"
#include "memory/resourceArea.hpp"
#include "oops/access.inline.hpp"
#include "oops/compressedOops.inline.hpp"
#include "oops/oop.inline.hpp"
#include "runtime/atomic.hpp"
#include "runtime/orderAccess.hpp"
#include "utilities/growableArray.hpp"

int    HeapRegion::LogOfHRGrainBytes = 0;
int    HeapRegion::LogOfHRGrainWords = 0;
size_t HeapRegion::GrainBytes        = 0;
size_t HeapRegion::GrainWords        = 0;
size_t HeapRegion::CardsPerRegion    = 0;

size_t HeapRegion::max_region_size() {
  return HeapRegionBounds::max_size();
}

size_t HeapRegion::min_region_size_in_words() {
  return HeapRegionBounds::min_size() >> LogHeapWordSize;
}

void HeapRegion::setup_heap_region_size(size_t initial_heap_size, size_t max_heap_size) {
  size_t region_size = G1HeapRegionSize;
  if (FLAG_IS_DEFAULT(G1HeapRegionSize)) {
    size_t average_heap_size = (initial_heap_size + max_heap_size) / 2;
    region_size = MAX2(average_heap_size / HeapRegionBounds::target_number(),
                       HeapRegionBounds::min_size());
  }

  int region_size_log = log2_long((jlong) region_size);
  // Recalculate the region size to make sure it's a power of
  // 2. This means that region_size is the largest power of 2 that's
  // <= what we've calculated so far.
  region_size = ((size_t)1 << region_size_log);

  // Now make sure that we don't go over or under our limits.
  if (region_size < HeapRegionBounds::min_size()) {
    region_size = HeapRegionBounds::min_size();
  } else if (region_size > HeapRegionBounds::max_size()) {
    region_size = HeapRegionBounds::max_size();
  }

  // And recalculate the log.
  region_size_log = log2_long((jlong) region_size);

  // Now, set up the globals.
  guarantee(LogOfHRGrainBytes == 0, "we should only set it once");
  LogOfHRGrainBytes = region_size_log;

  guarantee(LogOfHRGrainWords == 0, "we should only set it once");
  LogOfHRGrainWords = LogOfHRGrainBytes - LogHeapWordSize;

  guarantee(GrainBytes == 0, "we should only set it once");
  // The cast to int is safe, given that we've bounded region_size by
  // MIN_REGION_SIZE and MAX_REGION_SIZE.
  GrainBytes = region_size;

  guarantee(GrainWords == 0, "we should only set it once");
  GrainWords = GrainBytes >> LogHeapWordSize;
  guarantee((size_t) 1 << LogOfHRGrainWords == GrainWords, "sanity");

  guarantee(CardsPerRegion == 0, "we should only set it once");
  CardsPerRegion = GrainBytes >> G1CardTable::card_shift;

  if (G1HeapRegionSize != GrainBytes) {
    FLAG_SET_ERGO(size_t, G1HeapRegionSize, GrainBytes);
  }
}

void HeapRegion::hr_clear(bool keep_remset, bool clear_space, bool locked) {
  set_young_index_in_cset(-1);
  uninstall_surv_rate_group();
  set_free();
  reset_pre_dummy_top();

  if (!keep_remset) {
    if (locked) {
      rem_set()->clear_locked();
    } else {
      rem_set()->clear();
    }
  }

  zero_marked_bytes();

  init_top_at_mark_start();
  if (clear_space) clear(SpaceDecorator::Mangle);
}

void HeapRegion::clear_cardtable() {
  G1CardTable* ct = G1CollectedHeap::heap()->card_table();
  ct->clear(MemRegion(bottom(), end()));
}

void HeapRegion::calc_gc_efficiency() {
  // GC efficiency is the ratio of how much space would be
  // reclaimed over how long we predict it would take to reclaim it.
  G1CollectedHeap* g1h = G1CollectedHeap::heap();
  G1Policy* g1p = g1h->g1_policy();

  // Retrieve a prediction of the elapsed time for this region for
  // a mixed gc because the region will only be evacuated during a
  // mixed gc.
  double region_elapsed_time_ms = g1p->predict_region_elapsed_time_ms(this, false /* for_young_gc */);
  _gc_efficiency = (double) reclaimable_bytes() / region_elapsed_time_ms;
}

void HeapRegion::set_free() {
  report_region_type_change(G1HeapRegionTraceType::Free);
  _type.set_free();
}

void HeapRegion::set_eden() {
  report_region_type_change(G1HeapRegionTraceType::Eden);
  _type.set_eden();
}

void HeapRegion::set_eden_pre_gc() {
  report_region_type_change(G1HeapRegionTraceType::Eden);
  _type.set_eden_pre_gc();
}

void HeapRegion::set_survivor() {
  report_region_type_change(G1HeapRegionTraceType::Survivor);
  _type.set_survivor();
}

void HeapRegion::move_to_old() {
  if (_type.relabel_as_old()) {
    report_region_type_change(G1HeapRegionTraceType::Old);
  }
}

void HeapRegion::set_old() {
  report_region_type_change(G1HeapRegionTraceType::Old);
  _type.set_old();
}

void HeapRegion::set_open_archive() {
  report_region_type_change(G1HeapRegionTraceType::OpenArchive);
  _type.set_open_archive();
}

void HeapRegion::set_closed_archive() {
  report_region_type_change(G1HeapRegionTraceType::ClosedArchive);
  _type.set_closed_archive();
}

void HeapRegion::set_starts_humongous(HeapWord* obj_top, size_t fill_size) {
  report_region_type_change(G1HeapRegionTraceType::StartsHumongous);
  _type.set_starts_humongous();
  _humongous_start_region = this;

  _bot_part.set_for_starts_humongous(obj_top, fill_size);
}

void HeapRegion::set_continues_humongous(HeapRegion* first_hr) {
  report_region_type_change(G1HeapRegionTraceType::ContinuesHumongous);
  _type.set_continues_humongous();
  _humongous_start_region = first_hr;

  _bot_part.set_object_can_span(true);
}

void HeapRegion::clear_humongous() {
  _humongous_start_region = NULL;

  _bot_part.set_object_can_span(false);
}

HeapRegion::HeapRegion(uint hrm_index, G1BlockOffsetTable* bot, MemRegion mr) :
    G1ContiguousSpace(bot),
    _hrm_index(hrm_index),
    _humongous_start_region(NULL),
    _evacuation_failed(false),
    _prev_marked_bytes(0), _next_marked_bytes(0), _gc_efficiency(0.0),
    _next(NULL), _prev(NULL),
     _young_index_in_cset(-1), _surv_rate_group(NULL), _age_index(-1),
    _rem_set(NULL), _recorded_rs_length(0), _predicted_elapsed_time_ms(0)
{
  _rem_set = new HeapRegionRemSet(bot, this);

  initialize(mr);
}

void HeapRegion::initialize(MemRegion mr, bool clear_space, bool mangle_space) {
  G1ContiguousSpace::initialize(mr, clear_space, mangle_space);

  hr_clear(false /*par*/, false /*clear_space*/);
  set_top(bottom());
}

void HeapRegion::report_region_type_change(G1HeapRegionTraceType::Type to) {
  HeapRegionTracer::send_region_type_change(_hrm_index, get_trace_type(), to, (uintptr_t)bottom(), used());
}

void HeapRegion::note_self_forwarding_removal_start(bool during_initial_mark, bool during_conc_mark) {
  // We always recreate the prev marking info and we'll explicitly
  // mark all objects we find to be self-forwarded on the prev
  // bitmap. So all objects need to be below PTAMS.
  _prev_marked_bytes = 0;

  if (during_initial_mark) {
    // During initial-mark, we'll also explicitly mark all objects
    // we find to be self-forwarded on the next bitmap. So all
    // objects need to be below NTAMS.
    _next_top_at_mark_start = top();
    _next_marked_bytes = 0;
  } else if (during_conc_mark) {
    // During concurrent mark, all objects in the CSet (including
    // the ones we find to be self-forwarded) are implicitly live.
    // So all objects need to be above NTAMS.
    _next_top_at_mark_start = bottom();
    _next_marked_bytes = 0;
  }
}

void HeapRegion::note_self_forwarding_removal_end(size_t marked_bytes) {
  _prev_top_at_mark_start = top();
  _prev_marked_bytes = marked_bytes;
}

// Code roots support

void HeapRegion::add_strong_code_root(nmethod* nm) {
  HeapRegionRemSet* hrrs = rem_set();
  hrrs->add_strong_code_root(nm);
}

void HeapRegion::add_strong_code_root_locked(nmethod* nm) {
  HeapRegionRemSet* hrrs = rem_set();
  hrrs->add_strong_code_root_locked(nm);
}

void HeapRegion::remove_strong_code_root(nmethod* nm) {
  HeapRegionRemSet* hrrs = rem_set();
  hrrs->remove_strong_code_root(nm);
}

void HeapRegion::strong_code_roots_do(CodeBlobClosure* blk) const {
  HeapRegionRemSet* hrrs = rem_set();
  hrrs->strong_code_roots_do(blk);
}

class VerifyStrongCodeRootOopClosure: public OopClosure {
  const HeapRegion* _hr;
  bool _failures;
  bool _has_oops_in_region;

  template <class T> void do_oop_work(T* p) {
    T heap_oop = RawAccess<>::oop_load(p);
    if (!CompressedOops::is_null(heap_oop)) {
      oop obj = CompressedOops::decode_not_null(heap_oop);

      // Note: not all the oops embedded in the nmethod are in the
      // current region. We only look at those which are.
      if (_hr->is_in(obj)) {
        // Object is in the region. Check that its less than top
        if (_hr->top() <= (HeapWord*)obj) {
          // Object is above top
          _failures = true;
          return;
        }
        // Nmethod has at least one oop in the current region
        _has_oops_in_region = true;
      }
    }
  }

public:
  VerifyStrongCodeRootOopClosure(const HeapRegion* hr) :
    _hr(hr), _failures(false), _has_oops_in_region(false) { }

  void do_oop(narrowOop* p) { do_oop_work(p); }
  void do_oop(oop* p)       { do_oop_work(p); }

  bool failures()           { return _failures; }
  bool has_oops_in_region() { return _has_oops_in_region; }
};

class VerifyStrongCodeRootCodeBlobClosure: public CodeBlobClosure {
  const HeapRegion* _hr;
  bool _failures;
public:
  VerifyStrongCodeRootCodeBlobClosure(const HeapRegion* hr) :
    _hr(hr), _failures(false) { }

  void do_code_blob(CodeBlob* cb) {
    nmethod* nm = (cb == NULL) ? NULL : cb->as_compiled_method()->as_nmethod_or_null();
    if (nm != NULL) {
      // Verify that the nemthod is live
      if (!nm->is_alive()) {
        _failures = true;
      } else {
        VerifyStrongCodeRootOopClosure oop_cl(_hr);
        nm->oops_do(&oop_cl);
        if (!oop_cl.has_oops_in_region()) {
          _failures = true;
        } else if (oop_cl.failures()) {
          _failures = true;
        }
      }
    }
  }

  bool failures()       { return _failures; }
};

void HeapRegion::verify_strong_code_roots(VerifyOption vo, bool* failures) const {
  if (!G1VerifyHeapRegionCodeRoots) {
    // We're not verifying code roots.
    return;
  }
  if (vo == VerifyOption_G1UseFullMarking) {
    // Marking verification during a full GC is performed after class
    // unloading, code cache unloading, etc so the strong code roots
    // attached to each heap region are in an inconsistent state. They won't
    // be consistent until the strong code roots are rebuilt after the
    // actual GC. Skip verifying the strong code roots in this particular
    // time.
    return;
  }

  HeapRegionRemSet* hrrs = rem_set();
  size_t strong_code_roots_length = hrrs->strong_code_roots_list_length();

  // if this region is empty then there should be no entries
  // on its strong code root list
  if (is_empty()) {
    if (strong_code_roots_length > 0) {
      *failures = true;
    }
    return;
  }

  if (is_continues_humongous()) {
    if (strong_code_roots_length > 0) {
      *failures = true;
    }
    return;
  }

  VerifyStrongCodeRootCodeBlobClosure cb_cl(this);
  strong_code_roots_do(&cb_cl);

  if (cb_cl.failures()) {
    *failures = true;
  }
}

void HeapRegion::print() const { print_on(tty); }
void HeapRegion::print_on(outputStream* st) const {
  st->print("|%4u", this->_hrm_index);
  st->print("|" PTR_FORMAT ", " PTR_FORMAT ", " PTR_FORMAT,
            p2i(bottom()), p2i(top()), p2i(end()));
  st->print("|%3d%%", (int) ((double) used() * 100 / capacity()));
  st->print("|%2s", get_short_type_str());
  if (in_collection_set()) {
    st->print("|CS");
  } else {
    st->print("|  ");
  }
  st->print_cr("|TAMS " PTR_FORMAT ", " PTR_FORMAT "| %s ", p2i(prev_top_at_mark_start()), p2i(next_top_at_mark_start()), rem_set()->get_state_str());
}

class G1VerificationClosure : public BasicOopIterateClosure {
protected:
  G1CollectedHeap* _g1h;
  G1CardTable *_ct;
  oop _containing_obj;
  bool _failures;
  int _n_failures;
  VerifyOption _vo;
public:
  // _vo == UsePrevMarking -> use "prev" marking information,
  // _vo == UseNextMarking -> use "next" marking information,
  // _vo == UseFullMarking -> use "next" marking bitmap but no TAMS.
  G1VerificationClosure(G1CollectedHeap* g1h, VerifyOption vo) :
    _g1h(g1h), _ct(g1h->card_table()),
    _containing_obj(NULL), _failures(false), _n_failures(0), _vo(vo) {
  }

  void set_containing_obj(oop obj) {
    _containing_obj = obj;
  }

  bool failures() { return _failures; }
  int n_failures() { return _n_failures; }

  void print_object(outputStream* out, oop obj) {
    Klass* k = obj->klass();
    const char* class_name = k->external_name();
    out->print_cr("class name %s", class_name);
  }
};

class VerifyLiveClosure : public G1VerificationClosure {
public:
  VerifyLiveClosure(G1CollectedHeap* g1h, VerifyOption vo) : G1VerificationClosure(g1h, vo) { }
  virtual void do_oop(narrowOop* p) { do_oop_work(p); }
  virtual void do_oop(oop* p) { do_oop_work(p); }

  template <class T>
  void do_oop_work(T* p) {
    verify_liveness(p);
  }

  template <class T>
  void verify_liveness(T* p) {
    T heap_oop = RawAccess<>::oop_load(p);
    if (!CompressedOops::is_null(heap_oop)) {
      oop obj = CompressedOops::decode_not_null(heap_oop);
      if (!_g1h->is_in_closed_subset(obj) || _g1h->is_obj_dead_cond(obj, _vo)) {
        MutexLockerEx x(ParGCRareEvent_lock, Mutex::_no_safepoint_check_flag);

        _failures = true;
        _n_failures++;
      }
    }
  }
};

class VerifyRemSetClosure : public G1VerificationClosure {
public:
  VerifyRemSetClosure(G1CollectedHeap* g1h, VerifyOption vo) : G1VerificationClosure(g1h, vo) { }
  virtual void do_oop(narrowOop* p) { do_oop_work(p); }
  virtual void do_oop(oop* p) { do_oop_work(p); }

  template <class T>
  void do_oop_work(T* p) {
    verify_remembered_set(p);
  }

  template <class T>
  void verify_remembered_set(T* p) {
    T heap_oop = RawAccess<>::oop_load(p);
    if (!CompressedOops::is_null(heap_oop)) {
      oop obj = CompressedOops::decode_not_null(heap_oop);
      HeapRegion* from = _g1h->heap_region_containing((HeapWord*)p);
      HeapRegion* to = _g1h->heap_region_containing(obj);
      if (from != NULL && to != NULL && from != to && !to->is_pinned() && to->rem_set()->is_complete()) {
        jbyte cv_obj = *_ct->byte_for_const(_containing_obj);
        jbyte cv_field = *_ct->byte_for_const(p);
        const jbyte dirty = G1CardTable::dirty_card_val();

        bool is_bad = !(from->is_young() || to->rem_set()->contains_reference(p) || (_containing_obj->is_objArray() ? cv_field == dirty : cv_obj == dirty || cv_field == dirty));
        if (is_bad) {
          MutexLockerEx x(ParGCRareEvent_lock, Mutex::_no_safepoint_check_flag);

          _failures = true;
          _n_failures++;
        }
      }
    }
  }
};

// Closure that applies the given two closures in sequence.
class G1Mux2Closure : public BasicOopIterateClosure {
  OopClosure* _c1;
  OopClosure* _c2;
public:
  G1Mux2Closure(OopClosure *c1, OopClosure *c2) { _c1 = c1; _c2 = c2; }
  template <class T> inline void do_oop_work(T* p) {
    // Apply first closure; then apply the second.
    _c1->do_oop(p);
    _c2->do_oop(p);
  }
  virtual inline void do_oop(oop* p) { do_oop_work(p); }
  virtual inline void do_oop(narrowOop* p) { do_oop_work(p); }
};

// This really ought to be commoned up into OffsetTableContigSpace somehow.
// We would need a mechanism to make that code skip dead objects.

void HeapRegion::verify(VerifyOption vo, bool* failures) const {
  G1CollectedHeap* g1h = G1CollectedHeap::heap();
  *failures = false;
  HeapWord* p = bottom();
  HeapWord* prev_p = NULL;
  VerifyLiveClosure vl_cl(g1h, vo);
  VerifyRemSetClosure vr_cl(g1h, vo);
  bool is_region_humongous = is_humongous();
  size_t object_num = 0;
  while (p < top()) {
    oop obj = oop(p);
    size_t obj_size = block_size(p);
    object_num += 1;

    if (!g1h->is_obj_dead_cond(obj, this, vo)) {
      if (oopDesc::is_oop(obj)) {
        Klass* klass = obj->klass();
        bool is_metaspace_object = Metaspace::contains(klass);
        if (!is_metaspace_object) {
          *failures = true;
          return;
        } else if (!klass->is_klass()) {
          *failures = true;
          return;
        } else {
          vl_cl.set_containing_obj(obj);
          if (!g1h->collector_state()->in_full_gc() || G1VerifyRSetsDuringFullGC) {
            // verify liveness and rem_set
            vr_cl.set_containing_obj(obj);
            G1Mux2Closure mux(&vl_cl, &vr_cl);
            obj->oop_iterate(&mux);

            if (vr_cl.failures()) {
              *failures = true;
            }
            if (G1MaxVerifyFailures >= 0 && vr_cl.n_failures() >= G1MaxVerifyFailures) {
              return;
            }
          } else {
            // verify only liveness
            obj->oop_iterate(&vl_cl);
          }
          if (vl_cl.failures()) {
            *failures = true;
          }
          if (G1MaxVerifyFailures >= 0 && vl_cl.n_failures() >= G1MaxVerifyFailures) {
            return;
          }
        }
      } else {
        *failures = true;
        return;
      }
    }
    prev_p = p;
    p += obj_size;
  }

  if (!is_young() && !is_empty()) {
    _bot_part.verify();
  }

  if (is_region_humongous) {
    oop obj = oop(this->humongous_start_region()->bottom());
    if ((HeapWord*)obj > bottom() || (HeapWord*)obj + obj->size() < bottom()) {
      *failures = true;
      return;
    }
  }

  if (!is_region_humongous && p != top()) {
    *failures = true;
    return;
  }

  HeapWord* the_end = end();
  // Do some extra BOT consistency checking for addresses in the
  // range [top, end). BOT look-ups in this range should yield
  // top. No point in doing that if top == end (there's nothing there).
  if (p < the_end) {
    // Look up top
    HeapWord* addr_1 = p;
    HeapWord* b_start_1 = _bot_part.block_start_const(addr_1);
    if (b_start_1 != p) {
      *failures = true;
      return;
    }

    // Look up top + 1
    HeapWord* addr_2 = p + 1;
    if (addr_2 < the_end) {
      HeapWord* b_start_2 = _bot_part.block_start_const(addr_2);
      if (b_start_2 != p) {
        *failures = true;
        return;
      }
    }

    // Look up an address between top and end
    size_t diff = pointer_delta(the_end, p) / 2;
    HeapWord* addr_3 = p + diff;
    if (addr_3 < the_end) {
      HeapWord* b_start_3 = _bot_part.block_start_const(addr_3);
      if (b_start_3 != p) {
        *failures = true;
        return;
      }
    }

    // Look up end - 1
    HeapWord* addr_4 = the_end - 1;
    HeapWord* b_start_4 = _bot_part.block_start_const(addr_4);
    if (b_start_4 != p) {
      *failures = true;
      return;
    }
  }

  verify_strong_code_roots(vo, failures);
}

void HeapRegion::verify() const {
  bool dummy = false;
  verify(VerifyOption_G1UsePrevMarking, /* failures */ &dummy);
}

void HeapRegion::verify_rem_set(VerifyOption vo, bool* failures) const {
  G1CollectedHeap* g1h = G1CollectedHeap::heap();
  *failures = false;
  HeapWord* p = bottom();
  HeapWord* prev_p = NULL;
  VerifyRemSetClosure vr_cl(g1h, vo);
  while (p < top()) {
    oop obj = oop(p);
    size_t obj_size = block_size(p);

    if (!g1h->is_obj_dead_cond(obj, this, vo)) {
      if (oopDesc::is_oop(obj)) {
        vr_cl.set_containing_obj(obj);
        obj->oop_iterate(&vr_cl);

        if (vr_cl.failures()) {
          *failures = true;
        }
        if (G1MaxVerifyFailures >= 0 && vr_cl.n_failures() >= G1MaxVerifyFailures) {
          return;
        }
      } else {
        *failures = true;
        return;
      }
    }

    prev_p = p;
    p += obj_size;
  }
}

void HeapRegion::verify_rem_set() const {
  bool failures = false;
  verify_rem_set(VerifyOption_G1UsePrevMarking, &failures);
  guarantee(!failures, "HeapRegion RemSet verification failed");
}

void HeapRegion::prepare_for_compaction(CompactPoint* cp) {
  // Not used for G1 anymore, but pure virtual in Space.
  ShouldNotReachHere();
}

// G1OffsetTableContigSpace code; copied from space.cpp.  Hope this can go
// away eventually.

void G1ContiguousSpace::clear(bool mangle_space) {
  set_top(bottom());
  CompactibleSpace::clear(mangle_space);
  reset_bot();
}

void G1ContiguousSpace::print() const {
  print_short();
  tty->print_cr(" [" INTPTR_FORMAT ", " INTPTR_FORMAT ", " INTPTR_FORMAT ", " INTPTR_FORMAT ")", p2i(bottom()), p2i(top()), p2i(_bot_part.threshold()), p2i(end()));
}

HeapWord* G1ContiguousSpace::initialize_threshold() {
  return _bot_part.initialize_threshold();
}

HeapWord* G1ContiguousSpace::cross_threshold(HeapWord* start, HeapWord* end) {
  _bot_part.alloc_block(start, end);
  return _bot_part.threshold();
}

void G1ContiguousSpace::safe_object_iterate(ObjectClosure* blk) {
  object_iterate(blk);
}

void G1ContiguousSpace::object_iterate(ObjectClosure* blk) {
  HeapWord* p = bottom();
  while (p < top()) {
    if (block_is_obj(p)) {
      blk->do_object(oop(p));
    }
    p += block_size(p);
  }
}

G1ContiguousSpace::G1ContiguousSpace(G1BlockOffsetTable* bot) :
  _bot_part(bot, this),
  _par_alloc_lock(Mutex::leaf, "OffsetTableContigSpace par alloc lock", true)
{
}

void G1ContiguousSpace::initialize(MemRegion mr, bool clear_space, bool mangle_space) {
  CompactibleSpace::initialize(mr, clear_space, mangle_space);
  _top = bottom();
  set_saved_mark_word(NULL);
  reset_bot();
}
