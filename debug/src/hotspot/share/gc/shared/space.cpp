#include "precompiled.hpp"

#include "classfile/systemDictionary.hpp"
#include "classfile/vmSymbols.hpp"
#include "gc/shared/blockOffsetTable.inline.hpp"
#include "gc/shared/collectedHeap.inline.hpp"
#include "gc/shared/genCollectedHeap.hpp"
#include "gc/shared/genOopClosures.inline.hpp"
#include "gc/shared/space.hpp"
#include "gc/shared/space.inline.hpp"
#include "gc/shared/spaceDecorator.hpp"
#include "memory/iterator.inline.hpp"
#include "memory/universe.hpp"
#include "oops/oop.inline.hpp"
#include "runtime/atomic.hpp"
#include "runtime/java.hpp"
#include "runtime/orderAccess.hpp"
#include "runtime/prefetch.inline.hpp"
#include "runtime/safepoint.hpp"
#include "utilities/align.hpp"
#include "utilities/copy.hpp"
#include "utilities/globalDefinitions.hpp"
#include "utilities/macros.hpp"

HeapWord* DirtyCardToOopClosure::get_actual_top(HeapWord* top, HeapWord* top_obj) {
  if (top_obj != NULL) {
    if (_sp->block_is_obj(top_obj)) {
      if (_precision == CardTable::ObjHeadPreciseArray) {
        if (oop(top_obj)->is_objArray() || oop(top_obj)->is_typeArray()) {
          // An arrayOop is starting on the dirty card - since we do exact
          // store checks for objArrays we are done.
        } else {
          // Otherwise, it is possible that the object starting on the dirty
          // card spans the entire card, and that the store happened on a
          // later card.  Figure out where the object ends.
          // Use the block_size() method of the space over which
          // the iteration is being done.  That space (e.g. CMS) may have
          // specific requirements on object sizes which will
          // be reflected in the block_size() method.
          top = top_obj + oop(top_obj)->size();
        }
      }
    } else {
      top = top_obj;
    }
  }
  return top;
}

void DirtyCardToOopClosure::walk_mem_region(MemRegion mr, HeapWord* bottom, HeapWord* top) {
  // 1. Blocks may or may not be objects.
  // 2. Even when a block_is_obj(), it may not entirely
  //    occupy the block if the block quantum is larger than
  //    the object size.
  // We can and should try to optimize by calling the non-MemRegion
  // version of oop_iterate() for all but the extremal objects
  // (for which we need to call the MemRegion version of
  // oop_iterate()) To be done post-beta XXX
  for (; bottom < top; bottom += _sp->block_size(bottom)) {
    // As in the case of contiguous space above, we'd like to
    // just use the value returned by oop_iterate to increment the
    // current pointer; unfortunately, that won't work in CMS because
    // we'd need an interface change (it seems) to have the space
    // "adjust the object size" (for instance pad it up to its
    // block alignment or minimum block size restrictions. XXX
    if (_sp->block_is_obj(bottom) && !_sp->obj_allocated_since_save_marks(oop(bottom))) {
      oop(bottom)->oop_iterate(_cl, mr);
    }
  }
}

// We get called with "mr" representing the dirty region
// that we want to process. Because of imprecise marking,
// we may need to extend the incoming "mr" to the right,
// and scan more. However, because we may already have
// scanned some of that extended region, we may need to
// trim its right-end back some so we do not scan what
// we (or another worker thread) may already have scanned
// or planning to scan.
void DirtyCardToOopClosure::do_MemRegion(MemRegion mr) {
  // Some collectors need to do special things whenever their dirty
  // cards are processed. For instance, CMS must remember mutator updates
  // (i.e. dirty cards) so as to re-scan mutated objects.
  // Such work can be piggy-backed here on dirty card scanning, so as to make
  // it slightly more efficient than doing a complete non-destructive pre-scan
  // of the card table.
  MemRegionClosure* pCl = _sp->preconsumptionDirtyCardClosure();
  if (pCl != NULL) {
    pCl->do_MemRegion(mr);
  }

  HeapWord* bottom = mr.start();
  HeapWord* last = mr.last();
  HeapWord* top = mr.end();
  HeapWord* bottom_obj;
  HeapWord* top_obj;

  bottom_obj = _sp->block_start(bottom);
  top_obj    = _sp->block_start(last);

  // Given what we think is the top of the memory region and
  // the start of the object at the top, get the actual
  // value of the top.
  top = get_actual_top(top, top_obj);

  // If the previous call did some part of this region, don't redo.
  if (_precision == CardTable::ObjHeadPreciseArray && _min_done != NULL && _min_done < top) {
    top = _min_done;
  }

  // Top may have been reset, and in fact may be below bottom,
  // e.g. the dirty card region is entirely in a now free object
  // -- something that could happen with a concurrent sweeper.
  bottom = MIN2(bottom, top);
  MemRegion extended_mr = MemRegion(bottom, top);

  // Walk the region if it is not empty; otherwise there is nothing to do.
  if (!extended_mr.is_empty()) {
    walk_mem_region(extended_mr, bottom_obj, top);
  }

  _min_done = bottom;
}

DirtyCardToOopClosure* Space::new_dcto_cl(OopIterateClosure* cl, CardTable::PrecisionStyle precision, HeapWord* boundary, bool parallel) {
  return new DirtyCardToOopClosure(this, cl, precision, boundary);
}

HeapWord* ContiguousSpaceDCTOC::get_actual_top(HeapWord* top, HeapWord* top_obj) {
  if (top_obj != NULL && top_obj < (_sp->toContiguousSpace())->top()) {
    if (_precision == CardTable::ObjHeadPreciseArray) {
      if (oop(top_obj)->is_objArray() || oop(top_obj)->is_typeArray()) {
        // An arrayOop is starting on the dirty card - since we do exact
        // store checks for objArrays we are done.
      } else {
        // Otherwise, it is possible that the object starting on the dirty
        // card spans the entire card, and that the store happened on a
        // later card.  Figure out where the object ends.
        top = top_obj + oop(top_obj)->size();
      }
    }
  } else {
    top = (_sp->toContiguousSpace())->top();
  }
  return top;
}

void FilteringDCTOC::walk_mem_region(MemRegion mr, HeapWord* bottom, HeapWord* top) {
  // Note that this assumption won't hold if we have a concurrent
  // collector in this space, which may have freed up objects after
  // they were dirtied and before the stop-the-world GC that is
  // examining cards here.

  if (_boundary != NULL) {
    // We have a boundary outside of which we don't want to look
    // at objects, so create a filtering closure around the
    // oop closure before walking the region.
    FilteringClosure filter(_boundary, _cl);
    walk_mem_region_with_cl(mr, bottom, top, &filter);
  } else {
    // No boundary, simply walk the heap with the oop closure.
    walk_mem_region_with_cl(mr, bottom, top, _cl);
  }
}

// We must replicate this so that the static type of "FilteringClosure"
// (see above) is apparent at the oop_iterate calls.
#define ContiguousSpaceDCTOC__walk_mem_region_with_cl_DEFN(ClosureType) \
void ContiguousSpaceDCTOC::walk_mem_region_with_cl(MemRegion mr, HeapWord* bottom, HeapWord* top, ClosureType* cl) { \
  bottom += oop(bottom)->oop_iterate_size(cl, mr); \
  if (bottom < top) { \
    HeapWord* next_obj = bottom + oop(bottom)->size(); \
    while (next_obj < top) { \
      /* Bottom lies entirely below top, so we can call the */ \
      /* non-memRegion version of oop_iterate below. */ \
      oop(bottom)->oop_iterate(cl); \
      bottom = next_obj; \
      next_obj = bottom + oop(bottom)->size(); \
    } \
    /* Last object. */ \
    oop(bottom)->oop_iterate(cl, mr); \
  } \
}

// (There are only two of these, rather than N, because the split is due
// only to the introduction of the FilteringClosure, a local part of the
// impl of this abstraction.)
ContiguousSpaceDCTOC__walk_mem_region_with_cl_DEFN(OopIterateClosure)
ContiguousSpaceDCTOC__walk_mem_region_with_cl_DEFN(FilteringClosure)

DirtyCardToOopClosure* ContiguousSpace::new_dcto_cl(OopIterateClosure* cl, CardTable::PrecisionStyle precision, HeapWord* boundary, bool parallel) {
  return new ContiguousSpaceDCTOC(this, cl, precision, boundary);
}

void Space::initialize(MemRegion mr, bool clear_space, bool mangle_space) {
  HeapWord* bottom = mr.start();
  HeapWord* end    = mr.end();
  set_bottom(bottom);
  set_end(end);
  if (clear_space) clear(mangle_space);
}

void Space::clear(bool mangle_space) {
  if (ZapUnusedHeapArea && mangle_space) {
    mangle_unused_area();
  }
}

ContiguousSpace::ContiguousSpace(): CompactibleSpace(), _top(NULL), _concurrent_iteration_safe_limit(NULL) {
  _mangler = new GenSpaceMangler(this);
}

ContiguousSpace::~ContiguousSpace() {
  delete _mangler;
}

void ContiguousSpace::initialize(MemRegion mr, bool clear_space, bool mangle_space) {
  CompactibleSpace::initialize(mr, clear_space, mangle_space);
  set_concurrent_iteration_safe_limit(top());
}

void ContiguousSpace::clear(bool mangle_space) {
  set_top(bottom());
  set_saved_mark();
  CompactibleSpace::clear(mangle_space);
}

bool ContiguousSpace::is_free_block(const HeapWord* p) const {
  return p >= _top;
}

void OffsetTableContigSpace::clear(bool mangle_space) {
  ContiguousSpace::clear(mangle_space);
  _offsets.initialize_threshold();
}

void OffsetTableContigSpace::set_bottom(HeapWord* new_bottom) {
  Space::set_bottom(new_bottom);
  _offsets.set_bottom(new_bottom);
}

void OffsetTableContigSpace::set_end(HeapWord* new_end) {
  // Space should not advertise an increase in size
  // until after the underlying offset table has been enlarged.
  _offsets.resize(pointer_delta(new_end, bottom()));
  Space::set_end(new_end);
}

void CompactibleSpace::initialize(MemRegion mr, bool clear_space, bool mangle_space) {
  Space::initialize(mr, clear_space, mangle_space);
  set_compaction_top(bottom());
  _next_compaction_space = NULL;
}

void CompactibleSpace::clear(bool mangle_space) {
  Space::clear(mangle_space);
  _compaction_top = bottom();
}

HeapWord* CompactibleSpace::forward(oop q, size_t size, CompactPoint* cp, HeapWord* compact_top) {
  // q is alive
  // First check if we should switch compaction space
  size_t compaction_max_size = pointer_delta(end(), compact_top);
  while (size > compaction_max_size) {
    // switch to next compaction space
    cp->space->set_compaction_top(compact_top);
    cp->space = cp->space->next_compaction_space();
    if (cp->space == NULL) {
      cp->gen = GenCollectedHeap::heap()->young_gen();
      cp->space = cp->gen->first_compaction_space();
    }
    compact_top = cp->space->bottom();
    cp->space->set_compaction_top(compact_top);
    cp->threshold = cp->space->initialize_threshold();
    compaction_max_size = pointer_delta(cp->space->end(), compact_top);
  }

  // store the forwarding pointer into the mark word
  if ((HeapWord*)q != compact_top) {
    q->forward_to(oop(compact_top));
  } else {
    // if the object isn't moving we can just set the mark to the default
    // mark and handle it specially later on.
    q->init_mark_raw();
  }

  compact_top += size;

  // we need to update the offset table so that the beginnings of objects can be
  // found during scavenge.  Note that we are updating the offset table based on
  // where the object will be once the compaction phase finishes.
  if (compact_top > cp->threshold)
    cp->threshold = cp->space->cross_threshold(compact_top - size, compact_top);
  return compact_top;
}

void Space::print_short() const { print_short_on(tty); }

void Space::print_short_on(outputStream* st) const {
  st->print(" space " SIZE_FORMAT "K, %3d%% used", capacity() / K,
              (int) ((double) used() * 100 / capacity()));
}

void Space::print() const { print_on(tty); }

void Space::print_on(outputStream* st) const {
  print_short_on(st);
  st->print_cr(" [" INTPTR_FORMAT ", " INTPTR_FORMAT ")", p2i(bottom()), p2i(end()));
}

void ContiguousSpace::print_on(outputStream* st) const {
  print_short_on(st);
  st->print_cr(" [" INTPTR_FORMAT ", " INTPTR_FORMAT ", " INTPTR_FORMAT ")", p2i(bottom()), p2i(top()), p2i(end()));
}

void OffsetTableContigSpace::print_on(outputStream* st) const {
  print_short_on(st);
  st->print_cr(" [" INTPTR_FORMAT ", " INTPTR_FORMAT ", "
                INTPTR_FORMAT ", " INTPTR_FORMAT ")",
              p2i(bottom()), p2i(top()), p2i(_offsets.threshold()), p2i(end()));
}

void ContiguousSpace::verify() const {
  HeapWord* p = bottom();
  HeapWord* t = top();
  HeapWord* prev_p = NULL;
  while (p < t) {
    oop(p)->verify();
    prev_p = p;
    p += oop(p)->size();
  }
  guarantee(p == top(), "end of last object must match end of space");
  if (top() != end()) {
    guarantee(top() == block_start_const(end()-1) && top() == block_start_const(top()), "top should be start of unallocated block, if it exists");
  }
}

void Space::oop_iterate(OopIterateClosure* blk) {
  ObjectToOopClosure blk2(blk);
  object_iterate(&blk2);
}

bool Space::obj_is_alive(const HeapWord* p) const {
  return true;
}

void ContiguousSpace::oop_iterate(OopIterateClosure* blk) {
  if (is_empty()) return;
  HeapWord* obj_addr = bottom();
  HeapWord* t = top();
  // Could call objects iterate, but this is easier.
  while (obj_addr < t) {
    obj_addr += oop(obj_addr)->oop_iterate_size(blk);
  }
}

void ContiguousSpace::object_iterate(ObjectClosure* blk) {
  if (is_empty()) return;
  object_iterate_from(bottom(), blk);
}

// For a ContiguousSpace object_iterate() and safe_object_iterate()
// are the same.
void ContiguousSpace::safe_object_iterate(ObjectClosure* blk) {
  object_iterate(blk);
}

void ContiguousSpace::object_iterate_from(HeapWord* mark, ObjectClosure* blk) {
  while (mark < top()) {
    blk->do_object(oop(mark));
    mark += oop(mark)->size();
  }
}

HeapWord*
ContiguousSpace::object_iterate_careful(ObjectClosureCareful* blk) {
  HeapWord * limit = concurrent_iteration_safe_limit();
  for (HeapWord* p = bottom(); p < limit;) {
    size_t size = blk->do_object_careful(oop(p));
    if (size == 0) {
      return p;  // failed at p
    } else {
      p += size;
    }
  }
  return NULL; // all done
}

// Very general, slow implementation.
HeapWord* ContiguousSpace::block_start_const(const void* p) const {
  if (p >= top()) {
    return top();
  } else {
    HeapWord* last = bottom();
    HeapWord* cur = last;
    while (cur <= p) {
      last = cur;
      cur += oop(cur)->size();
    }
    return last;
  }
}

size_t ContiguousSpace::block_size(const HeapWord* p) const {
  HeapWord* current_top = top();
  if (p < current_top) {
    return oop(p)->size();
  } else {
    return pointer_delta(end(), (HeapWord*) p);
  }
}

// This version requires locking.
inline HeapWord* ContiguousSpace::allocate_impl(size_t size) {
  HeapWord* obj = top();
  if (pointer_delta(end(), obj) >= size) {
    HeapWord* new_top = obj + size;
    set_top(new_top);
    return obj;
  } else {
    return NULL;
  }
}

// This version is lock-free.
inline HeapWord* ContiguousSpace::par_allocate_impl(size_t size) {
  do {
    HeapWord* obj = top();
    if (pointer_delta(end(), obj) >= size) {
      HeapWord* new_top = obj + size;
      HeapWord* result = Atomic::cmpxchg(new_top, top_addr(), obj);
      // result can be one of two:
      //  the old top value: the exchange succeeded
      //  otherwise: the new value of the top is returned.
      if (result == obj) {
        return obj;
      }
    } else {
      return NULL;
    }
  } while (true);
}

HeapWord* ContiguousSpace::allocate_aligned(size_t size) {
  HeapWord* end_value = end();

  HeapWord* obj = CollectedHeap::align_allocation_or_fail(top(), end_value, SurvivorAlignmentInBytes);
  if (obj == NULL) {
    return NULL;
  }

  if (pointer_delta(end_value, obj) >= size) {
    HeapWord* new_top = obj + size;
    set_top(new_top);
    return obj;
  } else {
    set_top(obj);
    return NULL;
  }
}

// Requires locking.
HeapWord* ContiguousSpace::allocate(size_t size) {
  return allocate_impl(size);
}

// Lock-free.
HeapWord* ContiguousSpace::par_allocate(size_t size) {
  return par_allocate_impl(size);
}

void ContiguousSpace::allocate_temporary_filler(int factor) {
  // allocate temporary type array decreasing free size with factor 'factor'
  size_t size = pointer_delta(end(), top());

  // if space is full, return
  if (size == 0) return;

  if (factor > 0) {
    size -= size/factor;
  }
  size = align_object_size(size);

  const size_t array_header_size = typeArrayOopDesc::header_size(T_INT);
  if (size >= align_object_size(array_header_size)) {
    size_t length = (size - array_header_size) * (HeapWordSize / sizeof(jint));
    // allocate uninitialized int array
    typeArrayOop t = (typeArrayOop) allocate(size);
    t->set_mark_raw(markOopDesc::prototype());
    t->set_klass(Universe::intArrayKlassObj());
    t->set_length((int)length);
  } else {
    instanceOop obj = (instanceOop) allocate(size);
    obj->set_mark_raw(markOopDesc::prototype());
    obj->set_klass_gap(0);
    obj->set_klass(SystemDictionary::Object_klass());
  }
}

HeapWord* OffsetTableContigSpace::initialize_threshold() {
  return _offsets.initialize_threshold();
}

HeapWord* OffsetTableContigSpace::cross_threshold(HeapWord* start, HeapWord* end) {
  _offsets.alloc_block(start, end);
  return _offsets.threshold();
}

OffsetTableContigSpace::OffsetTableContigSpace(BlockOffsetSharedArray* sharedOffsetArray, MemRegion mr) :
  _offsets(sharedOffsetArray, mr),
  _par_alloc_lock(Mutex::leaf, "OffsetTableContigSpace par alloc lock", true) {
  _offsets.set_contig_space(this);
  initialize(mr, SpaceDecorator::Clear, SpaceDecorator::Mangle);
}

#define OBJ_SAMPLE_INTERVAL 0
#define BLOCK_SAMPLE_INTERVAL 100

void OffsetTableContigSpace::verify() const {
  HeapWord* p = bottom();
  HeapWord* prev_p = NULL;
  int objs = 0;
  int blocks = 0;

  if (VerifyObjectStartArray) {
    _offsets.verify();
  }

  while (p < top()) {
    size_t size = oop(p)->size();
    // For a sampling of objects in the space, find it using the
    // block offset table.
    if (blocks == BLOCK_SAMPLE_INTERVAL) {
      guarantee(p == block_start_const(p + (size/2)), "check offset computation");
      blocks = 0;
    } else {
      blocks++;
    }

    if (objs == OBJ_SAMPLE_INTERVAL) {
      oop(p)->verify();
      objs = 0;
    } else {
      objs++;
    }
    prev_p = p;
    p += size;
  }
  guarantee(p == top(), "end of last object must match end of space");
}

size_t TenuredSpace::allowed_dead_ratio() const {
  return MarkSweepDeadRatio;
}
