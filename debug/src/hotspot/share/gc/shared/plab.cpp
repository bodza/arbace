#include "precompiled.hpp"

#include "gc/shared/collectedHeap.hpp"
#include "gc/shared/plab.inline.hpp"
#include "gc/shared/threadLocalAllocBuffer.hpp"
#include "logging/log.hpp"
#include "oops/arrayOop.hpp"
#include "oops/oop.inline.hpp"

size_t PLAB::min_size() {
  // Make sure that we return something that is larger than AlignmentReserve
  return align_object_size(MAX2(MinTLABSize / HeapWordSize, (size_t)oopDesc::header_size())) + AlignmentReserve;
}

size_t PLAB::max_size() {
  return ThreadLocalAllocBuffer::max_size();
}

PLAB::PLAB(size_t desired_plab_sz_) :
  _word_sz(desired_plab_sz_), _bottom(NULL), _top(NULL),
  _end(NULL), _hard_end(NULL), _allocated(0), _wasted(0), _undo_wasted(0) {
  // ArrayOopDesc::header_size depends on command line initialization.
  AlignmentReserve = oopDesc::header_size() > MinObjAlignment ? align_object_size(arrayOopDesc::header_size(T_INT)) : 0;
}

// If the minimum object size is greater than MinObjAlignment, we can
// end up with a shard at the end of the buffer that's smaller than
// the smallest object.  We can't allow that because the buffer must
// look like it's full of objects when we retire it, so we make
// sure we have enough space for a filler int array object.
size_t PLAB::AlignmentReserve;

void PLAB::flush_and_retire_stats(PLABStats* stats) {
  // Retire the last allocation buffer.
  size_t unused = retire_internal();

  // Now flush the statistics.
  stats->add_allocated(_allocated);
  stats->add_wasted(_wasted);
  stats->add_undo_wasted(_undo_wasted);
  stats->add_unused(unused);

  // Since we have flushed the stats we need to clear  the _allocated and _wasted
  // fields in case somebody retains an instance of this over GCs. Not doing so
  // will artifically inflate the values in the statistics.
  _allocated   = 0;
  _wasted      = 0;
  _undo_wasted = 0;
}

void PLAB::retire() {
  _wasted += retire_internal();
}

size_t PLAB::retire_internal() {
  size_t result = 0;
  if (_top < _hard_end) {
    Universe::heap()->fill_with_dummy_object(_top, _hard_end, true);
    result += invalidate();
  }
  return result;
}

void PLAB::add_undo_waste(HeapWord* obj, size_t word_sz) {
  Universe::heap()->fill_with_dummy_object(obj, obj + word_sz, true);
  _undo_wasted += word_sz;
}

void PLAB::undo_last_allocation(HeapWord* obj, size_t word_sz) {
  _top = obj;
}

void PLAB::undo_allocation(HeapWord* obj, size_t word_sz) {
  // Is the alloc in the current alloc buffer?
  if (contains(obj)) {
    undo_last_allocation(obj, word_sz);
  } else {
    add_undo_waste(obj, word_sz);
  }
}

void PLABStats::log_plab_allocation() { }

void PLABStats::log_sizing(size_t calculated_words, size_t net_desired_words) { }

// Calculates plab size for current number of gc worker threads.
size_t PLABStats::desired_plab_sz(uint no_of_gc_workers) {
  return align_object_size(MIN2(MAX2(min_size(), _desired_net_plab_sz / no_of_gc_workers), max_size()));
}

// Compute desired plab size for one gc worker thread and latch result for later
// use. This should be called once at the end of parallel
// scavenge; it clears the sensor accumulators.
void PLABStats::adjust_desired_plab_sz() {
  log_plab_allocation();

  if (!ResizePLAB) {
    // Clear accumulators for next round.
    reset();
    return;
  }

  size_t plab_sz = compute_desired_plab_sz();
  // Take historical weighted average
  _filter.sample(plab_sz);
  _desired_net_plab_sz = MAX2(min_size(), (size_t)_filter.average());

  log_sizing(plab_sz, _desired_net_plab_sz);
  // Clear accumulators for next round
  reset();
}

size_t PLABStats::compute_desired_plab_sz() {
  size_t allocated      = MAX2(_allocated, size_t(1));
  double wasted_frac    = (double)_unused / (double)allocated;
  size_t target_refills = (size_t)((wasted_frac * TargetSurvivorRatio) / TargetPLABWastePct);
  if (target_refills == 0) {
    target_refills = 1;
  }
  size_t used = allocated - _wasted - _unused;
  // Assumed to have 1 gc worker thread
  size_t recent_plab_sz = used / target_refills;
  return recent_plab_sz;
}
