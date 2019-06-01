#ifndef SHARE_VM_GC_G1_G1PARSCANTHREADSTATE_INLINE_HPP
#define SHARE_VM_GC_G1_G1PARSCANTHREADSTATE_INLINE_HPP

#include "gc/g1/g1ParScanThreadState.hpp"
#include "gc/g1/g1RemSet.hpp"
#include "oops/access.inline.hpp"
#include "oops/oop.inline.hpp"

template <class T> void G1ParScanThreadState::do_oop_evac(T* p) {
  // Reference should not be NULL here as such are never pushed to the task queue.
  oop obj = RawAccess<IS_NOT_NULL>::oop_load(p);

  // Although we never intentionally push references outside of the collection
  // set, due to (benign) races in the claim mechanism during RSet scanning more
  // than one thread might claim the same card. So the same card may be
  // processed multiple times, and so we might get references into old gen here.
  // So we need to redo this check.
  const InCSetState in_cset_state = _g1h->in_cset_state(obj);
  if (in_cset_state.is_in_cset()) {
    markOop m = obj->mark_raw();
    if (m->is_marked()) {
      obj = (oop) m->decode_pointer();
    } else {
      obj = copy_to_survivor_space(in_cset_state, obj, m);
    }
    RawAccess<IS_NOT_NULL>::oop_store(p, obj);
  } else if (in_cset_state.is_humongous()) {
    _g1h->set_humongous_is_live(obj);
  }

  if (!HeapRegion::is_in_same_region(p, obj)) {
    HeapRegion* from = _g1h->heap_region_containing(p);
    update_rs(from, p, obj);
  }
}

template <class T> inline void G1ParScanThreadState::push_on_queue(T* ref) {
  _refs->push(ref);
}

inline void G1ParScanThreadState::do_oop_partial_array(oop* p) {
  oop from_obj = clear_partial_array_mask(p);

  objArrayOop from_obj_array = objArrayOop(from_obj);
  // The from-space object contains the real length.
  int length                 = from_obj_array->length();

  oop to_obj                 = from_obj->forwardee();
  objArrayOop to_obj_array   = objArrayOop(to_obj);
  // We keep track of the next start index in the length field of the
  // to-space object.
  int next_index             = to_obj_array->length();

  int start                  = next_index;
  int end                    = length;
  int remainder              = end - start;
  // We'll try not to push a range that's smaller than ParGCArrayScanChunk.
  if (remainder > 2 * ParGCArrayScanChunk) {
    end = start + ParGCArrayScanChunk;
    to_obj_array->set_length(end);
    // Push the remainder before we process the range in case another
    // worker has run out of things to do and can steal it.
    oop* from_obj_p = set_partial_array_mask(from_obj);
    push_on_queue(from_obj_p);
  } else {
    // We'll process the final range for this object. Restore the length
    // so that the heap remains parsable in case of evacuation failure.
    to_obj_array->set_length(end);
  }
  _scanner.set_region(_g1h->heap_region_containing(to_obj));
  // Process indexes [start,end). It will also process the header
  // along with the first chunk (i.e., the chunk with start == 0).
  // Note that at this point the length field of to_obj_array is not
  // correct given that we are using it to keep track of the next
  // start index. oop_iterate_range() (thankfully!) ignores the length
  // field and only relies on the start / end parameters.  It does
  // however return the size of the object which will be incorrect. So
  // we have to ignore it even if we wanted to use it.
  to_obj_array->oop_iterate_range(&_scanner, start, end);
}

inline void G1ParScanThreadState::deal_with_reference(oop* ref_to_scan) {
  if (!has_partial_array_mask(ref_to_scan)) {
    do_oop_evac(ref_to_scan);
  } else {
    do_oop_partial_array(ref_to_scan);
  }
}

inline void G1ParScanThreadState::deal_with_reference(narrowOop* ref_to_scan) {
  do_oop_evac(ref_to_scan);
}

inline void G1ParScanThreadState::dispatch_reference(StarTask ref) {
  if (ref.is_narrow()) {
    deal_with_reference((narrowOop*)ref);
  } else {
    deal_with_reference((oop*)ref);
  }
}

void G1ParScanThreadState::steal_and_trim_queue(RefToScanQueueSet *task_queues) {
  StarTask stolen_task;
  while (task_queues->steal(_worker_id, &_hash_seed, stolen_task)) {
    dispatch_reference(stolen_task);

    // We've just processed a reference and we might have made
    // available new entries on the queues. So we have to make sure
    // we drain the queues as necessary.
    trim_queue();
  }
}

inline bool G1ParScanThreadState::needs_partial_trimming() const {
  return !_refs->overflow_empty() || _refs->size() > _stack_trim_upper_threshold;
}

inline bool G1ParScanThreadState::is_partially_trimmed() const {
  return _refs->overflow_empty() && _refs->size() <= _stack_trim_lower_threshold;
}

inline void G1ParScanThreadState::trim_queue_to_threshold(uint threshold) {
  StarTask ref;
  // Drain the overflow stack first, so other threads can potentially steal.
  while (_refs->pop_overflow(ref)) {
    if (!_refs->try_push_to_taskqueue(ref)) {
      dispatch_reference(ref);
    }
  }

  while (_refs->pop_local(ref, threshold)) {
    dispatch_reference(ref);
  }
}

inline void G1ParScanThreadState::trim_queue_partially() {
  if (!needs_partial_trimming()) {
    return;
  }

  const Ticks start = Ticks::now();
  do {
    trim_queue_to_threshold(_stack_trim_lower_threshold);
  } while (!is_partially_trimmed());
  _trim_ticks += Ticks::now() - start;
}

inline Tickspan G1ParScanThreadState::trim_ticks() const {
  return _trim_ticks;
}

inline void G1ParScanThreadState::reset_trim_ticks() {
  _trim_ticks = Tickspan();
}

#endif
