#ifndef SHARE_VM_GC_G1_HEAPREGIONSET_INLINE_HPP
#define SHARE_VM_GC_G1_HEAPREGIONSET_INLINE_HPP

#include "gc/g1/heapRegionSet.hpp"

inline void HeapRegionSetBase::add(HeapRegion* hr) {
  check_mt_safety();

  _length++;
  hr->set_containing_set(this);
}

inline void HeapRegionSetBase::remove(HeapRegion* hr) {
  check_mt_safety();

  hr->set_containing_set(NULL);
  _length--;
}

inline void FreeRegionList::add_ordered(HeapRegion* hr) {
  // add() will verify the region and check mt safety.
  add(hr);

  // Now link the region
  if (_head != NULL) {
    HeapRegion* curr;

    if (_last != NULL && _last->hrm_index() < hr->hrm_index()) {
      curr = _last;
    } else {
      curr = _head;
    }

    // Find first entry with a Region Index larger than entry to insert.
    while (curr != NULL && curr->hrm_index() < hr->hrm_index()) {
      curr = curr->next();
    }

    hr->set_next(curr);

    if (curr == NULL) {
      // Adding at the end
      hr->set_prev(_tail);
      _tail->set_next(hr);
      _tail = hr;
    } else if (curr->prev() == NULL) {
      // Adding at the beginning
      hr->set_prev(NULL);
      _head = hr;
      curr->set_prev(hr);
    } else {
      hr->set_prev(curr->prev());
      hr->prev()->set_next(hr);
      curr->set_prev(hr);
    }
  } else {
    // The list was empty
    _tail = hr;
    _head = hr;
  }
  _last = hr;
}

inline HeapRegion* FreeRegionList::remove_from_head_impl() {
  HeapRegion* result = _head;
  _head = result->next();
  if (_head == NULL) {
    _tail = NULL;
  } else {
    _head->set_prev(NULL);
  }
  result->set_next(NULL);
  return result;
}

inline HeapRegion* FreeRegionList::remove_from_tail_impl() {
  HeapRegion* result = _tail;

  _tail = result->prev();
  if (_tail == NULL) {
    _head = NULL;
  } else {
    _tail->set_next(NULL);
  }
  result->set_prev(NULL);
  return result;
}

inline HeapRegion* FreeRegionList::remove_region(bool from_head) {
  check_mt_safety();

  if (is_empty()) {
    return NULL;
  }

  HeapRegion* hr;

  if (from_head) {
    hr = remove_from_head_impl();
  } else {
    hr = remove_from_tail_impl();
  }

  if (_last == hr) {
    _last = NULL;
  }

  // remove() will verify the region and check mt safety.
  remove(hr);
  return hr;
}

#endif
