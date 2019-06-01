#include "precompiled.hpp"

#include "gc/shared/cardTable.hpp"
#include "gc/shared/collectedHeap.hpp"
#include "gc/shared/space.inline.hpp"
#include "logging/log.hpp"
#include "memory/virtualspace.hpp"
#include "runtime/java.hpp"
#include "runtime/os.hpp"
#include "services/memTracker.hpp"
#include "utilities/align.hpp"

size_t CardTable::compute_byte_map_size() {
  const size_t granularity = os::vm_allocation_granularity();
  return align_up(_guard_index + 1, MAX2(_page_size, granularity));
}

CardTable::CardTable(MemRegion whole_heap, bool conc_scan) :
  _scanned_concurrently(conc_scan),
  _whole_heap(whole_heap),
  _guard_index(0),
  _guard_region(),
  _last_valid_index(0),
  _page_size(os::vm_page_size()),
  _byte_map_size(0),
  _covered(NULL),
  _committed(NULL),
  _cur_covered_regions(0),
  _byte_map(NULL),
  _byte_map_base(NULL)
{
  _covered   = new MemRegion[_max_covered_regions];
  if (_covered == NULL) {
    vm_exit_during_initialization("Could not allocate card table covered region set.");
  }
}

CardTable::~CardTable() {
  if (_covered) {
    delete[] _covered;
    _covered = NULL;
  }
  if (_committed) {
    delete[] _committed;
    _committed = NULL;
  }
}

void CardTable::initialize() {
  _guard_index = cards_required(_whole_heap.word_size()) - 1;
  _last_valid_index = _guard_index - 1;

  _byte_map_size = compute_byte_map_size();

  HeapWord* low_bound  = _whole_heap.start();
  HeapWord* high_bound = _whole_heap.end();

  _cur_covered_regions = 0;
  _committed = new MemRegion[_max_covered_regions];
  if (_committed == NULL) {
    vm_exit_during_initialization("Could not allocate card table committed region set.");
  }

  const size_t rs_align = _page_size == (size_t) os::vm_page_size() ? 0 :
    MAX2(_page_size, (size_t) os::vm_allocation_granularity());
  ReservedSpace heap_rs(_byte_map_size, rs_align, false);

  MemTracker::record_virtual_memory_type((address)heap_rs.base(), mtGC);

  os::trace_page_sizes("Card Table", _guard_index + 1, _guard_index + 1,
                       _page_size, heap_rs.base(), heap_rs.size());
  if (!heap_rs.is_reserved()) {
    vm_exit_during_initialization("Could not reserve enough space for the card marking array");
  }

  // The assembler store_check code will do an unsigned shift of the oop,
  // then add it to _byte_map_base, i.e.
  //
  //   _byte_map = _byte_map_base + (uintptr_t(low_bound) >> card_shift)
  _byte_map = (jbyte*) heap_rs.base();
  _byte_map_base = _byte_map - (uintptr_t(low_bound) >> card_shift);

  jbyte* guard_card = &_byte_map[_guard_index];
  HeapWord* guard_page = align_down((HeapWord*)guard_card, _page_size);
  _guard_region = MemRegion(guard_page, _page_size);
  os::commit_memory_or_exit((char*)guard_page, _page_size, _page_size,
                            !ExecMem, "card table last card");
  *guard_card = last_card;
}

int CardTable::find_covering_region_by_base(HeapWord* base) {
  int i;
  for (i = 0; i < _cur_covered_regions; i++) {
    if (_covered[i].start() == base) return i;
    if (_covered[i].start() > base) break;
  }
  // If we didn't find it, create a new one.
  // Move the ones above up, to maintain sorted order.
  for (int j = _cur_covered_regions; j > i; j--) {
    _covered[j] = _covered[j-1];
    _committed[j] = _committed[j-1];
  }
  int res = i;
  _cur_covered_regions++;
  _covered[res].set_start(base);
  _covered[res].set_word_size(0);
  jbyte* ct_start = byte_for(base);
  HeapWord* ct_start_aligned = align_down((HeapWord*)ct_start, _page_size);
  _committed[res].set_start(ct_start_aligned);
  _committed[res].set_word_size(0);
  return res;
}

int CardTable::find_covering_region_containing(HeapWord* addr) {
  for (int i = 0; i < _cur_covered_regions; i++) {
    if (_covered[i].contains(addr)) {
      return i;
    }
  }
  ShouldNotReachHere();
  return -1;
}

HeapWord* CardTable::largest_prev_committed_end(int ind) const {
  HeapWord* max_end = NULL;
  for (int j = 0; j < ind; j++) {
    HeapWord* this_end = _committed[j].end();
    if (this_end > max_end) max_end = this_end;
  }
  return max_end;
}

MemRegion CardTable::committed_unique_to_self(int self, MemRegion mr) const {
  MemRegion result = mr;
  for (int r = 0; r < _cur_covered_regions; r += 1) {
    if (r != self) {
      result = result.minus(_committed[r]);
    }
  }
  // Never include the guard page.
  result = result.minus(_guard_region);
  return result;
}

void CardTable::resize_covered_region(MemRegion new_region) {
  // We don't change the start of a region, only the end.
  // collided is true if the expansion would push into another committed region
  int const ind = find_covering_region_by_base(new_region.start());
  MemRegion const old_region = _covered[ind];
  if (new_region.word_size() != old_region.word_size()) {
    // Commit new or uncommit old pages, if necessary.
    MemRegion cur_committed = _committed[ind];
    // Extend the end of this _committed region
    // to cover the end of any lower _committed regions.
    // This forms overlapping regions, but never interior regions.
    HeapWord* const max_prev_end = largest_prev_committed_end(ind);
    if (max_prev_end > cur_committed.end()) {
      cur_committed.set_end(max_prev_end);
    }
    // Align the end up to a page size (starts are already aligned).
    HeapWord* new_end = (HeapWord*) byte_after(new_region.last());
    HeapWord* new_end_aligned = align_up(new_end, _page_size);
    // Check the other regions (excludes "ind") to ensure that
    // the new_end_aligned does not intrude onto the committed
    // space of another region.
    int ri = 0;
    for (ri = ind + 1; ri < _cur_covered_regions; ri++) {
      if (new_end_aligned > _committed[ri].start()) {
        // Any region containing the new end
        // should start at or beyond the region found (ind)
        // for the new end (committed regions are not expected to
        // be proper subsets of other committed regions).
        new_end_aligned = _committed[ri].start();
        // new_end_aligned can be equal to the start of its
        // committed region (i.e., of "ind") if a second
        // region following "ind" also start at the same location
        // as "ind".
        // Should only collide with 1 region
        break;
      }
    }
    // The guard page is always committed and should not be committed over.
    // "guarded" is used for assertion checking below and recalls the fact
    // that the would-be end of the new committed region would have
    // penetrated the guard page.
    HeapWord* new_end_for_commit = new_end_aligned;

    if (new_end_for_commit > _guard_region.start()) {
      new_end_for_commit = _guard_region.start();
    }

    if (new_end_for_commit > cur_committed.end()) {
      // Must commit new pages.
      MemRegion const new_committed = MemRegion(cur_committed.end(), new_end_for_commit);

      os::commit_memory_or_exit((char*)new_committed.start(),
                                new_committed.byte_size(), _page_size,
                                !ExecMem, "card table expansion");
    // Use new_end_aligned (as opposed to new_end_for_commit) because
    // the cur_committed region may include the guard region.
    } else if (new_end_aligned < cur_committed.end()) {
      // Must uncommit pages.
      MemRegion const uncommit_region = committed_unique_to_self(ind, MemRegion(new_end_aligned, cur_committed.end()));
      if (!uncommit_region.is_empty()) {
        // It is not safe to uncommit cards if the boundary between
        // the generations is moving.  A shrink can uncommit cards
        // owned by generation A but being used by generation B.
        if (!UseAdaptiveGCBoundary) {
          if (!os::uncommit_memory((char*)uncommit_region.start(),
                                   uncommit_region.byte_size())) {
            ShouldNotReachHere();
            // The call failed so don't change the end of the
            // committed region.  This is better than taking the
            // VM down.
            new_end_aligned = _committed[ind].end();
          }
        } else {
          new_end_aligned = _committed[ind].end();
        }
      }
    }
    // In any case, we can reset the end of the current committed entry.
    _committed[ind].set_end(new_end_aligned);

    // The default of 0 is not necessarily clean cards.
    jbyte* entry;
    if (old_region.last() < _whole_heap.start()) {
      entry = byte_for(_whole_heap.start());
    } else {
      entry = byte_after(old_region.last());
    }
    // This line commented out cleans the newly expanded region and
    // not the aligned up expanded region.
    // jbyte* const end = byte_after(new_region.last());
    jbyte* const end = (jbyte*) new_end_for_commit;
    // do nothing if we resized downward.
    if (entry < end) {
      memset(entry, clean_card, pointer_delta(end, entry, sizeof(jbyte)));
    }
  }
  // In any case, the covered size changes.
  _covered[ind].set_word_size(new_region.word_size());
}

// Note that these versions are precise!  The scanning code has to handle the
// fact that the write barrier may be either precise or imprecise.
void CardTable::dirty_MemRegion(MemRegion mr) {
  jbyte* cur  = byte_for(mr.start());
  jbyte* last = byte_after(mr.last());
  while (cur < last) {
    *cur = dirty_card;
    cur++;
  }
}

void CardTable::clear_MemRegion(MemRegion mr) {
  // Be conservative: only clean cards entirely contained within the
  // region.
  jbyte* cur;
  if (mr.start() == _whole_heap.start()) {
    cur = byte_for(mr.start());
  } else {
    cur = byte_after(mr.start() - 1);
  }
  jbyte* last = byte_after(mr.last());
  memset(cur, clean_card, pointer_delta(last, cur, sizeof(jbyte)));
}

void CardTable::clear(MemRegion mr) {
  for (int i = 0; i < _cur_covered_regions; i++) {
    MemRegion mri = mr.intersection(_covered[i]);
    if (!mri.is_empty()) clear_MemRegion(mri);
  }
}

void CardTable::dirty(MemRegion mr) {
  jbyte* first = byte_for(mr.start());
  jbyte* last  = byte_after(mr.last());
  memset(first, dirty_card, last-first);
}

// Unlike several other card table methods, dirty_card_iterate()
// iterates over dirty cards ranges in increasing address order.
void CardTable::dirty_card_iterate(MemRegion mr, MemRegionClosure* cl) {
  for (int i = 0; i < _cur_covered_regions; i++) {
    MemRegion mri = mr.intersection(_covered[i]);
    if (!mri.is_empty()) {
      jbyte *cur_entry, *next_entry, *limit;
      for (cur_entry = byte_for(mri.start()), limit = byte_for(mri.last()); cur_entry <= limit; cur_entry  = next_entry) {
        next_entry = cur_entry + 1;
        if (*cur_entry == dirty_card) {
          size_t dirty_cards;
          // Accumulate maximal dirty card range, starting at cur_entry
          for (dirty_cards = 1; next_entry <= limit && *next_entry == dirty_card; dirty_cards++, next_entry++)
            ;
          MemRegion cur_cards(addr_for(cur_entry), dirty_cards*card_size_in_words);
          cl->do_MemRegion(cur_cards);
        }
      }
    }
  }
}

MemRegion CardTable::dirty_card_range_after_reset(MemRegion mr, bool reset, int reset_val) {
  for (int i = 0; i < _cur_covered_regions; i++) {
    MemRegion mri = mr.intersection(_covered[i]);
    if (!mri.is_empty()) {
      jbyte* cur_entry, *next_entry, *limit;
      for (cur_entry = byte_for(mri.start()), limit = byte_for(mri.last()); cur_entry <= limit; cur_entry  = next_entry) {
        next_entry = cur_entry + 1;
        if (*cur_entry == dirty_card) {
          size_t dirty_cards;
          // Accumulate maximal dirty card range, starting at cur_entry
          for (dirty_cards = 1; next_entry <= limit && *next_entry == dirty_card; dirty_cards++, next_entry++)
            ;
          MemRegion cur_cards(addr_for(cur_entry), dirty_cards*card_size_in_words);
          if (reset) {
            for (size_t i = 0; i < dirty_cards; i++) {
              cur_entry[i] = reset_val;
            }
          }
          return cur_cards;
        }
      }
    }
  }
  return MemRegion(mr.end(), mr.end());
}

uintx CardTable::ct_max_alignment_constraint() {
  return card_size * os::vm_page_size();
}

void CardTable::verify_guard() {
  // For product build verification
  guarantee(_byte_map[_guard_index] == last_card, "card table guard has been modified");
}

void CardTable::invalidate(MemRegion mr) {
  for (int i = 0; i < _cur_covered_regions; i++) {
    MemRegion mri = mr.intersection(_covered[i]);
    if (!mri.is_empty()) dirty_MemRegion(mri);
  }
}

void CardTable::verify() {
  verify_guard();
}

void CardTable::print_on(outputStream* st) const {
  st->print_cr("Card table byte_map: [" INTPTR_FORMAT "," INTPTR_FORMAT "] _byte_map_base: " INTPTR_FORMAT,
               p2i(_byte_map), p2i(_byte_map + _byte_map_size), p2i(_byte_map_base));
}
