#include "precompiled.hpp"
#include "gc/g1/g1CardTable.hpp"
#include "gc/g1/g1CollectedHeap.inline.hpp"
#include "gc/shared/memset_with_concurrent_readers.hpp"
#include "logging/log.hpp"
#include "runtime/atomic.hpp"
#include "runtime/orderAccess.hpp"

bool G1CardTable::mark_card_deferred(size_t card_index) {
  jbyte val = _byte_map[card_index];
  // It's already processed
  if ((val & (clean_card_mask_val() | deferred_card_val())) == deferred_card_val()) {
    return false;
  }

  // Cached bit can be installed either on a clean card or on a claimed card.
  jbyte new_val = val;
  if (val == clean_card_val()) {
    new_val = (jbyte)deferred_card_val();
  } else {
    if (val & claimed_card_val()) {
      new_val = val | (jbyte)deferred_card_val();
    }
  }
  if (new_val != val) {
    Atomic::cmpxchg(new_val, &_byte_map[card_index], val);
  }
  return true;
}

void G1CardTable::g1_mark_as_young(const MemRegion& mr) {
  jbyte *const first = byte_for(mr.start());
  jbyte *const last = byte_after(mr.last());

  memset_with_concurrent_readers(first, g1_young_gen, last - first);
}

void G1CardTableChangedListener::on_commit(uint start_idx, size_t num_regions, bool zero_filled) {
  // Default value for a clean card on the card table is -1. So we cannot take advantage of the zero_filled parameter.
  MemRegion mr(G1CollectedHeap::heap()->bottom_addr_for_region(start_idx), num_regions * HeapRegion::GrainWords);
  _card_table->clear(mr);
}

void G1CardTable::initialize(G1RegionToSpaceMapper* mapper) {
  mapper->set_mapping_changed_listener(&_listener);

  _byte_map_size = mapper->reserved().byte_size();

  _guard_index = cards_required(_whole_heap.word_size()) - 1;
  _last_valid_index = _guard_index - 1;

  HeapWord* low_bound  = _whole_heap.start();
  HeapWord* high_bound = _whole_heap.end();

  _cur_covered_regions = 1;
  _covered[0] = _whole_heap;

  _byte_map = (jbyte*) mapper->reserved().start();
  _byte_map_base = _byte_map - (uintptr_t(low_bound) >> card_shift);
  assert(byte_for(low_bound) == &_byte_map[0], "Checking start of map");
  assert(byte_for(high_bound-1) <= &_byte_map[_last_valid_index], "Checking end of map");

  log_trace(gc, barrier)("G1CardTable::G1CardTable: ");
  log_trace(gc, barrier)("    &_byte_map[0]: " INTPTR_FORMAT "  &_byte_map[_last_valid_index]: " INTPTR_FORMAT,
                         p2i(&_byte_map[0]), p2i(&_byte_map[_last_valid_index]));
  log_trace(gc, barrier)("    _byte_map_base: " INTPTR_FORMAT,  p2i(_byte_map_base));
}

bool G1CardTable::is_in_young(oop obj) const {
  volatile jbyte* p = byte_for(obj);
  return *p == G1CardTable::g1_young_card_val();
}
