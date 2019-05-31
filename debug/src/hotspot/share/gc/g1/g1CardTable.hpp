#ifndef SHARE_VM_GC_G1_G1CARDTABLE_HPP
#define SHARE_VM_GC_G1_G1CARDTABLE_HPP

#include "gc/g1/g1RegionToSpaceMapper.hpp"
#include "gc/shared/cardTable.hpp"
#include "oops/oopsHierarchy.hpp"
#include "utilities/macros.hpp"

class G1CardTable;
class G1RegionToSpaceMapper;

class G1CardTableChangedListener : public G1MappingChangedListener {
 private:
  G1CardTable* _card_table;
 public:
  G1CardTableChangedListener() : _card_table(NULL) { }

  void set_card_table(G1CardTable* card_table) { _card_table = card_table; }

  virtual void on_commit(uint start_idx, size_t num_regions, bool zero_filled);
};

class G1CardTable: public CardTable {
  friend class VMStructs;
  friend class G1CardTableChangedListener;

  G1CardTableChangedListener _listener;

  enum G1CardValues {
    g1_young_gen = CT_MR_BS_last_reserved << 1
  };

public:
  G1CardTable(MemRegion whole_heap): CardTable(whole_heap, /* scanned concurrently */ true), _listener() {
    _listener.set_card_table(this);
  }
  bool is_card_dirty(size_t card_index) {
    return _byte_map[card_index] == dirty_card_val();
  }

  static jbyte g1_young_card_val() { return g1_young_gen; }

/*
   Claimed and deferred bits are used together in G1 during the evacuation
   pause. These bits can have the following state transitions:
   1. The claimed bit can be put over any other card state. Except that
      the "dirty -> dirty and claimed" transition is checked for in
      G1 code and is not used.
   2. Deferred bit can be set only if the previous state of the card
      was either clean or claimed. mark_card_deferred() is wait-free.
      We do not care if the operation is be successful because if
      it does not it will only result in duplicate entry in the update
      buffer because of the "cache-miss". So it's not worth spinning.
 */

  bool is_card_claimed(size_t card_index) {
    jbyte val = _byte_map[card_index];
    return (val & (clean_card_mask_val() | claimed_card_val())) == claimed_card_val();
  }

  inline void set_card_claimed(size_t card_index);

  void verify_g1_young_region(MemRegion mr) { };
  void g1_mark_as_young(const MemRegion& mr);

  bool mark_card_deferred(size_t card_index);

  bool is_card_deferred(size_t card_index) {
    jbyte val = _byte_map[card_index];
    return (val & (clean_card_mask_val() | deferred_card_val())) == deferred_card_val();
  }

  static size_t compute_size(size_t mem_region_size_in_words) {
    size_t number_of_slots = (mem_region_size_in_words / card_size_in_words);
    return ReservedSpace::allocation_align_size_up(number_of_slots);
  }

  // Returns how many bytes of the heap a single byte of the Card Table corresponds to.
  static size_t heap_map_factor() { return card_size; }

  void initialize() { }
  void initialize(G1RegionToSpaceMapper* mapper);

  virtual void resize_covered_region(MemRegion new_region) { ShouldNotReachHere(); }

  virtual bool is_in_young(oop obj) const;
};

#endif
