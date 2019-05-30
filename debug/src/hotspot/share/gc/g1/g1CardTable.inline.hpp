#ifndef SHARE_VM_GC_G1_G1CARDTABLE_INLINE_HPP
#define SHARE_VM_GC_G1_G1CARDTABLE_INLINE_HPP

#include "gc/g1/g1CardTable.hpp"

void G1CardTable::set_card_claimed(size_t card_index) {
  jbyte val = _byte_map[card_index];
  if (val == clean_card_val()) {
    val = (jbyte)claimed_card_val();
  } else {
    val |= (jbyte)claimed_card_val();
  }
  _byte_map[card_index] = val;
}

#endif
