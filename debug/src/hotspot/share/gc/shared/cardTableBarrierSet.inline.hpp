#ifndef SHARE_VM_GC_SHARED_CARDTABLEBARRIERSET_INLINE_HPP
#define SHARE_VM_GC_SHARED_CARDTABLEBARRIERSET_INLINE_HPP

#include "gc/shared/cardTableBarrierSet.hpp"
#include "gc/shared/cardTable.hpp"
#include "runtime/orderAccess.hpp"

template <DecoratorSet decorators, typename T>
inline void CardTableBarrierSet::write_ref_field_post(T* field, oop newVal) {
  volatile jbyte* byte = _card_table->byte_for(field);
  if (_card_table->scanned_concurrently()) {
    // Perform a releasing store if the card table is scanned concurrently
    OrderAccess::release_store(byte, CardTable::dirty_card_val());
  } else {
    *byte = CardTable::dirty_card_val();
  }
}

#endif
