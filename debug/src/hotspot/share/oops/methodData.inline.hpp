#ifndef SHARE_VM_OOPS_METHODDATA_INLINE_HPP
#define SHARE_VM_OOPS_METHODDATA_INLINE_HPP

#include "oops/methodData.hpp"
#include "runtime/orderAccess.hpp"

inline void DataLayout::release_set_cell_at(int index, intptr_t value) {
  OrderAccess::release_store(&_cells[index], value);
}

inline void ProfileData::release_set_intptr_at(int index, intptr_t value) {
  data()->release_set_cell_at(index, value);
}

inline void ProfileData::release_set_uint_at(int index, uint value) {
  release_set_intptr_at(index, (intptr_t) value);
}

inline void ProfileData::release_set_int_at(int index, int value) {
  release_set_intptr_at(index, (intptr_t) value);
}

inline void RetData::release_set_bci(uint row, int bci) {
  // 'release' when setting the bci acts as a valid flag for other
  // threads wrt bci_count and bci_displacement.
  release_set_int_at(bci0_offset + row * ret_row_cell_count, bci);
}

#endif
