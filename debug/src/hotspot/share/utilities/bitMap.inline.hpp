#ifndef SHARE_VM_UTILITIES_BITMAP_INLINE_HPP
#define SHARE_VM_UTILITIES_BITMAP_INLINE_HPP

#include "runtime/atomic.hpp"
#include "utilities/bitMap.hpp"

inline void BitMap::set_bit(idx_t bit) {
  verify_index(bit);
  *word_addr(bit) |= bit_mask(bit);
}

inline void BitMap::clear_bit(idx_t bit) {
  verify_index(bit);
  *word_addr(bit) &= ~bit_mask(bit);
}

inline bool BitMap::par_set_bit(idx_t bit) {
  verify_index(bit);
  volatile bm_word_t* const addr = word_addr(bit);
  const bm_word_t mask = bit_mask(bit);
  bm_word_t old_val = *addr;

  do {
    const bm_word_t new_val = old_val | mask;
    if (new_val == old_val) {
      return false;     // Someone else beat us to it.
    }
    const bm_word_t cur_val = Atomic::cmpxchg(new_val, addr, old_val);
    if (cur_val == old_val) {
      return true;      // Success.
    }
    old_val = cur_val;  // The value changed, try again.
  } while (true);
}

inline bool BitMap::par_clear_bit(idx_t bit) {
  verify_index(bit);
  volatile bm_word_t* const addr = word_addr(bit);
  const bm_word_t mask = ~bit_mask(bit);
  bm_word_t old_val = *addr;

  do {
    const bm_word_t new_val = old_val & mask;
    if (new_val == old_val) {
      return false;     // Someone else beat us to it.
    }
    const bm_word_t cur_val = Atomic::cmpxchg(new_val, addr, old_val);
    if (cur_val == old_val) {
      return true;      // Success.
    }
    old_val = cur_val;  // The value changed, try again.
  } while (true);
}

inline void BitMap::set_range(idx_t beg, idx_t end, RangeSizeHint hint) {
  if (hint == small_range && end - beg == 1) {
    set_bit(beg);
  } else {
    if (hint == large_range) {
      set_large_range(beg, end);
    } else {
      set_range(beg, end);
    }
  }
}

inline void BitMap::clear_range(idx_t beg, idx_t end, RangeSizeHint hint) {
  if (end - beg == 1) {
    clear_bit(beg);
  } else {
    if (hint == large_range) {
      clear_large_range(beg, end);
    } else {
      clear_range(beg, end);
    }
  }
}

inline void BitMap::par_set_range(idx_t beg, idx_t end, RangeSizeHint hint) {
  if (hint == small_range && end - beg == 1) {
    par_at_put(beg, true);
  } else {
    if (hint == large_range) {
      par_at_put_large_range(beg, end, true);
    } else {
      par_at_put_range(beg, end, true);
    }
  }
}

inline void BitMap::set_range_of_words(idx_t beg, idx_t end) {
  bm_word_t* map = _map;
  for (idx_t i = beg; i < end; ++i) map[i] = ~(bm_word_t)0;
}

inline void BitMap::clear_range_of_words(bm_word_t* map, idx_t beg, idx_t end) {
  for (idx_t i = beg; i < end; ++i) map[i] = 0;
}

inline void BitMap::clear_range_of_words(idx_t beg, idx_t end) {
  clear_range_of_words(_map, beg, end);
}

inline void BitMap::clear() {
  clear_range_of_words(0, size_in_words());
}

inline void BitMap::par_clear_range(idx_t beg, idx_t end, RangeSizeHint hint) {
  if (hint == small_range && end - beg == 1) {
    par_at_put(beg, false);
  } else {
    if (hint == large_range) {
      par_at_put_large_range(beg, end, false);
    } else {
      par_at_put_range(beg, end, false);
    }
  }
}

inline BitMap::idx_t
BitMap::get_next_one_offset(idx_t l_offset, idx_t r_offset) const {

  if (l_offset == r_offset) {
    return l_offset;
  }
  idx_t   index = word_index(l_offset);
  idx_t r_index = word_index(r_offset-1) + 1;
  idx_t res_offset = l_offset;

  // check bits including and to the _left_ of offset's position
  idx_t pos = bit_in_word(res_offset);
  bm_word_t res = map(index) >> pos;
  if (res != 0) {
    // find the position of the 1-bit
    for (; !(res & 1); res_offset++) {
      res = res >> 1;
    }

    return MIN2(res_offset, r_offset);
  }
  // skip over all word length 0-bit runs
  for (index++; index < r_index; index++) {
    res = map(index);
    if (res != 0) {
      // found a 1, return the offset
      for (res_offset = bit_index(index); !(res & 1); res_offset++) {
        res = res >> 1;
      }
      return MIN2(res_offset, r_offset);
    }
  }
  return r_offset;
}

inline BitMap::idx_t
BitMap::get_next_zero_offset(idx_t l_offset, idx_t r_offset) const {

  if (l_offset == r_offset) {
    return l_offset;
  }
  idx_t   index = word_index(l_offset);
  idx_t r_index = word_index(r_offset-1) + 1;
  idx_t res_offset = l_offset;

  // check bits including and to the _left_ of offset's position
  idx_t pos = bit_in_word(res_offset);
  bm_word_t res = ~map(index) >> pos; // flip bits and shift for l_offset

  if (res != 0) {
    // find the position of the 1-bit
    for (; !(res & 1); res_offset++) {
      res = res >> 1;
    }
    return MIN2(res_offset, r_offset);
  }
  // skip over all word length 1-bit runs
  for (index++; index < r_index; index++) {
    res = map(index);
    if (res != ~(bm_word_t)0) {
      // found a 0, return the offset
      for (res_offset = index << LogBitsPerWord; res & 1; res_offset++) {
        res = res >> 1;
      }
      return MIN2(res_offset, r_offset);
    }
  }
  return r_offset;
}

inline BitMap::idx_t
BitMap::get_next_one_offset_aligned_right(idx_t l_offset, idx_t r_offset) const {
  verify_range(l_offset, r_offset);

  if (l_offset == r_offset) {
    return l_offset;
  }
  idx_t   index = word_index(l_offset);
  idx_t r_index = word_index(r_offset);
  idx_t res_offset = l_offset;

  // check bits including and to the _left_ of offset's position
  bm_word_t res = map(index) >> bit_in_word(res_offset);
  if (res != 0) {
    // find the position of the 1-bit
    for (; !(res & 1); res_offset++) {
      res = res >> 1;
    }
    return res_offset;
  }
  // skip over all word length 0-bit runs
  for (index++; index < r_index; index++) {
    res = map(index);
    if (res != 0) {
      // found a 1, return the offset
      for (res_offset = bit_index(index); !(res & 1); res_offset++) {
        res = res >> 1;
      }
      return res_offset;
    }
  }
  return r_offset;
}

// Returns a bit mask for a range of bits [beg, end) within a single word.  Each
// bit in the mask is 0 if the bit is in the range, 1 if not in the range.  The
// returned mask can be used directly to clear the range, or inverted to set the
// range.  Note:  end must not be 0.
inline BitMap::bm_word_t
BitMap::inverted_bit_mask_for_range(idx_t beg, idx_t end) const {
  bm_word_t mask = bit_mask(beg) - 1;   // low (right) bits
  if (bit_in_word(end) != 0) {
    mask |= ~(bit_mask(end) - 1);       // high (left) bits
  }
  return mask;
}

inline void BitMap::set_large_range_of_words(idx_t beg, idx_t end) {
  memset(_map + beg, ~(unsigned char)0, (end - beg) * sizeof(bm_word_t));
}

inline void BitMap::clear_large_range_of_words(idx_t beg, idx_t end) {
  memset(_map + beg, 0, (end - beg) * sizeof(bm_word_t));
}

inline BitMap::idx_t BitMap::word_index_round_up(idx_t bit) const {
  idx_t bit_rounded_up = bit + (BitsPerWord - 1);
  // Check for integer arithmetic overflow.
  return bit_rounded_up > bit ? word_index(bit_rounded_up) : size_in_words();
}

inline bool BitMap2D::is_valid_index(idx_t slot_index, idx_t bit_within_slot_index) {
  verify_bit_within_slot_index(bit_within_slot_index);
  return (bit_index(slot_index, bit_within_slot_index) < size_in_bits());
}

inline bool BitMap2D::at(idx_t slot_index, idx_t bit_within_slot_index) const {
  verify_bit_within_slot_index(bit_within_slot_index);
  return _map.at(bit_index(slot_index, bit_within_slot_index));
}

inline void BitMap2D::set_bit(idx_t slot_index, idx_t bit_within_slot_index) {
  verify_bit_within_slot_index(bit_within_slot_index);
  _map.set_bit(bit_index(slot_index, bit_within_slot_index));
}

inline void BitMap2D::clear_bit(idx_t slot_index, idx_t bit_within_slot_index) {
  verify_bit_within_slot_index(bit_within_slot_index);
  _map.clear_bit(bit_index(slot_index, bit_within_slot_index));
}

inline void BitMap2D::at_put(idx_t slot_index, idx_t bit_within_slot_index, bool value) {
  verify_bit_within_slot_index(bit_within_slot_index);
  _map.at_put(bit_index(slot_index, bit_within_slot_index), value);
}

inline void BitMap2D::at_put_grow(idx_t slot_index, idx_t bit_within_slot_index, bool value) {
  verify_bit_within_slot_index(bit_within_slot_index);

  idx_t bit = bit_index(slot_index, bit_within_slot_index);
  if (bit >= _map.size()) {
    _map.resize(2 * MAX2(_map.size(), bit));
  }
  _map.at_put(bit, value);
}

#endif
