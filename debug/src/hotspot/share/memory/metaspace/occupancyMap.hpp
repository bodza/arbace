#ifndef SHARE_MEMORY_METASPACE_OCCUPANCYMAP_HPP
#define SHARE_MEMORY_METASPACE_OCCUPANCYMAP_HPP

#include "memory/allocation.hpp"
#include "utilities/debug.hpp"
#include "utilities/globalDefinitions.hpp"

namespace metaspace {
class Metachunk;

// Helper for Occupancy Bitmap. A type trait to give an all-bits-are-one-unsigned constant.
template <typename T> struct all_ones  { static const T value; };
template <> struct all_ones <uint64_t> { static const uint64_t value = 0xFFFFFFFFFFFFFFFFULL; };
template <> struct all_ones <uint32_t> { static const uint32_t value = 0xFFFFFFFF; };

// The OccupancyMap is a bitmap which, for a given VirtualSpaceNode,
// keeps information about
// - where a chunk starts
// - whether a chunk is in-use or free
// A bit in this bitmap represents one range of memory in the smallest
// chunk size (SpecializedChunk or ClassSpecializedChunk).
class OccupancyMap : public CHeapObj<mtInternal> {
  // The address range this map covers.
  const MetaWord* const _reference_address;
  const size_t _word_size;

  // The word size of a specialized chunk, aka the number of words one
  // bit in this map represents.
  const size_t _smallest_chunk_word_size;

  // map data
  // Data are organized in two bit layers:
  // The first layer is the chunk-start-map. Here, a bit is set to mark
  // the corresponding region as the head of a chunk.
  // The second layer is the in-use-map. Here, a set bit indicates that
  // the corresponding belongs to a chunk which is in use.
  uint8_t* _map[2];

  enum { layer_chunk_start_map = 0, layer_in_use_map = 1 };

  // length, in bytes, of bitmap data
  size_t _map_size;

  // Returns true if bit at position pos at bit-layer layer is set.
  bool get_bit_at_position(unsigned pos, unsigned layer) const {
    const unsigned byteoffset = pos / 8;
    const unsigned mask = 1 << (pos % 8);
    return (_map[layer][byteoffset] & mask) > 0;
  }

  // Changes bit at position pos at bit-layer layer to value v.
  void set_bit_at_position(unsigned pos, unsigned layer, bool v) {
    const unsigned byteoffset = pos / 8;
    const unsigned mask = 1 << (pos % 8);
    if (v) {
      _map[layer][byteoffset] |= mask;
    } else {
      _map[layer][byteoffset] &= ~mask;
    }
  }

  // Optimized case of is_any_bit_set_in_region for 32/64bit aligned access:
  // pos is 32/64 aligned and num_bits is 32/64.
  // This is the typical case when coalescing to medium chunks, whose size is
  // 32 or 64 times the specialized chunk size (depending on class or non class
  // case), so they occupy 64 bits which should be 64bit aligned, because
  // chunks are chunk-size aligned.
  template <typename T>
  bool is_any_bit_set_in_region_3264(unsigned pos, unsigned num_bits, unsigned layer) const {
    const size_t byteoffset = pos / 8;
    const T w = *(T*)(_map[layer] + byteoffset);
    return w > 0 ? true : false;
  }

  // Returns true if any bit in region [pos1, pos1 + num_bits) is set in bit-layer layer.
  bool is_any_bit_set_in_region(unsigned pos, unsigned num_bits, unsigned layer) const {
    if (pos % 32 == 0 && num_bits == 32) {
      return is_any_bit_set_in_region_3264<uint32_t>(pos, num_bits, layer);
    } else if (pos % 64 == 0 && num_bits == 64) {
      return is_any_bit_set_in_region_3264<uint64_t>(pos, num_bits, layer);
    } else {
      for (unsigned n = 0; n < num_bits; n ++) {
        if (get_bit_at_position(pos + n, layer)) {
          return true;
        }
      }
    }
    return false;
  }

  // Returns true if any bit in region [p, p+word_size) is set in bit-layer layer.
  bool is_any_bit_set_in_region(MetaWord* p, size_t word_size, unsigned layer) const {
    const unsigned pos = get_bitpos_for_address(p);
    const unsigned num_bits = (unsigned) (word_size / _smallest_chunk_word_size);
    return is_any_bit_set_in_region(pos, num_bits, layer);
  }

  // Optimized case of set_bits_of_region for 32/64bit aligned access:
  // pos is 32/64 aligned and num_bits is 32/64.
  // This is the typical case when coalescing to medium chunks, whose size
  // is 32 or 64 times the specialized chunk size (depending on class or non
  // class case), so they occupy 64 bits which should be 64bit aligned,
  // because chunks are chunk-size aligned.
  template <typename T>
  void set_bits_of_region_T(unsigned pos, unsigned num_bits, unsigned layer, bool v) {
    const size_t byteoffset = pos / 8;
    T* const pw = (T*)(_map[layer] + byteoffset);
    *pw = v ? all_ones<T>::value : (T) 0;
  }

  // Set all bits in a region starting at pos to a value.
  void set_bits_of_region(unsigned pos, unsigned num_bits, unsigned layer, bool v) {
    if (pos % 32 == 0 && num_bits == 32) {
      set_bits_of_region_T<uint32_t>(pos, num_bits, layer, v);
    } else if (pos % 64 == 0 && num_bits == 64) {
      set_bits_of_region_T<uint64_t>(pos, num_bits, layer, v);
    } else {
      for (unsigned n = 0; n < num_bits; n ++) {
        set_bit_at_position(pos + n, layer, v);
      }
    }
  }

  // Helper: sets all bits in a region [p, p+word_size).
  void set_bits_of_region(MetaWord* p, size_t word_size, unsigned layer, bool v) {
    const unsigned pos = get_bitpos_for_address(p);
    const unsigned num_bits = (unsigned) (word_size / _smallest_chunk_word_size);
    set_bits_of_region(pos, num_bits, layer, v);
  }

  // Helper: given an address, return the bit position representing that address.
  unsigned get_bitpos_for_address(const MetaWord* p) const {
    const ptrdiff_t d = (p - _reference_address) / _smallest_chunk_word_size;
    return (unsigned) d;
  }

 public:
  OccupancyMap(const MetaWord* reference_address, size_t word_size, size_t smallest_chunk_word_size);
  ~OccupancyMap();

  // Returns true if at address x a chunk is starting.
  bool chunk_starts_at_address(MetaWord* p) const {
    const unsigned pos = get_bitpos_for_address(p);
    return get_bit_at_position(pos, layer_chunk_start_map);
  }

  void set_chunk_starts_at_address(MetaWord* p, bool v) {
    const unsigned pos = get_bitpos_for_address(p);
    set_bit_at_position(pos, layer_chunk_start_map, v);
  }

  // Removes all chunk-start-bits inside a region, typically as a
  // result of a chunk merge.
  void wipe_chunk_start_bits_in_region(MetaWord* p, size_t word_size) {
    set_bits_of_region(p, word_size, layer_chunk_start_map, false);
  }

  // Returns true if there are life (in use) chunks in the region limited
  // by [p, p+word_size).
  bool is_region_in_use(MetaWord* p, size_t word_size) const {
    return is_any_bit_set_in_region(p, word_size, layer_in_use_map);
  }

  // Marks the region starting at p with the size word_size as in use
  // or free, depending on v.
  void set_region_in_use(MetaWord* p, size_t word_size, bool v) {
    set_bits_of_region(p, word_size, layer_in_use_map, v);
  }
};
}

#endif
