#include "precompiled.hpp"
#include "utilities/debug.hpp"
#include "utilities/globalDefinitions.hpp"
#include "memory/metaspace/metachunk.hpp"
#include "memory/metaspace/occupancyMap.hpp"
#include "runtime/os.hpp"

namespace metaspace {

OccupancyMap::OccupancyMap(const MetaWord* reference_address, size_t word_size, size_t smallest_chunk_word_size) :
            _reference_address(reference_address), _word_size(word_size),
            _smallest_chunk_word_size(smallest_chunk_word_size)
{
  assert(reference_address != NULL, "invalid reference address");
  assert(is_aligned(reference_address, smallest_chunk_word_size), "Reference address not aligned to smallest chunk size.");
  assert(is_aligned(word_size, smallest_chunk_word_size), "Word_size shall be a multiple of the smallest chunk size.");
  // Calculate bitmap size: one bit per smallest_chunk_word_size'd area.
  size_t num_bits = word_size / smallest_chunk_word_size;
  _map_size = (num_bits + 7) / 8;
  assert(_map_size * 8 >= num_bits, "sanity");
  _map[0] = (uint8_t*) os::malloc(_map_size, mtInternal);
  _map[1] = (uint8_t*) os::malloc(_map_size, mtInternal);
  assert(_map[0] != NULL && _map[1] != NULL, "Occupancy Map: allocation failed.");
  memset(_map[1], 0, _map_size);
  memset(_map[0], 0, _map_size);
  // Sanity test: the first respectively last possible chunk start address in
  // the covered range shall map to the first and last bit in the bitmap.
  assert(get_bitpos_for_address(reference_address) == 0, "First chunk address in range must map to fist bit in bitmap.");
  assert(get_bitpos_for_address(reference_address + word_size - smallest_chunk_word_size) == num_bits - 1, "Last chunk address in range must map to last bit in bitmap.");
}

OccupancyMap::~OccupancyMap() {
  os::free(_map[0]);
  os::free(_map[1]);
}
}
