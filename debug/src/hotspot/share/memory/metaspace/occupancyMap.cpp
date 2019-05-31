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
  // Calculate bitmap size: one bit per smallest_chunk_word_size'd area.
  size_t num_bits = word_size / smallest_chunk_word_size;
  _map_size = (num_bits + 7) / 8;
  _map[0] = (uint8_t*) os::malloc(_map_size, mtInternal);
  _map[1] = (uint8_t*) os::malloc(_map_size, mtInternal);
  memset(_map[1], 0, _map_size);
  memset(_map[0], 0, _map_size);
}

OccupancyMap::~OccupancyMap() {
  os::free(_map[0]);
  os::free(_map[1]);
}
}
