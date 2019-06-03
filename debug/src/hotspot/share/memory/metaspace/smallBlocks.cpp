#include "precompiled.hpp"

#include "memory/metaspace/smallBlocks.hpp"
#include "utilities/globalDefinitions.hpp"
#include "utilities/ostream.hpp"

namespace metaspace {
void SmallBlocks::print_on(outputStream* st) const {
  st->print_cr("SmallBlocks:");
  for (uint i = _small_block_min_size; i < _small_block_max_size; i++) {
    uint k = i - _small_block_min_size;
    st->print_cr("small_lists size " SIZE_FORMAT " count " SIZE_FORMAT, _small_lists[k].size(), _small_lists[k].count());
  }
}

// Returns the total size, in words, of all blocks, across all block sizes.
size_t SmallBlocks::total_size() const {
  size_t result = 0;
  for (uint i = _small_block_min_size; i < _small_block_max_size; i++) {
    uint k = i - _small_block_min_size;
    result = result + _small_lists[k].count() * _small_lists[k].size();
  }
  return result;
}

// Returns the total number of all blocks across all block sizes.
uintx SmallBlocks::total_num_blocks() const {
  uintx result = 0;
  for (uint i = _small_block_min_size; i < _small_block_max_size; i++) {
    uint k = i - _small_block_min_size;
    result = result + _small_lists[k].count();
  }
  return result;
}
}
