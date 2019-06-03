#include "precompiled.hpp"

#include "memory/binaryTreeDictionary.inline.hpp"
#include "memory/metaspace/blockFreelist.hpp"
#include "utilities/ostream.hpp"
#include "utilities/globalDefinitions.hpp"

namespace metaspace {
BlockFreelist::BlockFreelist() : _dictionary(new BlockTreeDictionary()), _small_blocks(NULL) { }

BlockFreelist::~BlockFreelist() {
  delete _dictionary;
  if (_small_blocks != NULL) {
    delete _small_blocks;
  }
}

void BlockFreelist::return_block(MetaWord* p, size_t word_size) {
  Metablock* free_chunk = ::new (p) Metablock(word_size);
  if (word_size < SmallBlocks::small_block_max_size()) {
    small_blocks()->return_block(free_chunk, word_size);
  } else {
    dictionary()->return_chunk(free_chunk);
  }
}

MetaWord* BlockFreelist::get_block(size_t word_size) {
  // Try small_blocks first.
  if (word_size < SmallBlocks::small_block_max_size()) {
    // Don't create small_blocks() until needed.  small_blocks() allocates the small block list for
    // this space manager.
    MetaWord* new_block = (MetaWord*) small_blocks()->get_block(word_size);
    if (new_block != NULL) {
      return new_block;
    }
  }

  if (word_size < BlockFreelist::min_dictionary_size()) {
    // If allocation in small blocks fails, this is Dark Matter.  Too small for dictionary.
    return NULL;
  }

  Metablock* free_block = dictionary()->get_chunk(word_size);
  if (free_block == NULL) {
    return NULL;
  }

  const size_t block_size = free_block->size();
  if (block_size > WasteMultiplier * word_size) {
    return_block((MetaWord*)free_block, block_size);
    return NULL;
  }

  MetaWord* new_block = (MetaWord*)free_block;
  const size_t unused = block_size - word_size;
  if (unused >= SmallBlocks::small_block_min_size()) {
    return_block(new_block + word_size, unused);
  }

  return new_block;
}

void BlockFreelist::print_on(outputStream* st) const {
  dictionary()->print_free_lists(st);
  if (_small_blocks != NULL) {
    _small_blocks->print_on(st);
  }
}
}
