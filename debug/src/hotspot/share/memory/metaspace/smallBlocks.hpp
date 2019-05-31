#ifndef SHARE_MEMORY_METASPACE_SMALLBLOCKS_HPP
#define SHARE_MEMORY_METASPACE_SMALLBLOCKS_HPP

#include "memory/allocation.hpp"
#include "memory/binaryTreeDictionary.hpp"
#include "memory/metaspace/metablock.hpp"
#include "utilities/globalDefinitions.hpp"

class outputStream;

namespace metaspace {

class SmallBlocks : public CHeapObj<mtClass> {

  const static uint _small_block_max_size = sizeof(TreeChunk<Metablock,  FreeList<Metablock> >)/HeapWordSize;
  // Note: this corresponds to the imposed miminum allocation size, see SpaceManager::get_allocation_word_size()
  const static uint _small_block_min_size = sizeof(Metablock)/HeapWordSize;

private:
  FreeList<Metablock> _small_lists[_small_block_max_size - _small_block_min_size];

  FreeList<Metablock>& list_at(size_t word_size) {
    return _small_lists[word_size - _small_block_min_size];
  }

public:
  SmallBlocks() {
    for (uint i = _small_block_min_size; i < _small_block_max_size; i++) {
      uint k = i - _small_block_min_size;
      _small_lists[k].set_size(i);
    }
  }

  // Returns the total size, in words, of all blocks, across all block sizes.
  size_t total_size() const;

  // Returns the total number of all blocks across all block sizes.
  uintx total_num_blocks() const;

  static uint small_block_max_size() { return _small_block_max_size; }
  static uint small_block_min_size() { return _small_block_min_size; }

  MetaWord* get_block(size_t word_size) {
    if (list_at(word_size).count() > 0) {
      MetaWord* new_block = (MetaWord*) list_at(word_size).get_chunk_at_head();
      return new_block;
    } else {
      return NULL;
    }
  }
  void return_block(Metablock* free_chunk, size_t word_size) {
    list_at(word_size).return_chunk_at_head(free_chunk, false);
  }

  void print_on(outputStream* st) const;
};
}

#endif
