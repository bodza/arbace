#ifndef SHARE_MEMORY_METASPACE_BLOCKFREELIST_HPP
#define SHARE_MEMORY_METASPACE_BLOCKFREELIST_HPP

#include "memory/allocation.hpp"
#include "memory/binaryTreeDictionary.hpp"
#include "memory/freeList.hpp"
#include "memory/metaspace/smallBlocks.hpp"
#include "memory/metaspace/metablock.hpp"
#include "utilities/globalDefinitions.hpp"

namespace metaspace {

typedef BinaryTreeDictionary<Metablock, FreeList<Metablock> > BlockTreeDictionary;

// Used to manage the free list of Metablocks (a block corresponds
// to the allocation of a quantum of metadata).
class BlockFreelist : public CHeapObj<mtClass> {

  BlockTreeDictionary* const _dictionary;
  SmallBlocks* _small_blocks;

  // Only allocate and split from freelist if the size of the allocation
  // is at least 1/4th the size of the available block.
  const static int WasteMultiplier = 4;

  // Accessors
  BlockTreeDictionary* dictionary() const { return _dictionary; }
  SmallBlocks* small_blocks() {
    if (_small_blocks == NULL) {
      _small_blocks = new SmallBlocks();
    }
    return _small_blocks;
  }

 public:

  BlockFreelist();
  ~BlockFreelist();

  // Get and return a block to the free list
  MetaWord* get_block(size_t word_size);
  void return_block(MetaWord* p, size_t word_size);

  // Returns the total size, in words, of all blocks kept in this structure.
  size_t total_size() const  {
    size_t result = dictionary()->total_size();
    if (_small_blocks != NULL) {
      result = result + _small_blocks->total_size();
    }
    return result;
  }

  // Returns the number of all blocks kept in this structure.
  uintx num_blocks() const {
    uintx result = dictionary()->total_free_blocks();
    if (_small_blocks != NULL) {
      result = result + _small_blocks->total_num_blocks();
    }
    return result;
  }

  static size_t min_dictionary_size()   { return TreeChunk<Metablock, FreeList<Metablock> >::min_size(); }
  void print_on(outputStream* st) const;
};
}

#endif
