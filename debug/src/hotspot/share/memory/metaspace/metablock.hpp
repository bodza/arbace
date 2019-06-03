#ifndef SHARE_MEMORY_METASPACE_METABLOCK_HPP
#define SHARE_MEMORY_METASPACE_METABLOCK_HPP

#include "memory/metaspace/metabase.hpp"
#include "utilities/globalDefinitions.hpp"

namespace metaspace {
// Metablock is the unit of allocation from a Chunk.
//
// A Metablock may be reused by its SpaceManager but are never moved between
// SpaceManagers.  There is no explicit link to the Metachunk
// from which it was allocated.  Metablock may be deallocated and
// put on a freelist but the space is never freed, rather
// the Metachunk it is a part of will be deallocated when it's
// associated class loader is collected.

class Metablock : public Metabase<Metablock> {
  friend class VMStructs;
 public:
  Metablock(size_t word_size) : Metabase<Metablock>(word_size) { }
};
}

#endif
