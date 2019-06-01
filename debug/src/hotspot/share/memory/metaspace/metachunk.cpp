#include "precompiled.hpp"

#include "memory/allocation.hpp"
#include "memory/metaspace/metachunk.hpp"
#include "memory/metaspace/occupancyMap.hpp"
#include "memory/metaspace/virtualSpaceNode.hpp"
#include "utilities/align.hpp"
#include "utilities/copy.hpp"
#include "utilities/debug.hpp"

namespace metaspace {

size_t Metachunk::object_alignment() {
  // Must align pointers and sizes to 8,
  // so that 64 bit types get correctly aligned.
  const size_t alignment = 8;

  // Make sure that the Klass alignment also agree.
  STATIC_ASSERT(alignment == (size_t)KlassAlignmentInBytes);

  return alignment;
}

size_t Metachunk::overhead() {
  return align_up(sizeof(Metachunk), object_alignment()) / BytesPerWord;
}

// Metachunk methods

Metachunk::Metachunk(ChunkIndex chunktype, bool is_class, size_t word_size, VirtualSpaceNode* container)
    : Metabase<Metachunk>(word_size),
    _chunk_type(chunktype),
    _is_class(is_class),
    _sentinel(CHUNK_SENTINEL),
    _origin(origin_normal),
    _use_count(0),
    _top(NULL),
    _container(container)
{
  _top = initial_top();
  set_is_tagged_free(false);
}

MetaWord* Metachunk::allocate(size_t word_size) {
  MetaWord* result = NULL;
  // If available, bump the pointer to allocate.
  if (free_word_size() >= word_size) {
    result = _top;
    _top = _top + word_size;
  }
  return result;
}

// _bottom points to the start of the chunk including the overhead.
size_t Metachunk::used_word_size() const {
  return pointer_delta(_top, bottom(), sizeof(MetaWord));
}

size_t Metachunk::free_word_size() const {
  return pointer_delta(end(), _top, sizeof(MetaWord));
}

void Metachunk::print_on(outputStream* st) const {
  st->print_cr("Metachunk: bottom " PTR_FORMAT " top " PTR_FORMAT " end " PTR_FORMAT " size " SIZE_FORMAT " (%s)",
               p2i(bottom()), p2i(_top), p2i(end()), word_size(), chunk_size_name(get_chunk_type()));
  if (Verbose) {
    st->print_cr("    used " SIZE_FORMAT " free " SIZE_FORMAT, used_word_size(), free_word_size());
  }
}

// Helper, returns a descriptive name for the given index.
const char* chunk_size_name(ChunkIndex index) {
  switch (index) {
    case SpecializedIndex:
      return "specialized";
    case SmallIndex:
      return "small";
    case MediumIndex:
      return "medium";
    case HumongousIndex:
      return "humongous";
    default:
      return "Invalid index";
  }
}

void do_update_in_use_info_for_chunk(Metachunk* chunk, bool inuse) {
  chunk->set_is_tagged_free(!inuse);
  OccupancyMap* const ocmap = chunk->container()->occupancy_map();
  ocmap->set_region_in_use((MetaWord*)chunk, chunk->word_size(), inuse);
}
}
