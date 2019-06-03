#include "precompiled.hpp"

#include "memory/metaspace/metachunk.hpp"
#include "memory/metaspace.hpp"
#include "memory/metaspace/chunkManager.hpp"
#include "memory/metaspace/metaspaceCommon.hpp"
#include "memory/metaspace/occupancyMap.hpp"
#include "memory/metaspace/virtualSpaceNode.hpp"
#include "memory/virtualspace.hpp"
#include "runtime/os.hpp"
#include "services/memTracker.hpp"
#include "utilities/copy.hpp"
#include "utilities/debug.hpp"
#include "utilities/globalDefinitions.hpp"

namespace metaspace {
// Decide if large pages should be committed when the memory is reserved.
static bool should_commit_large_pages_when_reserving(size_t bytes) {
  if (UseLargePages && UseLargePagesInMetaspace && !os::can_commit_large_page_memory()) {
    size_t words = bytes / BytesPerWord;
    bool is_class = false; // We never reserve large pages for the class space.
    if (MetaspaceGC::can_expand(words, is_class) && MetaspaceGC::allowed_expansion() >= words) {
      return true;
    }
  }

  return false;
}

// byte_size is the size of the associated virtualspace.
VirtualSpaceNode::VirtualSpaceNode(bool is_class, size_t bytes) :
    _is_class(is_class), _top(NULL), _next(NULL), _rs(), _container_count(0), _occupancy_map(NULL) {
  bool large_pages = should_commit_large_pages_when_reserving(bytes);
  _rs = ReservedSpace(bytes, Metaspace::reserve_alignment(), large_pages);

  if (_rs.is_reserved()) {
    MemTracker::record_virtual_memory_type((address)_rs.base(), mtClass);
  }
}

void VirtualSpaceNode::purge(ChunkManager* chunk_manager) {
    Metachunk* chunk = first_chunk();
  Metachunk* invalid_chunk = (Metachunk*) top();
  while (chunk < invalid_chunk ) {
    MetaWord* next = ((MetaWord*)chunk) + chunk->word_size();
    chunk_manager->remove_chunk(chunk);
    chunk->remove_sentinel();
    chunk = (Metachunk*) next;
  }
}

void VirtualSpaceNode::print_map(outputStream* st, bool is_class) const {
  if (bottom() == top()) {
    return;
  }

  const size_t spec_chunk_size = is_class ? ClassSpecializedChunk : SpecializedChunk;
  const size_t small_chunk_size = is_class ? ClassSmallChunk : SmallChunk;
  const size_t med_chunk_size = is_class ? ClassMediumChunk : MediumChunk;

  int line_len = 100;
  const size_t section_len = align_up(spec_chunk_size * line_len, med_chunk_size);
  line_len = (int)(section_len / spec_chunk_size);

  static const int NUM_LINES = 4;

  char* lines[NUM_LINES];
  for (int i = 0; i < NUM_LINES; i ++) {
    lines[i] = (char*)os::malloc(line_len, mtInternal);
  }
  int pos = 0;
  const MetaWord* p = bottom();
  const Metachunk* chunk = (const Metachunk*)p;
  const MetaWord* chunk_end = p + chunk->word_size();
  while (p < top()) {
    if (pos == line_len) {
      pos = 0;
      for (int i = 0; i < NUM_LINES; i ++) {
        st->fill_to(22);
        st->print_raw(lines[i], line_len);
        st->cr();
      }
    }
    if (pos == 0) {
      st->print(PTR_FORMAT ":", p2i(p));
    }
    if (p == chunk_end) {
      chunk = (Metachunk*)p;
      chunk_end = p + chunk->word_size();
    }
    // line 1: chunk starting points (a dot if that area is a chunk start).
    lines[0][pos] = p == (const MetaWord*)chunk ? '.' : ' ';

    // Line 2: chunk type (x=spec, s=small, m=medium, h=humongous), uppercase if
    // chunk is in use.
    const bool chunk_is_free = ((Metachunk*)chunk)->is_tagged_free();
    if (chunk->word_size() == spec_chunk_size) {
      lines[1][pos] = chunk_is_free ? 'x' : 'X';
    } else if (chunk->word_size() == small_chunk_size) {
      lines[1][pos] = chunk_is_free ? 's' : 'S';
    } else if (chunk->word_size() == med_chunk_size) {
      lines[1][pos] = chunk_is_free ? 'm' : 'M';
    } else if (chunk->word_size() > med_chunk_size) {
      lines[1][pos] = chunk_is_free ? 'h' : 'H';
    } else {
      ShouldNotReachHere();
    }

    // Line 3: chunk origin
    const ChunkOrigin origin = chunk->get_origin();
    lines[2][pos] = origin == origin_normal ? ' ' : '0' + (int) origin;

    // Line 4: Virgin chunk? Virgin chunks are chunks created as a byproduct of padding or splitting,
    //         but were never used.
    lines[3][pos] = chunk->get_use_count() > 0 ? ' ' : 'v';

    p += spec_chunk_size;
    pos ++;
  }
  if (pos > 0) {
    for (int i = 0; i < NUM_LINES; i ++) {
      st->fill_to(22);
      st->print_raw(lines[i], line_len);
      st->cr();
    }
  }
  for (int i = 0; i < NUM_LINES; i ++) {
    os::free(lines[i]);
  }
}

void VirtualSpaceNode::inc_container_count() {
  _container_count++;
}

void VirtualSpaceNode::dec_container_count() {
  _container_count--;
}

VirtualSpaceNode::~VirtualSpaceNode() {
  _rs.release();
  if (_occupancy_map != NULL) {
    delete _occupancy_map;
  }
}

size_t VirtualSpaceNode::used_words_in_vs() const {
  return pointer_delta(top(), bottom(), sizeof(MetaWord));
}

// Space committed in the VirtualSpace
size_t VirtualSpaceNode::capacity_words_in_vs() const {
  return pointer_delta(end(), bottom(), sizeof(MetaWord));
}

size_t VirtualSpaceNode::free_words_in_vs() const {
  return pointer_delta(end(), top(), sizeof(MetaWord));
}

// Given an address larger than top(), allocate padding chunks until top is at the given address.
void VirtualSpaceNode::allocate_padding_chunks_until_top_is_at(MetaWord* target_top) {
  // Padding chunks are added to the freelist.
  ChunkManager* const chunk_manager = Metaspace::get_chunk_manager(this->is_class());

  // shorthands
  const size_t spec_word_size = chunk_manager->specialized_chunk_word_size();
  const size_t small_word_size = chunk_manager->small_chunk_word_size();
  const size_t med_word_size = chunk_manager->medium_chunk_word_size();

  while (top() < target_top) {
    // We could make this coding more generic, but right now we only deal with two possible chunk sizes
    // for padding chunks, so it is not worth it.
    size_t padding_chunk_word_size = small_word_size;
    if (is_aligned(top(), small_word_size * sizeof(MetaWord)) == false) {
      padding_chunk_word_size = spec_word_size;
    }
    MetaWord* here = top();
    inc_top(padding_chunk_word_size);

    // Create new padding chunk.
    ChunkIndex padding_chunk_type = get_chunk_type_by_size(padding_chunk_word_size, is_class());

    Metachunk* const padding_chunk = ::new (here) Metachunk(padding_chunk_type, is_class(), padding_chunk_word_size, this);

    // Mark chunk start in occupancy map.
    occupancy_map()->set_chunk_starts_at_address((MetaWord*)padding_chunk, true);

    // Chunks are born as in-use (see MetaChunk ctor). So, before returning
    // the padding chunk to its chunk manager, mark it as in use (ChunkManager
    // will assert that).
    do_update_in_use_info_for_chunk(padding_chunk, true);

    // Return Chunk to freelist.
    inc_container_count();
    chunk_manager->return_single_chunk(padding_chunk);
    // Please note: at this point, ChunkManager::return_single_chunk()
    // may already have merged the padding chunk with neighboring chunks, so
    // it may have vanished at this point. Do not reference the padding
    // chunk beyond this point.
  }
}

// Allocates the chunk from the virtual space only.
// This interface is also used internally for debugging.  Not all
// chunks removed here are necessarily used for allocation.
Metachunk* VirtualSpaceNode::take_from_committed(size_t chunk_word_size) {
  // Non-humongous chunks are to be allocated aligned to their chunk
  // size. So, start addresses of medium chunks are aligned to medium
  // chunk size, those of small chunks to small chunk size and so
  // forth. This facilitates merging of free chunks and reduces
  // fragmentation. Chunk sizes are spec < small < medium, with each
  // larger chunk size being a multiple of the next smaller chunk
  // size.
  // Because of this alignment, me may need to create a number of padding
  // chunks. These chunks are created and added to the freelist.

  // The chunk manager to which we will give our padding chunks.
  ChunkManager* const chunk_manager = Metaspace::get_chunk_manager(this->is_class());

  // shorthands
  const size_t spec_word_size = chunk_manager->specialized_chunk_word_size();
  const size_t small_word_size = chunk_manager->small_chunk_word_size();
  const size_t med_word_size = chunk_manager->medium_chunk_word_size();

  // Chunk alignment (in bytes) == chunk size unless humongous.
  // Humongous chunks are aligned to the smallest chunk size (spec).
  const size_t required_chunk_alignment = (chunk_word_size > med_word_size ?
      spec_word_size : chunk_word_size) * sizeof(MetaWord);

  // Do we have enough space to create the requested chunk plus
  // any padding chunks needed?
  MetaWord* const next_aligned = static_cast<MetaWord*>(align_up(top(), required_chunk_alignment));
  if (!is_available((next_aligned - top()) + chunk_word_size)) {
    return NULL;
  }

  // Before allocating the requested chunk, allocate padding chunks if necessary.
  // We only need to do this for small or medium chunks: specialized chunks are the
  // smallest size, hence always aligned. Homungous chunks are allocated unaligned
  // (implicitly, also aligned to smallest chunk size).
  if ((chunk_word_size == med_word_size || chunk_word_size == small_word_size) && next_aligned > top()) {
    allocate_padding_chunks_until_top_is_at(next_aligned);
  }

  // Bottom of the new chunk
  MetaWord* chunk_limit = top();

  if (!is_available(chunk_word_size)) {
    return NULL;
  }

  // Take the space  (bump top on the current virtual space).
  inc_top(chunk_word_size);

  // Initialize the chunk
  ChunkIndex chunk_type = get_chunk_type_by_size(chunk_word_size, is_class());
  Metachunk* result = ::new (chunk_limit) Metachunk(chunk_type, is_class(), chunk_word_size, this);
  occupancy_map()->set_chunk_starts_at_address((MetaWord*)result, true);
  do_update_in_use_info_for_chunk(result, true);

  inc_container_count();

  result->inc_use_count();

  return result;
}

// Expand the virtual space (commit more of the reserved space)
bool VirtualSpaceNode::expand_by(size_t min_words, size_t preferred_words) {
  size_t min_bytes = min_words * BytesPerWord;
  size_t preferred_bytes = preferred_words * BytesPerWord;

  size_t uncommitted = virtual_space()->reserved_size() - virtual_space()->actual_committed_size();

  if (uncommitted < min_bytes) {
    return false;
  }

  size_t commit = MIN2(preferred_bytes, uncommitted);
  bool result = virtual_space()->expand_by(commit, false);

  return result;
}

Metachunk* VirtualSpaceNode::get_chunk_vs(size_t chunk_word_size) {
  Metachunk* result = take_from_committed(chunk_word_size);
  return result;
}

bool VirtualSpaceNode::initialize() {
  if (!_rs.is_reserved()) {
    return false;
  }

  // ReservedSpaces marked as special will have the entire memory
  // pre-committed. Setting a committed size will make sure that
  // committed_size and actual_committed_size agrees.
  size_t pre_committed_size = _rs.special() ? _rs.size() : 0;

  bool result = virtual_space()->initialize_with_granularity(_rs, pre_committed_size, Metaspace::commit_alignment());
  if (result) {
    set_top((MetaWord*)virtual_space()->low());
  }

  // Initialize Occupancy Map.
  const size_t smallest_chunk_size = is_class() ? ClassSpecializedChunk : SpecializedChunk;
  _occupancy_map = new OccupancyMap(bottom(), reserved_words(), smallest_chunk_size);

  return result;
}

void VirtualSpaceNode::print_on(outputStream* st, size_t scale) const {
  size_t used_words = used_words_in_vs();
  size_t commit_words = committed_words();
  size_t res_words = reserved_words();
  VirtualSpace* vs = virtual_space();

  st->print("node @" PTR_FORMAT ": ", p2i(this));
  st->print("reserved=");
  print_scaled_words(st, res_words, scale);
  st->print(", committed=");
  print_scaled_words_and_percentage(st, commit_words, res_words, scale);
  st->print(", used=");
  print_scaled_words_and_percentage(st, used_words, res_words, scale);
  st->cr();
  st->print("   [" PTR_FORMAT ", " PTR_FORMAT ", "
      PTR_FORMAT ", " PTR_FORMAT ")",
      p2i(bottom()), p2i(top()), p2i(end()),
      p2i(vs->high_boundary()));
}

void VirtualSpaceNode::retire(ChunkManager* chunk_manager) {
  for (int i = (int)MediumIndex; i >= (int)ZeroIndex; --i) {
    ChunkIndex index = (ChunkIndex)i;
    size_t chunk_size = chunk_manager->size_by_index(index);

    while (free_words_in_vs() >= chunk_size) {
      Metachunk* chunk = get_chunk_vs(chunk_size);
      // Chunk will be allocated aligned, so allocation may require
      // additional padding chunks. That may cause above allocation to
      // fail. Just ignore the failed allocation and continue with the
      // next smaller chunk size. As the VirtualSpaceNode comitted
      // size should be a multiple of the smallest chunk size, we
      // should always be able to fill the VirtualSpace completely.
      if (chunk == NULL) {
        break;
      }
      chunk_manager->return_single_chunk(chunk);
    }
  }
}
}
