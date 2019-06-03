#ifndef SHARE_MEMORY_METASPACE_METASPACECOMMON_HPP
#define SHARE_MEMORY_METASPACE_METASPACECOMMON_HPP

#include "utilities/debug.hpp"
#include "utilities/globalDefinitions.hpp"

class outputStream;

namespace metaspace {
enum ChunkSizes {    // in words.
  ClassSpecializedChunk = 128,
  SpecializedChunk = 128,
  ClassSmallChunk = 256,
  SmallChunk = 512,
  ClassMediumChunk = 4 * K,
  MediumChunk = 8 * K
};

// Print a size, in words, scaled.
void print_scaled_words(outputStream* st, size_t word_size, size_t scale = 0, int width = -1);

// Convenience helper: prints a size value and a percentage.
void print_scaled_words_and_percentage(outputStream* st, size_t word_size, size_t compare_word_size, size_t scale = 0, int width = -1);

// Print a human readable size.
// byte_size: size, in bytes, to be printed.
// scale: one of 1 (byte-wise printing), sizeof(word) (word-size printing), K, M, G (scaled by KB, MB, GB respectively,
//         or 0, which means the best scale is choosen dynamically.
// width: printing width.
void print_human_readable_size(outputStream* st, size_t byte_size, size_t scale = 0, int width = -1);

// Prints a percentage value. Values smaller than 1% but not 0 are displayed as "<1%", values
// larger than 99% but not 100% are displayed as ">100%".
void print_percentage(outputStream* st, size_t total, size_t part);

// ChunkIndex defines the type of chunk.
// Chunk types differ by size: specialized < small < medium, chunks
// larger than medium are humongous chunks of varying size.
enum ChunkIndex {
  ZeroIndex = 0,
  SpecializedIndex = ZeroIndex,
  SmallIndex = SpecializedIndex + 1,
  MediumIndex = SmallIndex + 1,
  HumongousIndex = MediumIndex + 1,
  NumberOfFreeLists = 3,
  NumberOfInUseLists = 4
};

// Utility functions.
size_t get_size_for_nonhumongous_chunktype(ChunkIndex chunk_type, bool is_class);
ChunkIndex get_chunk_type_by_size(size_t size, bool is_class);

ChunkIndex next_chunk_index(ChunkIndex i);
ChunkIndex prev_chunk_index(ChunkIndex i);
// Returns a descriptive name for a chunk type.
const char* chunk_size_name(ChunkIndex index);

// Verify chunk type.
inline bool is_valid_chunktype(ChunkIndex index) {
  return index == SpecializedIndex || index == SmallIndex || index == MediumIndex || index == HumongousIndex;
}

inline bool is_valid_nonhumongous_chunktype(ChunkIndex index) {
  return is_valid_chunktype(index) && index != HumongousIndex;
}
}

#endif
