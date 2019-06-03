#include "precompiled.hpp"

#include "memory/metaspace/metaspaceCommon.hpp"
#include "memory/metaspace/virtualSpaceNode.hpp"
#include "utilities/debug.hpp"
#include "utilities/globalDefinitions.hpp"
#include "utilities/ostream.hpp"

namespace metaspace {
// Print a size, in words, scaled.
void print_scaled_words(outputStream* st, size_t word_size, size_t scale, int width) {
  print_human_readable_size(st, word_size * sizeof(MetaWord), scale, width);
}

// Convenience helper: prints a size value and a percentage.
void print_scaled_words_and_percentage(outputStream* st, size_t word_size, size_t compare_word_size, size_t scale, int width) {
  print_scaled_words(st, word_size, scale, width);
  st->print(" (");
  print_percentage(st, compare_word_size, word_size);
  st->print(")");
}

// Print a human readable size.
// byte_size: size, in bytes, to be printed.
// scale: one of 1 (byte-wise printing), sizeof(word) (word-size printing), K, M, G (scaled by KB, MB, GB respectively,
//         or 0, which means the best scale is choosen dynamically.
// width: printing width.
void print_human_readable_size(outputStream* st, size_t byte_size, size_t scale, int width) {
  if (scale == 0) {
    // Dynamic mode. Choose scale for this value.
    if (byte_size == 0) {
      // Zero values are printed as bytes.
      scale = 1;
    } else {
      if (byte_size >= G) {
        scale = G;
      } else if (byte_size >= M) {
        scale = M;
      } else if (byte_size >= K) {
        scale = K;
      } else {
        scale = 1;
      }
    }
    return print_human_readable_size(st, byte_size, scale, width);
  }

  if (scale == 1) {
    st->print("%*" PRIuPTR " bytes", width, byte_size);
  } else if (scale == BytesPerWord) {
    st->print("%*" PRIuPTR " words", width, byte_size / BytesPerWord);
  } else {
    const char* display_unit = "";
    switch(scale) {
      case 1: display_unit = "bytes"; break;
      case BytesPerWord: display_unit = "words"; break;
      case K: display_unit = "KB"; break;
      case M: display_unit = "MB"; break;
      case G: display_unit = "GB"; break;
      default:
        ShouldNotReachHere();
    }
    float display_value = (float) byte_size / scale;
    // Since we use width to display a number with two trailing digits, increase it a bit.
    width += 3;
    // Prevent very small but non-null values showing up as 0.00.
    if (byte_size > 0 && display_value < 0.01f) {
      st->print("%*s %s", width, "<0.01", display_unit);
    } else {
      st->print("%*.2f %s", width, display_value, display_unit);
    }
  }
}

// Prints a percentage value. Values smaller than 1% but not 0 are displayed as "<1%", values
// larger than 99% but not 100% are displayed as ">100%".
void print_percentage(outputStream* st, size_t total, size_t part) {
  if (total == 0) {
    st->print("  ?%%");
  } else if (part == 0) {
    st->print("  0%%");
  } else if (part == total) {
    st->print("100%%");
  } else {
    // Note: clearly print very-small-but-not-0% and very-large-but-not-100% percentages.
    float p = ((float)part / total) * 100.0f;
    if (p < 1.0f) {
      st->print(" <1%%");
    } else if (p > 99.0f) {
      st->print(">99%%");
    } else {
      st->print("%3.0f%%", p);
    }
  }
}

// Returns size of this chunk type.
size_t get_size_for_nonhumongous_chunktype(ChunkIndex chunktype, bool is_class) {
  size_t size = 0;
  if (is_class) {
    switch(chunktype) {
      case SpecializedIndex: size = ClassSpecializedChunk; break;
      case SmallIndex: size = ClassSmallChunk; break;
      case MediumIndex: size = ClassMediumChunk; break;
      default:
        ShouldNotReachHere();
    }
  } else {
    switch(chunktype) {
      case SpecializedIndex: size = SpecializedChunk; break;
      case SmallIndex: size = SmallChunk; break;
      case MediumIndex: size = MediumChunk; break;
      default:
        ShouldNotReachHere();
    }
  }
  return size;
}

ChunkIndex get_chunk_type_by_size(size_t size, bool is_class) {
  if (is_class) {
    if (size == ClassSpecializedChunk) {
      return SpecializedIndex;
    } else if (size == ClassSmallChunk) {
      return SmallIndex;
    } else if (size == ClassMediumChunk) {
      return MediumIndex;
    } else if (size > ClassMediumChunk) {
      // A valid humongous chunk size is a multiple of the smallest chunk size.
      return HumongousIndex;
    }
  } else {
    if (size == SpecializedChunk) {
      return SpecializedIndex;
    } else if (size == SmallChunk) {
      return SmallIndex;
    } else if (size == MediumChunk) {
      return MediumIndex;
    } else if (size > MediumChunk) {
      // A valid humongous chunk size is a multiple of the smallest chunk size.
      return HumongousIndex;
    }
  }
  ShouldNotReachHere();
  return (ChunkIndex)-1;
}

ChunkIndex next_chunk_index(ChunkIndex i) {
  return (ChunkIndex) (i+1);
}

ChunkIndex prev_chunk_index(ChunkIndex i) {
  return (ChunkIndex) (i-1);
}
}
