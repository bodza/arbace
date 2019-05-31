#ifndef SHARE_VM_SERVICES_NMT_COMMON_HPP
#define SHARE_VM_SERVICES_NMT_COMMON_HPP

#include "memory/allocation.hpp"
#include "utilities/align.hpp"
#include "utilities/globalDefinitions.hpp"

#define CALC_OBJ_SIZE_IN_TYPE(obj, type) (align_up_(sizeof(obj), sizeof(type))/sizeof(type))

// Native memory tracking level
enum NMT_TrackingLevel {
  NMT_unknown = 0xFF,
  NMT_off     = 0x00,
  NMT_minimal = 0x01,
  NMT_summary = 0x02,
  NMT_detail  = 0x03
};

// Number of stack frames to capture. This is a
// build time decision.
const int NMT_TrackingStackDepth = 4;

// A few common utilities for native memory tracking
class NMTUtil : AllStatic {
 public:
  // Map memory type to index
  static inline int flag_to_index(MEMFLAGS flag) {
    const int index = flag & 0xff;
    return index;
  }

  // Map memory type to human readable name
  static const char* flag_to_name(MEMFLAGS flag) {
    return _memory_type_names[flag_to_index(flag)];
  }

  // Map an index to memory type
  static MEMFLAGS index_to_flag(int index) {
    return (MEMFLAGS)index;
  }

  // Memory size scale
  static const char* scale_name(size_t scale);
  static size_t scale_from_name(const char* scale);

  // Translate memory size in specified scale
  static size_t amount_in_scale(size_t amount, size_t scale) {
    return (amount + scale / 2) / scale;
  }
 private:
  static const char* _memory_type_names[mt_number_of_types];
};

#endif
