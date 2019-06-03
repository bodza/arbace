#ifndef SHARE_MEMORY_METASPACE_METADEBUG_HPP
#define SHARE_MEMORY_METASPACE_METADEBUG_HPP

#include "memory/allocation.hpp"

namespace metaspace {
class Metadebug : AllStatic {
  // Debugging support for Metaspaces
  static int _allocation_fail_alot_count;

 public:
  static void init_allocation_fail_alot_count();
};
}

#endif
