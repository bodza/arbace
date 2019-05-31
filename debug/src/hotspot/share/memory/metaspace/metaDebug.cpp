#include "precompiled.hpp"
#include "logging/log.hpp"
#include "memory/metaspace/metaDebug.hpp"
#include "runtime/os.hpp"
#include "runtime/thread.hpp"
#include "utilities/debug.hpp"
#include "utilities/globalDefinitions.hpp"

namespace metaspace {

int Metadebug::_allocation_fail_alot_count = 0;

void Metadebug::init_allocation_fail_alot_count() {
  if (MetadataAllocationFailALot) {
    _allocation_fail_alot_count = 1+(long)((double)MetadataAllocationFailALotInterval*os::random()/(max_jint+1.0));
  }
}
}
