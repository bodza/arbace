#include "precompiled.hpp"
#include "gc/shared/gcArguments.hpp"
#include "runtime/arguments.hpp"
#include "runtime/globals.hpp"
#include "runtime/globals_extension.hpp"
#include "utilities/macros.hpp"

void GCArguments::initialize() {
  if (FullGCALot && FLAG_IS_DEFAULT(MarkSweepAlwaysCompactCount)) {
    MarkSweepAlwaysCompactCount = 1;  // Move objects every gc.
  }

  if (!(UseParallelGC || UseParallelOldGC) && FLAG_IS_DEFAULT(ScavengeBeforeFullGC)) {
    FLAG_SET_DEFAULT(ScavengeBeforeFullGC, false);
  }

  if (GCTimeLimit == 100) {
    // Turn off gc-overhead-limit-exceeded checks
    FLAG_SET_DEFAULT(UseGCOverheadLimit, false);
  }

  if (MinHeapFreeRatio == 100) {
    // Keeping the heap 100% free is hard ;-) so limit it to 99%.
    FLAG_SET_ERGO(uintx, MinHeapFreeRatio, 99);
  }

  if (!ClassUnloading) {
    // If class unloading is disabled, also disable concurrent class unloading.
    FLAG_SET_CMDLINE(bool, ClassUnloadingWithConcurrentMark, false);
  }
}
