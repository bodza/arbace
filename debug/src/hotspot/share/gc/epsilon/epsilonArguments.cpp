#include "precompiled.hpp"
#include "gc/epsilon/epsilonArguments.hpp"
#include "gc/epsilon/epsilonHeap.hpp"
#include "gc/epsilon/epsilonCollectorPolicy.hpp"
#include "gc/shared/gcArguments.inline.hpp"
#include "runtime/globals.hpp"
#include "runtime/globals_extension.hpp"
#include "runtime/vm_version.hpp"
#include "utilities/macros.hpp"

size_t EpsilonArguments::conservative_max_heap_alignment() {
  return UseLargePages ? os::large_page_size() : os::vm_page_size();
}

void EpsilonArguments::initialize() {
  GCArguments::initialize();

  assert(UseEpsilonGC, "Sanity");

  // Forcefully exit when OOME is detected. Nothing we can do at that point.
  if (FLAG_IS_DEFAULT(ExitOnOutOfMemoryError)) {
    FLAG_SET_DEFAULT(ExitOnOutOfMemoryError, true);
  }

  if (EpsilonMaxTLABSize < MinTLABSize) {
    warning("EpsilonMaxTLABSize < MinTLABSize, adjusting it to " SIZE_FORMAT, MinTLABSize);
    EpsilonMaxTLABSize = MinTLABSize;
  }

  if (!EpsilonElasticTLAB && EpsilonElasticTLABDecay) {
    warning("Disabling EpsilonElasticTLABDecay because EpsilonElasticTLAB is disabled");
    FLAG_SET_DEFAULT(EpsilonElasticTLABDecay, false);
  }
}

CollectedHeap* EpsilonArguments::create_heap() {
  return create_heap_with_policy<EpsilonHeap, EpsilonCollectorPolicy>();
}
