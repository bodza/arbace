#include "precompiled.hpp"

#include "gc/shared/collectedHeap.hpp"
#include "gc/shared/gcConfiguration.hpp"
#include "memory/universe.hpp"
#include "runtime/arguments.hpp"
#include "runtime/globals.hpp"
#include "utilities/debug.hpp"

GCName GCConfiguration::young_collector() const {
  if (UseG1GC) {
    return G1New;
  }

  if (UseParallelGC) {
    return ParallelScavenge;
  }

  if (UseConcMarkSweepGC) {
    return ParNew;
  }

  if (UseZGC) {
    return NA;
  }

  return DefNew;
}

GCName GCConfiguration::old_collector() const {
  if (UseG1GC) {
    return G1Old;
  }

  if (UseConcMarkSweepGC) {
    return ConcurrentMarkSweep;
  }

  if (UseParallelOldGC) {
    return ParallelOld;
  }

  if (UseZGC) {
    return Z;
  }

  return SerialOld;
}

uint GCConfiguration::num_parallel_gc_threads() const {
  return ParallelGCThreads;
}

uint GCConfiguration::num_concurrent_gc_threads() const {
  return ConcGCThreads;
}

bool GCConfiguration::uses_dynamic_gc_threads() const {
  return UseDynamicNumberOfGCThreads;
}

bool GCConfiguration::is_explicit_gc_concurrent() const {
  return ExplicitGCInvokesConcurrent;
}

bool GCConfiguration::is_explicit_gc_disabled() const {
  return DisableExplicitGC;
}

bool GCConfiguration::has_pause_target_default_value() const {
  return FLAG_IS_DEFAULT(MaxGCPauseMillis);
}

uintx GCConfiguration::pause_target() const {
  return MaxGCPauseMillis;
}

uintx GCConfiguration::gc_time_ratio() const {
  return GCTimeRatio;
}

bool GCTLABConfiguration::uses_tlabs() const {
  return UseTLAB;
}

size_t GCTLABConfiguration::min_tlab_size() const {
  return MinTLABSize;
}

uint GCTLABConfiguration::tlab_refill_waste_limit() const {
  return TLABRefillWasteFraction;
}

intx GCSurvivorConfiguration::max_tenuring_threshold() const {
  return MaxTenuringThreshold;
}

intx GCSurvivorConfiguration::initial_tenuring_threshold() const {
  return InitialTenuringThreshold;
}

size_t GCHeapConfiguration::max_size() const {
  return MaxHeapSize;
}

size_t GCHeapConfiguration::min_size() const {
  return Arguments::min_heap_size();
}

size_t GCHeapConfiguration::initial_size() const {
  return InitialHeapSize;
}

bool GCHeapConfiguration::uses_compressed_oops() const {
  return UseCompressedOops;
}

Universe::NARROW_OOP_MODE GCHeapConfiguration::narrow_oop_mode() const {
  return Universe::narrow_oop_mode();
}

uint GCHeapConfiguration::object_alignment_in_bytes() const {
  return ObjectAlignmentInBytes;
}

int GCHeapConfiguration::heap_address_size_in_bits() const {
  return BitsPerHeapOop;
}

bool GCYoungGenerationConfiguration::has_max_size_default_value() const {
  return FLAG_IS_DEFAULT(MaxNewSize);
}

uintx GCYoungGenerationConfiguration::max_size() const {
  return MaxNewSize;
}

uintx GCYoungGenerationConfiguration::min_size() const {
  return NewSize;
}

intx GCYoungGenerationConfiguration::new_ratio() const {
  return NewRatio;
}
