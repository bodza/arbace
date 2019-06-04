#include "precompiled.hpp"

#include "gc/shared/gcCause.hpp"

const char* GCCause::to_string(GCCause::Cause cause) {
  switch (cause) {
    case _java_lang_system_gc:                      return "System.gc()";
    case _full_gc_alot:                             return "FullGCAlot";
    case _scavenge_alot:                            return "ScavengeAlot";
    case _allocation_profiler:                      return "Allocation Profiler";
    case _gc_locker:                                return "GCLocker Initiated GC";
    case _heap_inspection:                          return "Heap Inspection Initiated GC";
    case _heap_dump:                                return "Heap Dump Initiated GC";
    case _no_gc:                                    return "No GC";
    case _allocation_failure:                       return "Allocation Failure";
    case _tenured_generation_full:                  return "Tenured Generation Full";
    case _metadata_GC_threshold:                    return "Metadata GC Threshold";
    case _metadata_GC_clear_soft_refs:              return "Metadata GC Clear Soft References";
    case _old_generation_expanded_on_last_scavenge: return "Old Generation Expanded On Last Scavenge";
    case _old_generation_too_full_to_scavenge:      return "Old Generation Too Full To Scavenge";
    case _adaptive_size_policy:                     return "Ergonomics";
    case _g1_inc_collection_pause:                  return "G1 Evacuation Pause";
    case _g1_humongous_allocation:                  return "G1 Humongous Allocation";
    case _dcmd_gc_run:                              return "Diagnostic Command";
    case _z_timer:                                  return "Timer";
    case _z_warmup:                                 return "Warmup";
    case _z_allocation_rate:                        return "Allocation Rate";
    case _z_allocation_stall:                       return "Allocation Stall";
    case _z_proactive:                              return "Proactive";
    case _last_gc_cause:                            return "ILLEGAL VALUE - last gc cause - ILLEGAL VALUE";

    default:
      return "unknown GCCause";
  }
  ShouldNotReachHere();
}
