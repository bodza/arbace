#ifndef SHARE_VM_GC_SHARED_GCCAUSE_HPP
#define SHARE_VM_GC_SHARED_GCCAUSE_HPP

#include "memory/allocation.hpp"

//
// This class exposes implementation details of the various
// collector(s), and we need to be very careful with it. If
// use of this class grows, we should split it into public
// and implementation-private "causes".
//
// The definitions in the SA code should be kept in sync
// with the definitions here.
//

class GCCause : public AllStatic {
 public:
  enum Cause {
    /* public */
    _java_lang_system_gc,
    _full_gc_alot,
    _scavenge_alot,
    _allocation_profiler,
    _gc_locker,
    _heap_inspection,
    _heap_dump,
    _wb_young_gc,
    _wb_conc_mark,
    _wb_full_gc,

    /* implementation independent, but reserved for GC use */
    _no_gc,
    _no_cause_specified,
    _allocation_failure,

    /* implementation specific */

    _tenured_generation_full,
    _metadata_GC_threshold,
    _metadata_GC_clear_soft_refs,

    _cms_generation_full,
    _cms_initial_mark,
    _cms_final_remark,
    _cms_concurrent_mark,

    _old_generation_expanded_on_last_scavenge,
    _old_generation_too_full_to_scavenge,
    _adaptive_size_policy,

    _g1_inc_collection_pause,
    _g1_humongous_allocation,

    _dcmd_gc_run,

    _z_timer,
    _z_warmup,
    _z_allocation_rate,
    _z_allocation_stall,
    _z_proactive,

    _last_gc_cause
  };

  inline static bool is_user_requested_gc(GCCause::Cause cause) {
    return (cause == GCCause::_java_lang_system_gc ||
            cause == GCCause::_dcmd_gc_run);
  }

  inline static bool is_serviceability_requested_gc(GCCause::Cause cause) {
    return (cause == GCCause::_heap_inspection ||
            cause == GCCause::_heap_dump);
  }

  // Causes for collection of the tenured gernation
  inline static bool is_tenured_allocation_failure_gc(GCCause::Cause cause) {
    assert(cause != GCCause::_old_generation_too_full_to_scavenge && cause != GCCause::_old_generation_expanded_on_last_scavenge, "This GCCause may be correct but is not expected yet: %s", to_string(cause));
    // _tenured_generation_full or _cms_generation_full for full tenured generations
    // _adaptive_size_policy for a full collection after a young GC
    // _allocation_failure is the generic cause a collection which could result
    // in the collection of the tenured generation if there is not enough space
    // in the tenured generation to support a young GC.
    return (cause == GCCause::_tenured_generation_full ||
            cause == GCCause::_cms_generation_full ||
            cause == GCCause::_adaptive_size_policy ||
            cause == GCCause::_allocation_failure);
  }

  // Causes for collection of the young generation
  inline static bool is_allocation_failure_gc(GCCause::Cause cause) {
    // _allocation_failure is the generic cause a collection for allocation failure
    // _adaptive_size_policy is for a collecton done before a full GC
    return (cause == GCCause::_allocation_failure ||
            cause == GCCause::_adaptive_size_policy);
  }

  // Return a string describing the GCCause.
  static const char* to_string(GCCause::Cause cause);
};

#endif
