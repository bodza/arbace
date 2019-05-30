#ifndef SHARE_VM_GC_SHARED_GCCONFIGURATION_HPP
#define SHARE_VM_GC_SHARED_GCCONFIGURATION_HPP

#include "gc/shared/gcName.hpp"
#include "memory/universe.hpp"
#include "utilities/globalDefinitions.hpp"

class GCConfiguration {
 public:
  GCName young_collector() const;
  GCName old_collector() const;
  uint num_parallel_gc_threads() const;
  uint num_concurrent_gc_threads() const;
  bool uses_dynamic_gc_threads() const;
  bool is_explicit_gc_concurrent() const;
  bool is_explicit_gc_disabled() const;
  uintx gc_time_ratio() const;

  bool has_pause_target_default_value() const;
  uintx pause_target() const;
};

class GCTLABConfiguration {
 public:
  bool uses_tlabs() const;
  size_t min_tlab_size() const;
  uint tlab_refill_waste_limit() const;
};

class GCSurvivorConfiguration {
 public:
  intx initial_tenuring_threshold() const;
  intx max_tenuring_threshold() const;
};

class GCHeapConfiguration {
 public:
  size_t max_size() const;
  size_t min_size() const;
  size_t initial_size() const;
  bool uses_compressed_oops() const;
  Universe::NARROW_OOP_MODE narrow_oop_mode() const;
  uint object_alignment_in_bytes() const;
  int heap_address_size_in_bits() const;
};

class GCYoungGenerationConfiguration {
 public:
  bool has_max_size_default_value() const;
  uintx max_size() const;

  uintx min_size() const;
  intx new_ratio() const;
};

#endif
