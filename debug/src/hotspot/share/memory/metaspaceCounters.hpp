#ifndef SHARE_VM_MEMORY_METASPACECOUNTERS_HPP
#define SHARE_VM_MEMORY_METASPACECOUNTERS_HPP

#include "memory/allocation.hpp"

class MetaspacePerfCounters;

class MetaspaceCounters: public AllStatic {
  static MetaspacePerfCounters* _perf_counters;
  static size_t used();
  static size_t capacity();
  static size_t max_capacity();

 public:
  static void initialize_performance_counters();
  static void update_performance_counters();
};

class CompressedClassSpaceCounters: public AllStatic {
  static MetaspacePerfCounters* _perf_counters;
  static size_t used();
  static size_t capacity();
  static size_t max_capacity();

 public:
  static void initialize_performance_counters();
  static void update_performance_counters();
};

#endif
