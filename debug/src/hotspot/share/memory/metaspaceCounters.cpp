#include "precompiled.hpp"

#include "memory/metaspaceCounters.hpp"
#include "memory/resourceArea.hpp"
#include "runtime/globals.hpp"
#include "runtime/perfData.hpp"
#include "utilities/exceptions.hpp"

class MetaspacePerfCounters: public CHeapObj<mtInternal> {
  friend class VMStructs;
  PerfVariable*      _capacity;
  PerfVariable*      _used;
  PerfVariable*      _max_capacity;

  PerfVariable* create_variable(const char *ns, const char *name, size_t value, TRAPS) {
    const char *path = PerfDataManager::counter_name(ns, name);
    return PerfDataManager::create_variable(SUN_GC, path, PerfData::U_Bytes, value, THREAD);
  }

  void create_constant(const char *ns, const char *name, size_t value, TRAPS) {
    const char *path = PerfDataManager::counter_name(ns, name);
    PerfDataManager::create_constant(SUN_GC, path, PerfData::U_Bytes, value, THREAD);
  }

 public:
  MetaspacePerfCounters(const char* ns, size_t min_capacity, size_t curr_capacity, size_t max_capacity, size_t used) {
    EXCEPTION_MARK;
    ResourceMark rm;

    create_constant(ns, "minCapacity", min_capacity, THREAD);
    _capacity = create_variable(ns, "capacity", curr_capacity, THREAD);
    _max_capacity = create_variable(ns, "maxCapacity", max_capacity, THREAD);
    _used = create_variable(ns, "used", used, THREAD);
  }

  void update(size_t capacity, size_t max_capacity, size_t used) {
    _capacity->set_value(capacity);
    _max_capacity->set_value(max_capacity);
    _used->set_value(used);
  }
};

MetaspacePerfCounters* MetaspaceCounters::_perf_counters = NULL;

size_t MetaspaceCounters::used() {
  return MetaspaceUtils::used_bytes();
}

size_t MetaspaceCounters::capacity() {
  return MetaspaceUtils::committed_bytes();
}

size_t MetaspaceCounters::max_capacity() {
  return MetaspaceUtils::reserved_bytes();
}

void MetaspaceCounters::initialize_performance_counters() {
  if (UsePerfData) {
    size_t min_capacity = 0;
    _perf_counters = new MetaspacePerfCounters("metaspace", min_capacity,
                                               capacity(), max_capacity(), used());
  }
}

void MetaspaceCounters::update_performance_counters() {
  if (UsePerfData) {
    _perf_counters->update(capacity(), max_capacity(), used());
  }
}

MetaspacePerfCounters* CompressedClassSpaceCounters::_perf_counters = NULL;

size_t CompressedClassSpaceCounters::used() {
  return MetaspaceUtils::used_bytes(Metaspace::ClassType);
}

size_t CompressedClassSpaceCounters::capacity() {
  return MetaspaceUtils::committed_bytes(Metaspace::ClassType);
}

size_t CompressedClassSpaceCounters::max_capacity() {
  return MetaspaceUtils::reserved_bytes(Metaspace::ClassType);
}

void CompressedClassSpaceCounters::update_performance_counters() {
  if (UsePerfData && UseCompressedClassPointers) {
    _perf_counters->update(capacity(), max_capacity(), used());
  }
}

void CompressedClassSpaceCounters::initialize_performance_counters() {
  if (UsePerfData) {
    const char* ns = "compressedclassspace";

    if (UseCompressedClassPointers) {
      size_t min_capacity = 0;
      _perf_counters = new MetaspacePerfCounters(ns, min_capacity, capacity(),
                                                 max_capacity(), used());
    } else {
      _perf_counters = new MetaspacePerfCounters(ns, 0, 0, 0, 0);
    }
  }
}
