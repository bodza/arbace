#include "precompiled.hpp"

#include "jvm.h"
#include "gc/shared/ageTable.inline.hpp"
#include "gc/shared/ageTableTracer.hpp"
#include "gc/shared/collectedHeap.hpp"
#include "gc/shared/collectorPolicy.hpp"
#include "memory/resourceArea.hpp"
#include "logging/log.hpp"
#include "oops/oop.inline.hpp"
#include "utilities/copy.hpp"

AgeTable::AgeTable(bool global) {

  clear();

  if (UsePerfData && global) {

    ResourceMark rm;
    EXCEPTION_MARK;

    const char* agetable_ns = "generation.0.agetable";
    const char* bytes_ns = PerfDataManager::name_space(agetable_ns, "bytes");

    for (int age = 0; age < table_size; age ++) {
      char age_name[10];
      jio_snprintf(age_name, sizeof(age_name), "%2.2d", age);
      const char* cname = PerfDataManager::counter_name(bytes_ns, age_name);
      _perf_sizes[age] = PerfDataManager::create_variable(SUN_GC, cname,
                                                          PerfData::U_Bytes,
                                                          CHECK);
    }

    const char* cname = PerfDataManager::counter_name(agetable_ns, "size");
    PerfDataManager::create_constant(SUN_GC, cname, PerfData::U_None,
                                     table_size, CHECK);
  }
}

void AgeTable::clear() {
  for (size_t* p = sizes; p < sizes + table_size; ++p) {
    *p = 0;
  }
}

void AgeTable::merge(AgeTable* subTable) {
  for (int i = 0; i < table_size; i++) {
    sizes[i]+= subTable->sizes[i];
  }
}

uint AgeTable::compute_tenuring_threshold(size_t desired_survivor_size) {
  uint result;

  if (AlwaysTenure || NeverTenure) {
    result = MaxTenuringThreshold;
  } else {
    size_t total = 0;
    uint age = 1;
    while (age < table_size) {
      total += sizes[age];
      // check if including objects of age 'age' made us pass the desired
      // size, if so 'age' is the new threshold
      if (total > desired_survivor_size) break;
      age++;
    }
    result = age < MaxTenuringThreshold ? age : MaxTenuringThreshold;
  }

  return result;
}

void AgeTable::print_age_table(uint tenuring_threshold) {
  if (UsePerfData || AgeTableTracer::is_tenuring_distribution_event_enabled()) {
    size_t total = 0;
    uint age = 1;
    while (age < table_size) {
      size_t wordSize = sizes[age];
      total += wordSize;
      AgeTableTracer::send_tenuring_distribution_event(age, wordSize * oopSize);
      if (UsePerfData) {
        _perf_sizes[age]->set_value(wordSize * oopSize);
      }
      age++;
    }
  }
}
