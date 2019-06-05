#include "precompiled.hpp"

#include "jvm.h"
#include "gc/shared/ageTable.inline.hpp"
#include "gc/shared/collectedHeap.hpp"
#include "gc/shared/collectorPolicy.hpp"
#include "memory/resourceArea.hpp"
#include "oops/oop.inline.hpp"
#include "utilities/copy.hpp"

AgeTable::AgeTable(bool global) {
  clear();
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
