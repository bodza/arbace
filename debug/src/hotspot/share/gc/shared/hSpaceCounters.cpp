#include "precompiled.hpp"

#include "gc/shared/hSpaceCounters.hpp"
#include "memory/allocation.inline.hpp"
#include "memory/resourceArea.hpp"
#include "runtime/perfData.hpp"

HSpaceCounters::HSpaceCounters(const char* name_space, const char* name, int ordinal, size_t max_size, size_t initial_capacity) {

  if (UsePerfData) {
    EXCEPTION_MARK;
    ResourceMark rm;

    const char* cns = PerfDataManager::name_space(name_space, "space", ordinal);

    _name_space = NEW_C_HEAP_ARRAY(char, strlen(cns)+1, mtGC);
    strcpy(_name_space, cns);

    const char* cname = PerfDataManager::counter_name(_name_space, "name");
    PerfDataManager::create_string_constant(SUN_GC, cname, name, CHECK);

    cname = PerfDataManager::counter_name(_name_space, "maxCapacity");
    PerfDataManager::create_constant(SUN_GC, cname, PerfData::U_Bytes,
                                     (jlong)max_size, CHECK);

    cname = PerfDataManager::counter_name(_name_space, "capacity");
    _capacity = PerfDataManager::create_variable(SUN_GC, cname,
                                                 PerfData::U_Bytes,
                                                 initial_capacity, CHECK);

    cname = PerfDataManager::counter_name(_name_space, "used");
    _used = PerfDataManager::create_variable(SUN_GC, cname, PerfData::U_Bytes,
                                             (jlong) 0, CHECK);

    cname = PerfDataManager::counter_name(_name_space, "initCapacity");
    PerfDataManager::create_constant(SUN_GC, cname, PerfData::U_Bytes,
                                     initial_capacity, CHECK);
  }
}

HSpaceCounters::~HSpaceCounters() {
  if (_name_space != NULL) {
    FREE_C_HEAP_ARRAY(char, _name_space);
  }
}

void HSpaceCounters::update_capacity(size_t v) {
  _capacity->set_value(v);
}

void HSpaceCounters::update_used(size_t v) {
  _used->set_value(v);
}

void HSpaceCounters::update_all(size_t capacity, size_t used) {
  update_capacity(capacity);
  update_used(used);
}
