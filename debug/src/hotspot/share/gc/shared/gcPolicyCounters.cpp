#include "precompiled.hpp"

#include "gc/shared/gcPolicyCounters.hpp"
#include "memory/resourceArea.hpp"

GCPolicyCounters::GCPolicyCounters(const char* name, int collectors, int generations) {
  if (UsePerfData) {
    EXCEPTION_MARK;
    ResourceMark rm;

    _name_space = "policy";

    char* cname = PerfDataManager::counter_name(_name_space, "name");
    PerfDataManager::create_string_constant(SUN_GC, cname, name, CHECK);

    cname = PerfDataManager::counter_name(_name_space, "collectors");
    PerfDataManager::create_constant(SUN_GC, cname,  PerfData::U_None,
                                     collectors, CHECK);

    cname = PerfDataManager::counter_name(_name_space, "generations");
    PerfDataManager::create_constant(SUN_GC, cname,  PerfData::U_None,
                                     generations, CHECK);

    cname = PerfDataManager::counter_name(_name_space, "maxTenuringThreshold");
    PerfDataManager::create_constant(SUN_GC, cname, PerfData::U_None,
                                     MaxTenuringThreshold, CHECK);

    cname = PerfDataManager::counter_name(_name_space, "tenuringThreshold");
    _tenuring_threshold = PerfDataManager::create_variable(SUN_GC, cname, PerfData::U_None, MaxTenuringThreshold, CHECK);

    cname = PerfDataManager::counter_name(_name_space, "desiredSurvivorSize");
    _desired_survivor_size = PerfDataManager::create_variable(SUN_GC, cname, PerfData::U_Bytes, CHECK);
  }
}
