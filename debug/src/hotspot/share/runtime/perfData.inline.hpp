#ifndef SHARE_VM_RUNTIME_PERFDATA_INLINE_HPP
#define SHARE_VM_RUNTIME_PERFDATA_INLINE_HPP

#include "runtime/perfData.hpp"
#include "utilities/globalDefinitions.hpp"
#include "utilities/growableArray.hpp"

inline int PerfDataList::length() {
  return _set->length();
}

inline void PerfDataList::append(PerfData *p) {
  _set->append(p);
}

inline void PerfDataList::remove(PerfData *p) {
  _set->remove(p);
}

inline PerfData* PerfDataList::at(int index) {
  return _set->at(index);
}

inline int PerfDataManager::count() {
  return _all->length();
}

inline int PerfDataManager::sampled_count() {
  return _sampled->length();
}

inline int PerfDataManager::constants_count() {
  return _constants->length();
}

inline bool PerfDataManager::exists(const char* name) {
  return _all->contains(name);
}

#endif
