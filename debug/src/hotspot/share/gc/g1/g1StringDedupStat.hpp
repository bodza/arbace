#ifndef SHARE_VM_GC_G1_G1STRINGDEDUPSTAT_HPP
#define SHARE_VM_GC_G1_G1STRINGDEDUPSTAT_HPP

#include "gc/shared/stringdedup/stringDedupStat.hpp"

// G1 extension for gathering/reporting generational statistics
class G1StringDedupStat : public StringDedupStat {
private:
  uintx  _deduped_young;
  uintx  _deduped_young_bytes;
  uintx  _deduped_old;
  uintx  _deduped_old_bytes;

  G1CollectedHeap* const _heap;

public:
  G1StringDedupStat();

  void deduped(oop obj, uintx bytes);

  void add(const StringDedupStat* const stat);

  void print_statistics(bool total) const;

  void reset();
};

#endif
