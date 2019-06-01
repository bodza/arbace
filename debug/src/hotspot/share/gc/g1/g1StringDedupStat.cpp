#include "precompiled.hpp"

#include "gc/g1/g1CollectedHeap.inline.hpp"
#include "gc/g1/g1StringDedupStat.hpp"
#include "logging/log.hpp"

G1StringDedupStat::G1StringDedupStat() : StringDedupStat(),
  _deduped_young(0),
  _deduped_young_bytes(0),
  _deduped_old(0),
  _deduped_old_bytes(0),
  _heap(G1CollectedHeap::heap()) {
}

void G1StringDedupStat::deduped(oop obj, uintx bytes) {
  StringDedupStat::deduped(obj, bytes);
  if (_heap->is_in_young(obj)) {
    _deduped_young ++;
    _deduped_young_bytes += bytes;
  } else {
    _deduped_old ++;
    _deduped_old_bytes += bytes;
  }
}

void G1StringDedupStat::add(const StringDedupStat* const stat) {
  StringDedupStat::add(stat);
  const G1StringDedupStat* const g1_stat = (const G1StringDedupStat* const)stat;
  _deduped_young += g1_stat->_deduped_young;
  _deduped_young_bytes += g1_stat->_deduped_young_bytes;
  _deduped_old += g1_stat->_deduped_old;
  _deduped_old_bytes += g1_stat->_deduped_old_bytes;
}

void G1StringDedupStat::reset() {
  StringDedupStat::reset();
  _deduped_young = 0;
  _deduped_young_bytes = 0;
  _deduped_old = 0;
  _deduped_old_bytes = 0;
}
