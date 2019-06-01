#include "precompiled.hpp"

#include "gc/shared/stringdedup/stringDedupStat.hpp"
#include "logging/log.hpp"

StringDedupStat::StringDedupStat() :
  _inspected(0),
  _skipped(0),
  _hashed(0),
  _known(0),
  _new(0),
  _new_bytes(0),
  _deduped(0),
  _deduped_bytes(0),
  _idle(0),
  _exec(0),
  _block(0),
  _start_concurrent(0.0),
  _end_concurrent(0.0),
  _start_phase(0.0),
  _idle_elapsed(0.0),
  _exec_elapsed(0.0),
  _block_elapsed(0.0) {
}

void StringDedupStat::add(const StringDedupStat* const stat) {
  _inspected           += stat->_inspected;
  _skipped             += stat->_skipped;
  _hashed              += stat->_hashed;
  _known               += stat->_known;
  _new                 += stat->_new;
  _new_bytes           += stat->_new_bytes;
  _deduped             += stat->_deduped;
  _deduped_bytes       += stat->_deduped_bytes;
  _idle                += stat->_idle;
  _exec                += stat->_exec;
  _block               += stat->_block;
  _idle_elapsed        += stat->_idle_elapsed;
  _exec_elapsed        += stat->_exec_elapsed;
  _block_elapsed       += stat->_block_elapsed;
}

void StringDedupStat::print_start(const StringDedupStat* last_stat) { }

void StringDedupStat::print_end(const StringDedupStat* last_stat, const StringDedupStat* total_stat) {
  double total_deduped_bytes_percent = 0.0;

  if (total_stat->_new_bytes > 0) {
    // Avoid division by zero
    total_deduped_bytes_percent = percent_of(total_stat->_deduped_bytes, total_stat->_new_bytes);
  }
}

void StringDedupStat::reset() {
  _inspected = 0;
  _skipped = 0;
  _hashed = 0;
  _known = 0;
  _new = 0;
  _new_bytes = 0;
  _deduped = 0;
  _deduped_bytes = 0;
  _idle = 0;
  _exec = 0;
  _block = 0;
  _start_concurrent = 0.0;
  _end_concurrent = 0.0;
  _start_phase = 0.0;
  _idle_elapsed = 0.0;
  _exec_elapsed = 0.0;
  _block_elapsed = 0.0;
}
