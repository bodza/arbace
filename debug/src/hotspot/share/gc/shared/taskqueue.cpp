#include "precompiled.hpp"
#include "gc/shared/taskqueue.hpp"
#include "oops/oop.inline.hpp"
#include "logging/log.hpp"
#include "runtime/atomic.hpp"
#include "runtime/os.hpp"
#include "runtime/thread.inline.hpp"
#include "utilities/debug.hpp"
#include "utilities/stack.inline.hpp"

#ifdef TRACESPINNING
uint ParallelTaskTerminator::_total_yields = 0;
uint ParallelTaskTerminator::_total_spins = 0;
uint ParallelTaskTerminator::_total_peeks = 0;
#endif

#if TASKQUEUE_STATS
const char * const TaskQueueStats::_names[last_stat_id] = {
  "qpush", "qpop", "qpop-s", "qattempt", "qsteal", "opush", "omax"
};

TaskQueueStats & TaskQueueStats::operator +=(const TaskQueueStats & addend) {
  for (unsigned int i = 0; i < last_stat_id; ++i) {
    _stats[i] += addend._stats[i];
  }
  return *this;
}

void TaskQueueStats::print_header(unsigned int line, outputStream* const stream, unsigned int width) {
  // Use a width w: 1 <= w <= max_width
  const unsigned int max_width = 40;
  const unsigned int w = MAX2(MIN2(width, max_width), 1U);

  if (line == 0) { // spaces equal in width to the header
    const unsigned int hdr_width = w * last_stat_id + last_stat_id - 1;
    stream->print("%*s", hdr_width, " ");
  } else if (line == 1) { // labels
    stream->print("%*s", w, _names[0]);
    for (unsigned int i = 1; i < last_stat_id; ++i) {
      stream->print(" %*s", w, _names[i]);
    }
  } else if (line == 2) { // dashed lines
    char dashes[max_width + 1];
    memset(dashes, '-', w);
    dashes[w] = '\0';
    stream->print("%s", dashes);
    for (unsigned int i = 1; i < last_stat_id; ++i) {
      stream->print(" %s", dashes);
    }
  }
}

void TaskQueueStats::print(outputStream* stream, unsigned int width) const {
  #define FMT SIZE_FORMAT_W(*)
  stream->print(FMT, width, _stats[0]);
  for (unsigned int i = 1; i < last_stat_id; ++i) {
    stream->print(" " FMT, width, _stats[i]);
  }
  #undef FMT
}

#endif

int TaskQueueSetSuper::randomParkAndMiller(int *seed0) {
  const int a =      16807;
  const int m = 2147483647;
  const int q =     127773;  /* m div a */
  const int r =       2836;  /* m mod a */
  int seed = *seed0;
  int hi   = seed / q;
  int lo   = seed % q;
  int test = a * lo - r * hi;
  if (test > 0)
    seed = test;
  else
    seed = test + m;
  *seed0 = seed;
  return seed;
}

ParallelTaskTerminator::ParallelTaskTerminator(uint n_threads, TaskQueueSetSuper* queue_set) :
  _n_threads(n_threads),
  _queue_set(queue_set),
  _offered_termination(0) { }

bool ParallelTaskTerminator::peek_in_queue_set() {
  return _queue_set->peek();
}

void ParallelTaskTerminator::yield() {
  os::naked_yield();
}

void ParallelTaskTerminator::sleep(uint millis) {
  os::sleep(Thread::current(), millis, false);
}

bool
ParallelTaskTerminator::offer_termination(TerminatorTerminator* terminator) {
  Atomic::inc(&_offered_termination);

  uint yield_count = 0;
  // Number of hard spin loops done since last yield
  uint hard_spin_count = 0;
  // Number of iterations in the hard spin loop.
  uint hard_spin_limit = WorkStealingHardSpins;

  // If WorkStealingSpinToYieldRatio is 0, no hard spinning is done.
  // If it is greater than 0, then start with a small number
  // of spins and increase number with each turn at spinning until
  // the count of hard spins exceeds WorkStealingSpinToYieldRatio.
  // Then do a yield() call and start spinning afresh.
  if (WorkStealingSpinToYieldRatio > 0) {
    hard_spin_limit = WorkStealingHardSpins >> WorkStealingSpinToYieldRatio;
    hard_spin_limit = MAX2(hard_spin_limit, 1U);
  }
  // Remember the initial spin limit.
  uint hard_spin_start = hard_spin_limit;

  // Loop waiting for all threads to offer termination or
  // more work.
  while (true) {
    // Are all threads offering termination?
    if (_offered_termination == _n_threads) {
      return true;
    } else {
      // Look for more work.
      // Periodically sleep() instead of yield() to give threads
      // waiting on the cores the chance to grab this code
      if (yield_count <= WorkStealingYieldsBeforeSleep) {
        // Do a yield or hardspin.  For purposes of deciding whether
        // to sleep, count this as a yield.
        yield_count++;

        // Periodically call yield() instead spinning
        // After WorkStealingSpinToYieldRatio spins, do a yield() call
        // and reset the counts and starting limit.
        if (hard_spin_count > WorkStealingSpinToYieldRatio) {
          yield();
          hard_spin_count = 0;
          hard_spin_limit = hard_spin_start;
#ifdef TRACESPINNING
          _total_yields++;
#endif
        } else {
          // Hard spin this time
          // Increase the hard spinning period but only up to a limit.
          hard_spin_limit = MIN2(2*hard_spin_limit,
                                 (uint) WorkStealingHardSpins);
          for (uint j = 0; j < hard_spin_limit; j++) {
            SpinPause();
          }
          hard_spin_count++;
#ifdef TRACESPINNING
          _total_spins++;
#endif
        }
      } else {
        log_develop_trace(gc, task)("ParallelTaskTerminator::offer_termination() thread " PTR_FORMAT " sleeps after %u yields",
                                    p2i(Thread::current()), yield_count);
        yield_count = 0;
        // A sleep will cause this processor to seek work on another processor's
        // runqueue, if it has nothing else to run (as opposed to the yield
        // which may only move the thread to the end of the this processor's
        // runqueue).
        sleep(WorkStealingSleepMillis);
      }

#ifdef TRACESPINNING
      _total_peeks++;
#endif
      if (peek_in_queue_set() || (terminator != NULL && terminator->should_exit_termination())) {
        Atomic::dec(&_offered_termination);
        return false;
      }
    }
  }
}

#ifdef TRACESPINNING
void ParallelTaskTerminator::print_termination_counts() {
  log_trace(gc, task)("ParallelTaskTerminator Total yields: %u Total spins: %u Total peeks: %u", total_yields(), total_spins(), total_peeks());
}
#endif

void ParallelTaskTerminator::reset_for_reuse() {
  if (_offered_termination != 0) {
    _offered_termination = 0;
  }
}

void ParallelTaskTerminator::reset_for_reuse(uint n_threads) {
  reset_for_reuse();
  _n_threads = n_threads;
}
