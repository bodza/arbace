#ifndef SHARE_UTILITIES_SPINYIELD_HPP
#define SHARE_UTILITIES_SPINYIELD_HPP

#include "memory/allocation.hpp"
#include "utilities/globalDefinitions.hpp"
#include "utilities/ticks.hpp"

class outputStream;

extern "C" int SpinPause();

class SpinYield : public StackObj {
  Tickspan _sleep_time;
  uint _spins;
  uint _yields;
  uint _spin_limit;
  uint _yield_limit;

  void yield_or_sleep();

public:
  static const uint default_spin_limit = 4096;
  static const uint default_yield_limit = 64;

  // spin_limit is ignored (treated as zero) when !os::is_MP().
  explicit SpinYield(uint spin_limit = default_spin_limit,
                     uint yield_limit = default_yield_limit);

  // Perform next round of delay.
  void wait() {
    // Simple policy: return immediately (spinning) configured number
    // of times, then switch to yield/sleep.  Future work might
    // provide other policies, such as (1) always spin if system is
    // not saturated, or (2) sleeping if yielding is ineffective.
    if (_spins < _spin_limit) {
      ++_spins;
      SpinPause();
    } else {
      yield_or_sleep();
    }
  }

  // Write information about the wait duration to s.
  void report(outputStream* s) const;
};

#endif
