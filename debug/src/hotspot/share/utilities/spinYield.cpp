#include "precompiled.hpp"
#include "runtime/os.hpp"
#include "utilities/ostream.hpp"
#include "utilities/spinYield.hpp"

SpinYield::SpinYield(uint spin_limit, uint yield_limit) :
  _sleep_time(),
  _spins(0),
  _yields(0),
  _spin_limit(os::is_MP() ? spin_limit : 0),
  _yield_limit(yield_limit)
{ }

void SpinYield::yield_or_sleep() {
  if (_yields < _yield_limit) {
    ++_yields;
    os::naked_yield();
  } else {
    Ticks sleep_start = Ticks::now();
    os::naked_short_sleep(1);
    _sleep_time += Ticks::now() - sleep_start;
  }
}

static const char* print_separator(outputStream* s, const char* separator) {
  s->print("%s", separator);
  return ", ";
}

void SpinYield::report(outputStream* s) const {
  const char* initial_separator = "";
  const char* separator = initial_separator;
  if (_spins > 0) {             // Report spins, if any.
    separator = print_separator(s, separator);
    s->print("spins = %u", _spins);
  }
  if (_yields > 0) {            // Report yields, if any.
    separator = print_separator(s, separator);
    s->print("yields = %u", _yields);
  }
  if (_sleep_time.value() != 0) { // Report sleep duration, if slept.
    separator = print_separator(s, separator);
    s->print("sleep = " UINT64_FORMAT " usecs",
             _sleep_time.milliseconds());
  }
  if (separator == initial_separator) {
    s->print("no waiting");
  }
}
