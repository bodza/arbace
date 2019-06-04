#include "precompiled.hpp"

#include "runtime/handles.inline.hpp"
#include "runtime/orderAccess.hpp"
#include "runtime/sharedRuntime.hpp"
#include "runtime/threadHeapSampler.hpp"

// Cheap random number generator
uint64_t ThreadHeapSampler::_rnd;
// Default is 512kb.
int ThreadHeapSampler::_sampling_interval = 512 * 1024;
int ThreadHeapSampler::_enabled;

// Statics for the fast log
static const int FastLogNumBits = 10;
static const int FastLogMask = (1 << FastLogNumBits) - 1;
static double log_table[1<<FastLogNumBits];  // Constant
static bool log_table_initialized;

// Returns the next prng value.
// pRNG is: aX+b mod c with a = 0x5DEECE66D, b = 0xB, c = 1 << 48
// This is the lrand64 generator.
static uint64_t next_random(uint64_t rnd) {
  const uint64_t PrngMult = 0x5DEECE66DLL;
  const uint64_t PrngAdd = 0xB;
  const uint64_t PrngModPower = 48;
  const uint64_t PrngModMask = ((uint64_t)1 << PrngModPower) - 1;
  return (PrngMult * rnd + PrngAdd) & PrngModMask;
}

static double fast_log2(const double & d) {
  uint64_t x = 0;
  x = *reinterpret_cast<const uint64_t*>(&d);
  const uint32_t x_high = x >> 32;
  const uint32_t y = x_high >> (20 - FastLogNumBits) & FastLogMask;
  const int32_t exponent = ((x_high >> 20) & 0x7FF) - 1023;
  return exponent + log_table[y];
}

// Generates a geometric variable with the specified mean (512K by default).
// This is done by generating a random number between 0 and 1 and applying
// the inverse cumulative distribution function for an exponential.
// Specifically: Let m be the inverse of the sample interval, then
// the probability distribution function is m*exp(-mx) so the CDF is
// p = 1 - exp(-mx), so
// q = 1 - p = exp(-mx)
// log_e(q) = -mx
// -log_e(q)/m = x
// log_2(q) * (-log_e(2) * 1/m) = x
// In the code, q is actually in the range 1 to 2**26, hence the -26 below
void ThreadHeapSampler::pick_next_geometric_sample() {
  _rnd = next_random(_rnd);
  // Take the top 26 bits as the random number
  // (This plus a 1<<58 sampling bound gives a max possible step of
  // 5194297183973780480 bytes.  In this case,
  // for sample_parameter = 1<<19, max possible step is
  // 9448372 bytes (24 bits).
  const uint64_t PrngModPower = 48;  // Number of bits in prng
  // The uint32_t cast is to prevent a (hard-to-reproduce) NAN
  // under piii debug for some binaries.
  double q = static_cast<uint32_t>(_rnd >> (PrngModPower - 26)) + 1.0;
  // Put the computed p-value through the CDF of a geometric.
  // For faster performance (save ~1/20th exec time), replace
  // min(0.0, FastLog2(q) - 26)  by  (Fastlog2(q) - 26.000705)
  // The value 26.000705 is used rather than 26 to compensate
  // for inaccuracies in FastLog2 which otherwise result in a
  // negative answer.
  double log_val = (fast_log2(q) - 26);
  double result = (0.0 < log_val ? 0.0 : log_val) * (-log(2.0) * (get_sampling_interval())) + 1;
  size_t interval = static_cast<size_t>(result);
  _bytes_until_sample = interval;
}

void ThreadHeapSampler::pick_next_sample(size_t overflowed_bytes) {
  if (get_sampling_interval() == 1) {
    _bytes_until_sample = 1;
    return;
  }

  pick_next_geometric_sample();

  // Try to correct sample size by removing extra space from last allocation.
  if (overflowed_bytes > 0 && _bytes_until_sample > overflowed_bytes) {
    _bytes_until_sample -= overflowed_bytes;
  }
}

void ThreadHeapSampler::check_for_sampling(oop obj, size_t allocation_size, size_t bytes_since_allocation) {
  size_t total_allocated_bytes = bytes_since_allocation + allocation_size;

  // If not yet time for a sample, skip it.
  if (total_allocated_bytes < _bytes_until_sample) {
    _bytes_until_sample -= total_allocated_bytes;
    return;
  }

  size_t overflow_bytes = total_allocated_bytes - _bytes_until_sample;
  pick_next_sample(overflow_bytes);
}

void ThreadHeapSampler::init_log_table() {
  MutexLockerEx mu(ThreadHeapSampler_lock, Mutex::_no_safepoint_check_flag);

  if (log_table_initialized) {
    return;
  }

  for (int i = 0; i < (1 << FastLogNumBits); i++) {
    log_table[i] = (log(1.0 + static_cast<double>(i + 0.5) / (1 << FastLogNumBits)) / log(2.0));
  }

  log_table_initialized = true;
}

void ThreadHeapSampler::enable() {
  // Done here to be done when things have settled. This adds a mutex lock but
  // presumably, users won't be enabling and disabling all the time.
  init_log_table();
  OrderAccess::release_store(&_enabled, 1);
}

int ThreadHeapSampler::enabled() {
  return OrderAccess::load_acquire(&_enabled);
}

void ThreadHeapSampler::disable() {
  OrderAccess::release_store(&_enabled, 0);
}

int ThreadHeapSampler::get_sampling_interval() {
  return OrderAccess::load_acquire(&_sampling_interval);
}

void ThreadHeapSampler::set_sampling_interval(int sampling_interval) {
  OrderAccess::release_store(&_sampling_interval, sampling_interval);
}

// Methods used in assertion mode to check if a collector is present or not at
// the moment of TLAB sampling, ie a slow allocation path.
bool ThreadHeapSampler::sampling_collector_present() const {
  return _collectors_present > 0;
}

bool ThreadHeapSampler::remove_sampling_collector() {
  _collectors_present--;
  return true;
}

bool ThreadHeapSampler::add_sampling_collector() {
  _collectors_present++;
  return true;
}
