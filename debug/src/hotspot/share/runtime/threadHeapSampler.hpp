#ifndef RUNTIME_THREADHEAPSAMPLER_HPP
#define RUNTIME_THREADHEAPSAMPLER_HPP

#include "memory/allocation.hpp"

class ThreadHeapSampler {
 private:
  size_t _bytes_until_sample;
  // Cheap random number generator
  static uint64_t _rnd;

  void pick_next_geometric_sample();
  void pick_next_sample(size_t overflowed_bytes = 0);
  static int _enabled;
  static int _sampling_interval;

  // Used for assertion mode to determine if there is a path to a TLAB slow path
  // without a collector present.
  size_t _collectors_present;

  static void init_log_table();

 public:
  ThreadHeapSampler() : _bytes_until_sample(0) {
    _rnd = static_cast<uint32_t>(reinterpret_cast<uintptr_t>(this));
    if (_rnd == 0) {
      _rnd = 1;
    }

    _collectors_present = 0;
  }

  size_t bytes_until_sample()                    { return _bytes_until_sample; }
  void set_bytes_until_sample(size_t bytes)      { _bytes_until_sample = bytes; }

  void check_for_sampling(oop obj, size_t size_in_bytes, size_t bytes_allocated_before);

  static int enabled();
  static void enable();
  static void disable();

  static void set_sampling_interval(int sampling_interval);
  static int get_sampling_interval();

  bool sampling_collector_present() const;
  bool remove_sampling_collector();
  bool add_sampling_collector();
};

#endif
