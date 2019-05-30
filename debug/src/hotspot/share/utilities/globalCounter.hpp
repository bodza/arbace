#ifndef SHARE_UTILITIES_GLOBAL_COUNTER_HPP
#define SHARE_UTILITIES_GLOBAL_COUNTER_HPP

#include "memory/allocation.hpp"
#include "memory/padded.hpp"

class Thread;

// The GlobalCounter provides a synchronization mechanism between threads for
// safe memory reclamation and other ABA problems. All readers must call
// critical_section_begin before reading the volatile data and
// critical_section_end afterwards. The write side must call write_synchronize
// before reclaming the memory. The read-path only does an uncontented store
// to a thread-local-storage and fence to stop any loads from floating up, thus
// light weight and wait-free. The write-side is more heavy since it must check
// all readers and wait until they have left the generation. (a system memory
// barrier can be used on write-side to remove fence in read-side,
// not implemented).
class GlobalCounter : public AllStatic {
 private:
  // Since do not know what we will end up next to in BSS, we make sure the
  // counter is on a seperate cacheline.
  struct PaddedCounter {
    DEFINE_PAD_MINUS_SIZE(0, DEFAULT_CACHE_LINE_SIZE/2, 0);
    volatile uintx _counter;
    DEFINE_PAD_MINUS_SIZE(1, DEFAULT_CACHE_LINE_SIZE/2, sizeof(volatile uintx));
  };

  // The global counter
  static PaddedCounter _global_counter;

  // Bit 0 is active bit.
  static const uintx COUNTER_ACTIVE = 1;
  // Thus we increase counter by 2.
  static const uintx COUNTER_INCREMENT = 2;

  // The per thread scanning closure.
  class CounterThreadCheck;

 public:
  // Must be called before accessing the data. Only threads accessible lock-free
  // can used this. Those included now are all Threads on SMR ThreadsList and
  // the VMThread. Nesting is not yet supported.
  static void critical_section_begin(Thread *thread);

  // Must be called after finished accessing the data.
  // Do not provide fence, allows load/stores moving into the critical section.
  static void critical_section_end(Thread *thread);

  // Make the data inaccessible to readers before calling. When this call
  // returns it's safe to reclaim the data.
  static void write_synchronize();

  // A scoped object for a reads-side critical-section.
  class CriticalSection;
};

#endif
