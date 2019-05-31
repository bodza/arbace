#ifndef CPU_ZERO_VM_ICACHE_ZERO_HPP
#define CPU_ZERO_VM_ICACHE_ZERO_HPP

// Interface for updating the instruction cache.  Whenever the VM
// modifies code, part of the processor instruction cache potentially
// has to be flushed.  This implementation is empty: Zero never deals
// with code.

class ICache : public AbstractICache {
 public:
  static void initialize() { }
  static void invalidate_word(address addr) { }
  static void invalidate_range(address start, int nbytes) { }
};

#endif
