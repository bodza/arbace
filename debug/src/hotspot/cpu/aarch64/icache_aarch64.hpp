#ifndef CPU_AARCH64_VM_ICACHE_AARCH64_HPP
#define CPU_AARCH64_VM_ICACHE_AARCH64_HPP

// Interface for updating the instruction cache.  Whenever the VM
// modifies code, part of the processor instruction cache potentially
// has to be flushed.

class ICache : public AbstractICache {
 public:
  static void initialize();
  static void invalidate_word(address addr) {
    __clear_cache((char *)addr, (char *)(addr + 3));
  }
  static void invalidate_range(address start, int nbytes) {
    __clear_cache((char *)start, (char *)(start + nbytes));
  }
};

#endif
