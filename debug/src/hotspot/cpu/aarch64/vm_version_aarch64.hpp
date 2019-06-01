#ifndef CPU_AARCH64_VM_VM_VERSION_AARCH64_HPP
#define CPU_AARCH64_VM_VM_VERSION_AARCH64_HPP

#include "runtime/globals_extension.hpp"
#include "runtime/vm_version.hpp"
#include "utilities/sizes.hpp"

class VM_Version : public Abstract_VM_Version {
  friend class JVMCIVMStructs;

protected:
  static int _cpu;
  static int _model;
  static int _model2;
  static int _variant;
  static int _revision;
  static int _stepping;

  struct PsrInfo {
    uint32_t dczid_el0;
    uint32_t ctr_el0;
  };
  static PsrInfo _psr_info;
  static void get_processor_features();

public:
  // Initialization
  static void initialize();

  static bool expensive_load(int ld_size, int scale) {
    if (cpu_family() == CPU_ARM) {
      // Half-word load with index shift by 1 (aka scale is 2) has
      // extra cycle latency, e.g. ldrsh w0, [x1,w2,sxtw #1].
      if (ld_size == 2 && scale == 2) {
        return true;
      }
    }
    return false;
  }

  enum Family {
    CPU_ARM       = 'A',
    CPU_BROADCOM  = 'B',
    CPU_CAVIUM    = 'C',
    CPU_DEC       = 'D',
    CPU_INFINEON  = 'I',
    CPU_MOTOROLA  = 'M',
    CPU_NVIDIA    = 'N',
    CPU_AMCC      = 'P',
    CPU_QUALCOM   = 'Q',
    CPU_MARVELL   = 'V',
    CPU_INTEL     = 'i',
  };

  enum Feature_Flag {
    CPU_FP           = (1<<0),
    CPU_ASIMD        = (1<<1),
    CPU_EVTSTRM      = (1<<2),
    CPU_AES          = (1<<3),
    CPU_PMULL        = (1<<4),
    CPU_SHA1         = (1<<5),
    CPU_SHA2         = (1<<6),
    CPU_CRC32        = (1<<7),
    CPU_LSE          = (1<<8),
    CPU_STXR_PREFETCH= (1 << 29),
    CPU_A53MAC       = (1 << 30),
    CPU_DMB_ATOMICS  = (1 << 31),
  };

  static int cpu_family()                     { return _cpu; }
  static int cpu_model()                      { return _model; }
  static int cpu_model2()                     { return _model2; }
  static int cpu_variant()                    { return _variant; }
  static int cpu_revision()                   { return _revision; }
  static ByteSize dczid_el0_offset() { return byte_offset_of(PsrInfo, dczid_el0); }
  static ByteSize ctr_el0_offset()   { return byte_offset_of(PsrInfo, ctr_el0); }
  static bool is_zva_enabled() {
    // Check the DZP bit (bit 4) of dczid_el0 is zero
    // and block size (bit 0~3) is not zero.
    return ((_psr_info.dczid_el0 & 0x10) == 0 && (_psr_info.dczid_el0 & 0xf) != 0);
  }
  static int zva_length() {
    return 4 << (_psr_info.dczid_el0 & 0xf);
  }
  static int icache_line_size() {
    return (1 << (_psr_info.ctr_el0 & 0x0f)) * 4;
  }
  static int dcache_line_size() {
    return (1 << ((_psr_info.ctr_el0 >> 16) & 0x0f)) * 4;
  }
};

#endif
