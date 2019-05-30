#ifndef CPU_ZERO_VM_RELOCINFO_ZERO_HPP
#define CPU_ZERO_VM_RELOCINFO_ZERO_HPP

  // machine-dependent parts of class relocInfo
 private:
  enum {
    // these constants mean nothing without an assembler
    offset_unit  =  1,
    format_width =  1
  };

 public:

  static bool mustIterateImmediateOopsInCode() { return true; }

#endif
