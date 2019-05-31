#ifndef CPU_ZERO_VM_REGISTERMAP_ZERO_HPP
#define CPU_ZERO_VM_REGISTERMAP_ZERO_HPP

  // machine-dependent implemention for register maps
  friend class frame;

 private:
  // This is the hook for finding a register in an "well-known" location,
  // such as a register block of a predetermined format.
  // Since there is none, we just return NULL.
  // See registerMap_sparc.hpp for an example of grabbing registers
  // from register save areas of a standard layout.
  address pd_location(VMReg reg) const { return NULL; }

  // no PD state to clear or copy:
  void pd_clear() { }
  void pd_initialize() { }
  void pd_initialize_from(const RegisterMap* map) { }

#endif
