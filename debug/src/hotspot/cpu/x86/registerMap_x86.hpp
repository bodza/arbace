#ifndef CPU_X86_VM_REGISTERMAP_X86_HPP
#define CPU_X86_VM_REGISTERMAP_X86_HPP

// machine-dependent implemention for register maps
  friend class frame;

 private:
  // This is the hook for finding a register in an "well-known" location,
  // such as a register block of a predetermined format.
  address pd_location(VMReg reg) const;
  // no PD state to clear or copy:
  void pd_clear() { }
  void pd_initialize() { }
  void pd_initialize_from(const RegisterMap* map) { }

#endif
