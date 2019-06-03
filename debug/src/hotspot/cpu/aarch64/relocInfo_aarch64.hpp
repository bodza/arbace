#ifndef CPU_AARCH64_VM_RELOCINFO_AARCH64_HPP
#define CPU_AARCH64_VM_RELOCINFO_AARCH64_HPP

  // machine-dependent parts of class relocInfo
 private:
  enum {
    // Relocations are byte-aligned.
    offset_unit        =  1,
    // We don't use format().
    format_width       =  0
  };

 public:
  // This platform has no oops in the code that are not also
  // listed in the oop section.
  static bool mustIterateImmediateOopsInCode() { return false; }

#endif
