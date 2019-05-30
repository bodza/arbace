#ifndef CPU_AARCH64_VM_VMSTRUCTS_AARCH64_HPP
#define CPU_AARCH64_VM_VMSTRUCTS_AARCH64_HPP

// These are the CPU-specific fields, types and integer
// constants required by the Serviceability Agent. This file is
// referenced by vmStructs.cpp.

#define VM_STRUCTS_CPU(nonstatic_field, static_field, unchecked_nonstatic_field, volatile_nonstatic_field, nonproduct_nonstatic_field, c2_nonstatic_field, unchecked_c1_static_field, unchecked_c2_static_field) \
  volatile_nonstatic_field(JavaFrameAnchor, _last_Java_fp, intptr_t*)

#define VM_TYPES_CPU(declare_type, declare_toplevel_type, declare_oop_type, declare_integer_type, declare_unsigned_integer_type, declare_c1_toplevel_type, declare_c2_type, declare_c2_toplevel_type)

#define VM_INT_CONSTANTS_CPU(declare_constant, declare_preprocessor_constant, declare_c1_constant, declare_c2_constant, declare_c2_preprocessor_constant)

#define VM_LONG_CONSTANTS_CPU(declare_constant, declare_preprocessor_constant, declare_c1_constant, declare_c2_constant, declare_c2_preprocessor_constant)

#endif
