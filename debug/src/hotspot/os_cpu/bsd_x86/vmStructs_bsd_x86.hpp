#ifndef OS_CPU_BSD_X86_VM_VMSTRUCTS_BSD_X86_HPP
#define OS_CPU_BSD_X86_VM_VMSTRUCTS_BSD_X86_HPP

// These are the OS and CPU-specific fields, types and integer
// constants required by the Serviceability Agent. This file is
// referenced by vmStructs.cpp.

#define VM_STRUCTS_OS_CPU(nonstatic_field, static_field, unchecked_nonstatic_field, volatile_nonstatic_field, nonproduct_nonstatic_field, c2_nonstatic_field, unchecked_c1_static_field, unchecked_c2_static_field) \
 \
  /******************************/ \
  /* Threads (NOTE: incomplete) */ \
  /******************************/ \
  nonstatic_field(OSThread, _thread_id, OSThread::thread_id_t) \
  nonstatic_field(OSThread, _unique_thread_id, uint64_t)

#define VM_TYPES_OS_CPU(declare_type, declare_toplevel_type, declare_oop_type, declare_integer_type, declare_unsigned_integer_type, declare_c1_toplevel_type, declare_c2_type, declare_c2_toplevel_type) \
 \
  /**********************/ \
  /* Thread IDs         */ \
  /**********************/ \
 \
  declare_unsigned_integer_type(OSThread::thread_id_t)

#define VM_INT_CONSTANTS_OS_CPU(declare_constant, declare_preprocessor_constant, declare_c1_constant, declare_c2_constant, declare_c2_preprocessor_constant)

#define VM_LONG_CONSTANTS_OS_CPU(declare_constant, declare_preprocessor_constant, declare_c1_constant, declare_c2_constant, declare_c2_preprocessor_constant)

#endif
