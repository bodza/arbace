#ifndef OS_BSD_VM_VMSTRUCTS_BSD_HPP
#define OS_BSD_VM_VMSTRUCTS_BSD_HPP

#include <dlfcn.h>

// These are the OS-specific fields, types and integer
// constants required by the Serviceability Agent. This file is
// referenced by vmStructs.cpp.

#define VM_STRUCTS_OS(nonstatic_field, static_field, unchecked_nonstatic_field, volatile_nonstatic_field, nonproduct_nonstatic_field, c2_nonstatic_field, unchecked_c1_static_field, unchecked_c2_static_field)

#define VM_TYPES_OS(declare_type, declare_toplevel_type, declare_oop_type, declare_integer_type, declare_unsigned_integer_type, declare_c1_toplevel_type, declare_c2_type, declare_c2_toplevel_type)

#define VM_ADDRESSES_OS(declare_address, declare_preprocessor_address, declare_function) declare_preprocessor_address("RTLD_DEFAULT", RTLD_DEFAULT)

#endif
