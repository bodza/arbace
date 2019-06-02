#ifndef CPU_AARCH64_VM_TEMPLATETABLE_AARCH64_64_HPP
#define CPU_AARCH64_VM_TEMPLATETABLE_AARCH64_64_HPP

static void prepare_invoke(int byte_no,
                             Register method,         // linked method (or i-klass)
                             Register index = noreg,  // itable index, MethodType, etc.
                             Register recv  = noreg,  // if caller wants to see it
                             Register flags = noreg   // if caller wants to test it
                             );
  static void invokevirtual_helper(Register index, Register recv, Register flags);

  // Helpers
  static void index_check(Register array, Register index);
  static void index_check_without_pop(Register array, Register index);

#endif
