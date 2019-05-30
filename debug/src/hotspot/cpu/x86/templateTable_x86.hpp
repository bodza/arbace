#ifndef CPU_X86_VM_TEMPLATETABLE_X86_HPP
#define CPU_X86_VM_TEMPLATETABLE_X86_HPP

  static void prepare_invoke(int byte_no,
                             Register method,         // linked method (or i-klass)
                             Register index = noreg,  // itable index, MethodType, etc.
                             Register recv  = noreg,  // if caller wants to see it
                             Register flags = noreg   // if caller wants to test it
                             );
  static void invokevirtual_helper(Register index, Register recv, Register flags);
  static void volatile_barrier(Assembler::Membar_mask_bits order_constraint);

  // Helpers
  static void index_check(Register array, Register index);
  static void index_check_without_pop(Register array, Register index);

#endif
