#ifndef CPU_AARCH64_VM_C1_DEFS_AARCH64_HPP
#define CPU_AARCH64_VM_C1_DEFS_AARCH64_HPP

// native word offsets from memory address (little endian)
enum {
  pd_lo_word_offset_in_bytes = 0,
  pd_hi_word_offset_in_bytes = BytesPerWord
};

// explicit rounding operations are required to implement the strictFP mode
enum {
  pd_strict_fp_requires_explicit_rounding = false
};

// FIXME: There are no callee-saved

// registers
enum {
  pd_nof_cpu_regs_frame_map = RegisterImpl::number_of_registers,       // number of registers used during code emission
  pd_nof_fpu_regs_frame_map = FloatRegisterImpl::number_of_registers,  // number of registers used during code emission

  pd_nof_caller_save_cpu_regs_frame_map = 19 - 2,  // number of registers killed by calls
  pd_nof_caller_save_fpu_regs_frame_map = 32,  // number of registers killed by calls

  pd_first_callee_saved_reg = 19 - 2,
  pd_last_callee_saved_reg = 26 - 2,

  pd_last_allocatable_cpu_reg = 16,

  pd_nof_cpu_regs_reg_alloc = pd_last_allocatable_cpu_reg + 1,  // number of registers that are visible to register allocator
  pd_nof_fpu_regs_reg_alloc = 8,  // number of registers that are visible to register allocator

  pd_nof_cpu_regs_linearscan = 32, // number of registers visible to linear scan
  pd_nof_fpu_regs_linearscan = pd_nof_fpu_regs_frame_map, // number of registers visible to linear scan
  pd_nof_xmm_regs_linearscan = 0, // like sparc we don't have any of these
  pd_first_cpu_reg = 0,
  pd_last_cpu_reg = 16,
  pd_first_byte_reg = 0,
  pd_last_byte_reg = 16,
  pd_first_fpu_reg = pd_nof_cpu_regs_frame_map,
  pd_last_fpu_reg =  pd_first_fpu_reg + 31,

  pd_first_callee_saved_fpu_reg = 8 + pd_first_fpu_reg,
  pd_last_callee_saved_fpu_reg = 15 + pd_first_fpu_reg,
};

// Encoding of float value in debug info.  This is true on x86 where
// floats are extended to doubles when stored in the stack, false for
// AArch64 where floats and doubles are stored in their native form.
enum {
  pd_float_saved_as_double = false
};

#endif
