#ifndef CPU_X86_VM_C1_FRAMEMAP_X86_HPP
#define CPU_X86_VM_C1_FRAMEMAP_X86_HPP

//  On i486 the frame looks as follows:
//
//  +-----------------------------+---------+----------------------------------------+----------------+-----------
//  | size_arguments-nof_reg_args | 2 words | size_locals-size_arguments+numreg_args | _size_monitors | spilling .
//  +-----------------------------+---------+----------------------------------------+----------------+-----------
//
//  The FPU registers are mapped with their offset from TOS; therefore the
//  status of FPU stack must be updated during code emission.

 public:
  static const int pd_c_runtime_reserved_arg_size;

  enum {
    nof_xmm_regs = pd_nof_xmm_regs_frame_map,
    nof_caller_save_xmm_regs = pd_nof_caller_save_xmm_regs_frame_map,
    first_available_sp_in_frame = 0,
    frame_pad_in_bytes = 16,
    nof_reg_args = 6
  };

 private:
  static LIR_Opr      _caller_save_xmm_regs [nof_caller_save_xmm_regs];

  static XMMRegister _xmm_regs[nof_xmm_regs];

 public:
  static LIR_Opr receiver_opr;

  static LIR_Opr rsi_opr;
  static LIR_Opr rdi_opr;
  static LIR_Opr rbx_opr;
  static LIR_Opr rax_opr;
  static LIR_Opr rdx_opr;
  static LIR_Opr rcx_opr;
  static LIR_Opr rsp_opr;
  static LIR_Opr rbp_opr;

  static LIR_Opr rsi_oop_opr;
  static LIR_Opr rdi_oop_opr;
  static LIR_Opr rbx_oop_opr;
  static LIR_Opr rax_oop_opr;
  static LIR_Opr rdx_oop_opr;
  static LIR_Opr rcx_oop_opr;

  static LIR_Opr rsi_metadata_opr;
  static LIR_Opr rdi_metadata_opr;
  static LIR_Opr rbx_metadata_opr;
  static LIR_Opr rax_metadata_opr;
  static LIR_Opr rdx_metadata_opr;
  static LIR_Opr rcx_metadata_opr;

  static LIR_Opr  r8_opr;
  static LIR_Opr  r9_opr;
  static LIR_Opr r10_opr;
  static LIR_Opr r11_opr;
  static LIR_Opr r12_opr;
  static LIR_Opr r13_opr;
  static LIR_Opr r14_opr;
  static LIR_Opr r15_opr;

  static LIR_Opr  r8_oop_opr;
  static LIR_Opr  r9_oop_opr;

  static LIR_Opr r11_oop_opr;
  static LIR_Opr r12_oop_opr;
  static LIR_Opr r13_oop_opr;
  static LIR_Opr r14_oop_opr;

  static LIR_Opr  r8_metadata_opr;
  static LIR_Opr  r9_metadata_opr;

  static LIR_Opr r11_metadata_opr;
  static LIR_Opr r12_metadata_opr;
  static LIR_Opr r13_metadata_opr;
  static LIR_Opr r14_metadata_opr;

  static LIR_Opr long0_opr;
  static LIR_Opr long1_opr;
  static LIR_Opr fpu0_float_opr;
  static LIR_Opr fpu0_double_opr;
  static LIR_Opr xmm0_float_opr;
  static LIR_Opr xmm0_double_opr;

  static LIR_Opr as_long_opr(Register r) {
    return LIR_OprFact::double_cpu(cpu_reg2rnr(r), cpu_reg2rnr(r));
  }
  static LIR_Opr as_pointer_opr(Register r) {
    return LIR_OprFact::double_cpu(cpu_reg2rnr(r), cpu_reg2rnr(r));
  }

  // VMReg name for spilled physical FPU stack slot n
  static VMReg fpu_regname (int n);

  static XMMRegister nr2xmmreg(int rnr);

  static bool is_caller_save_register (LIR_Opr opr) { return true; }
  static bool is_caller_save_register (Register r) { return true; }

  static LIR_Opr caller_save_xmm_reg_at(int i) {
    return _caller_save_xmm_regs[i];
  }

  static int adjust_reg_range(int range) {
    // Reduce the number of available regs (to free r12) in case of compressed oops
    if (UseCompressedOops || UseCompressedClassPointers) return range - 1;
    return range;
  }

  static int get_num_caller_save_xmms(void) {
    int num_caller_save_xmm_regs = nof_caller_save_xmm_regs;
    if (UseAVX < 3) {
      num_caller_save_xmm_regs = num_caller_save_xmm_regs / 2;
    }
    return num_caller_save_xmm_regs;
  }

  static int nof_caller_save_cpu_regs() { return adjust_reg_range(pd_nof_caller_save_cpu_regs_frame_map); }
  static int last_cpu_reg()             { return adjust_reg_range(pd_last_cpu_reg); }
  static int last_byte_reg()            { return adjust_reg_range(pd_last_byte_reg); }

#endif
