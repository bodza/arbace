#ifndef CPU_AARCH64_VM_C1_LINEARSCAN_HPP
#define CPU_AARCH64_VM_C1_LINEARSCAN_HPP

inline bool LinearScan::is_processed_reg_num(int reg_num) {
  return reg_num <= FrameMap::last_cpu_reg() || reg_num >= pd_nof_cpu_regs_frame_map;
}

inline int LinearScan::num_physical_regs(BasicType type) {
  return 1;
}

inline bool LinearScan::requires_adjacent_regs(BasicType type) {
  return false;
}

inline bool LinearScan::is_caller_save(int assigned_reg) {
  assert(assigned_reg >= 0 && assigned_reg < nof_regs, "should call this only for registers");
  if (assigned_reg < pd_first_callee_saved_reg)
    return true;
  if (assigned_reg > pd_last_callee_saved_reg && assigned_reg < pd_first_callee_saved_fpu_reg)
    return true;
  if (assigned_reg > pd_last_callee_saved_fpu_reg && assigned_reg < pd_last_fpu_reg)
    return true;
  return false;
}

inline void LinearScan::pd_add_temps(LIR_Op* op) {
  // FIXME ??
}

// Implementation of LinearScanWalker

inline bool LinearScanWalker::pd_init_regs_for_alloc(Interval* cur) {
  if (allocator()->gen()->is_vreg_flag_set(cur->reg_num(), LIRGenerator::callee_saved)) {
    assert(cur->type() != T_FLOAT && cur->type() != T_DOUBLE, "cpu regs only");
    _first_reg = pd_first_callee_saved_reg;
    _last_reg = pd_last_callee_saved_reg;
    return true;
  } else if (cur->type() == T_INT || cur->type() == T_LONG || cur->type() == T_OBJECT || cur->type() == T_ADDRESS || cur->type() == T_METADATA) {
    _first_reg = pd_first_cpu_reg;
    _last_reg = pd_last_allocatable_cpu_reg;
    return true;
  }
  return false;
}

#endif
