#ifndef SHARE_VM_C1_C1_OPTIMIZER_HPP
#define SHARE_VM_C1_C1_OPTIMIZER_HPP

#include "c1/c1_IR.hpp"
#include "c1/c1_Instruction.hpp"

class Optimizer {
 private:
  IR* _ir;

 public:
  Optimizer(IR* ir);
  IR* ir() const { return _ir; }

  // optimizations
  void eliminate_conditional_expressions();
  void eliminate_blocks();
  void eliminate_null_checks();
};

#endif
