#include "precompiled.hpp"

#include "c1/c1_Instruction.hpp"
#include "c1/c1_LinearScan.hpp"
#include "utilities/bitMap.inline.hpp"

//----------------------------------------------------------------------
// Allocation of FPU stack slots (Intel x86 only)
//----------------------------------------------------------------------

void LinearScan::allocate_fpu_stack() {
  // First compute which FPU registers are live at the start of each basic block
  // (To minimize the amount of work we have to do if we have to merge FPU stacks)
  if (ComputeExactFPURegisterUsage) {
    Interval* intervals_in_register, *intervals_in_memory;
    create_unhandled_lists(&intervals_in_register, &intervals_in_memory, is_in_fpu_register, NULL);

    // ignore memory intervals by overwriting intervals_in_memory
    // the dummy interval is needed to enforce the walker to walk until the given id:
    // without it, the walker stops when the unhandled-list is empty -> live information
    // beyond this point would be incorrect.
    Interval* dummy_interval = new Interval(any_reg);
    dummy_interval->add_range(max_jint - 2, max_jint - 1);
    dummy_interval->set_next(Interval::end());
    intervals_in_memory = dummy_interval;

    IntervalWalker iw(this, intervals_in_register, intervals_in_memory);

    const int num_blocks = block_count();
    for (int i = 0; i < num_blocks; i++) {
      BlockBegin* b = block_at(i);

      // register usage is only needed for merging stacks -> compute only
      // when more than one predecessor.
      // the block must not have any spill moves at the beginning (checked by assertions)
      // spill moves would use intervals that are marked as handled and so the usage bit
      // would been set incorrectly

      // NOTE: the check for number_of_preds > 1 is necessary. A block with only one
      //       predecessor may have spill moves at the begin of the block.
      //       If an interval ends at the current instruction id, it is not possible
      //       to decide if the register is live or not at the block begin -> the
      //       register information would be incorrect.
      if (b->number_of_preds() > 1) {
        int id = b->first_lir_instruction_id();
        ResourceBitMap regs(FrameMap::nof_fpu_regs);

        iw.walk_to(id);   // walk after the first instruction (always a label) of the block

        // Only consider FPU values in registers
        Interval* interval = iw.active_first(fixedKind);
        while (interval != Interval::end()) {
          int reg = interval->assigned_reg();

          regs.set_bit(reg - pd_first_fpu_reg);
          interval = interval->next();
        }

        b->set_fpu_register_usage(regs);
      }
    }
  }

  FpuStackAllocator alloc(ir()->compilation(), this);
  _fpu_stack_allocator = &alloc;
  alloc.allocate();
  _fpu_stack_allocator = NULL;
}

FpuStackAllocator::FpuStackAllocator(Compilation* compilation, LinearScan* allocator)
  : _compilation(compilation)
  , _lir(NULL)
  , _pos(-1)
  , _allocator(allocator)
  , _sim(compilation)
  , _temp_sim(compilation)
{ }

void FpuStackAllocator::allocate() {
  int num_blocks = allocator()->block_count();
  for (int i = 0; i < num_blocks; i++) {
    // Set up to process block
    BlockBegin* block = allocator()->block_at(i);
    intArray* fpu_stack_state = block->fpu_stack_state();

    // note: exception handler entries always start with an empty fpu stack
    //       because stack merging would be too complicated

    if (fpu_stack_state != NULL) {
      sim()->read_state(fpu_stack_state);
    } else {
      sim()->clear();
    }

    allocate_block(block);
    CHECK_BAILOUT();
  }
}

void FpuStackAllocator::allocate_block(BlockBegin* block) {
  bool processed_merge = false;
  LIR_OpList* insts = block->lir()->instructions_list();
  set_lir(block->lir());
  set_pos(0);

  // Note: insts->length() may change during loop
  while (pos() < insts->length()) {
    LIR_Op* op = insts->at(pos());
    _debug_information_computed = false;

    LIR_OpBranch* branch = op->as_OpBranch();
    LIR_Op1* op1 = op->as_Op1();
    LIR_Op2* op2 = op->as_Op2();
    LIR_OpCall* opCall = op->as_OpCall();

    if (branch != NULL && branch->block() != NULL) {
      if (!processed_merge) {
        // propagate stack at first branch to a successor
        processed_merge = true;
        bool required_merge = merge_fpu_stack_with_successors(block);
      }
    } else if (op1 != NULL) {
      handle_op1(op1);
    } else if (op2 != NULL) {
      handle_op2(op2);
    } else if (opCall != NULL) {
      handle_opCall(opCall);
    }

    compute_debug_information(op);

    set_pos(1 + pos());
  }

  // Propagate stack when block does not end with branch
  if (!processed_merge) {
    merge_fpu_stack_with_successors(block);
  }
}

void FpuStackAllocator::compute_debug_information(LIR_Op* op) {
  if (!_debug_information_computed && op->id() != -1 && allocator()->has_info(op->id())) {
    visitor.visit(op);

    // exception handling
    if (allocator()->compilation()->has_exception_handlers()) {
      XHandlers* xhandlers = visitor.all_xhandler();
      int n = xhandlers->length();
      for (int k = 0; k < n; k++) {
        allocate_exception_handler(xhandlers->handler_at(k));
      }
    }

    // compute debug information
    int n = visitor.info_count();

    for (int j = 0; j < n; j++) {
      CodeEmitInfo* info = visitor.info_at(j);
      // Compute debug information
      allocator()->compute_debug_info(info, op->id());
    }
  }
  _debug_information_computed = true;
}

void FpuStackAllocator::allocate_exception_handler(XHandler* xhandler) {
  if (!sim()->is_empty()) {
    LIR_List* old_lir = lir();
    int old_pos = pos();
    intArray* old_state = sim()->write_state();

    if (xhandler->entry_code() == NULL) {
      // need entry code to clear FPU stack
      LIR_List* entry_code = new LIR_List(_compilation);
      entry_code->jump(xhandler->entry_block());
      xhandler->set_entry_code(entry_code);
    }

    LIR_OpList* insts = xhandler->entry_code()->instructions_list();
    set_lir(xhandler->entry_code());
    set_pos(0);

    // Note: insts->length() may change during loop
    while (pos() < insts->length()) {
      LIR_Op* op = insts->at(pos());

      switch (op->code()) {
        case lir_move:

          handle_op1((LIR_Op1*)op);
          break;

        case lir_branch:

          // remove all remaining dead registers from FPU stack
          clear_fpu_stack(LIR_OprFact::illegalOpr);
          break;

        default:
          // other operations not allowed in exception entry code
          ShouldNotReachHere();
      }

      set_pos(pos() + 1);
    }

    set_lir(old_lir);
    set_pos(old_pos);
    sim()->read_state(old_state);
  }
}

int FpuStackAllocator::fpu_num(LIR_Opr opr) {
  return opr->is_single_fpu() ? opr->fpu_regnr() : opr->fpu_regnrLo();
}

int FpuStackAllocator::tos_offset(LIR_Opr opr) {
  return sim()->offset_from_tos(fpu_num(opr));
}

LIR_Opr FpuStackAllocator::to_fpu_stack(LIR_Opr opr) {
  int stack_offset = tos_offset(opr);
  if (opr->is_single_fpu()) {
    return LIR_OprFact::single_fpu(stack_offset)->make_fpu_stack_offset();
  } else {
    return LIR_OprFact::double_fpu(stack_offset)->make_fpu_stack_offset();
  }
}

LIR_Opr FpuStackAllocator::to_fpu_stack_top(LIR_Opr opr, bool dont_check_offset) {
  int stack_offset = 0;
  if (opr->is_single_fpu()) {
    return LIR_OprFact::single_fpu(stack_offset)->make_fpu_stack_offset();
  } else {
    return LIR_OprFact::double_fpu(stack_offset)->make_fpu_stack_offset();
  }
}

void FpuStackAllocator::insert_op(LIR_Op* op) {
  lir()->insert_before(pos(), op);
  set_pos(1 + pos());
}

void FpuStackAllocator::insert_exchange(int offset) {
  if (offset > 0) {
    LIR_Op1* fxch_op = new LIR_Op1(lir_fxch, LIR_OprFact::intConst(offset), LIR_OprFact::illegalOpr);
    insert_op(fxch_op);
    sim()->swap(offset);
  }
}

void FpuStackAllocator::insert_exchange(LIR_Opr opr) {
  insert_exchange(tos_offset(opr));
}

void FpuStackAllocator::insert_free(int offset) {
  // move stack slot to the top of stack and then pop it
  insert_exchange(offset);

  LIR_Op* fpop = new LIR_Op0(lir_fpop_raw);
  insert_op(fpop);
  sim()->pop();
}

void FpuStackAllocator::insert_free_if_dead(LIR_Opr opr) {
  if (sim()->contains(fpu_num(opr))) {
    int res_slot = tos_offset(opr);
    insert_free(res_slot);
  }
}

void FpuStackAllocator::insert_free_if_dead(LIR_Opr opr, LIR_Opr ignore) {
  if (fpu_num(opr) != fpu_num(ignore) && sim()->contains(fpu_num(opr))) {
    int res_slot = tos_offset(opr);
    insert_free(res_slot);
  }
}

void FpuStackAllocator::insert_copy(LIR_Opr from, LIR_Opr to) {
  int offset = tos_offset(from);
  LIR_Op1* fld = new LIR_Op1(lir_fld, LIR_OprFact::intConst(offset), LIR_OprFact::illegalOpr);
  insert_op(fld);

  sim()->push(fpu_num(to));
}

void FpuStackAllocator::do_rename(LIR_Opr from, LIR_Opr to) {
  sim()->rename(fpu_num(from), fpu_num(to));
}

void FpuStackAllocator::do_push(LIR_Opr opr) {
  sim()->push(fpu_num(opr));
}

void FpuStackAllocator::pop_if_last_use(LIR_Op* op, LIR_Opr opr) {
  if (opr->is_last_use()) {
    op->set_fpu_pop_count(1);
    sim()->pop();
  }
}

void FpuStackAllocator::pop_always(LIR_Op* op, LIR_Opr opr) {
  op->set_fpu_pop_count(1);
  sim()->pop();
}

void FpuStackAllocator::clear_fpu_stack(LIR_Opr preserve) {
  int result_stack_size = (preserve->is_fpu_register() && !preserve->is_xmm_register() ? 1 : 0);
  while (sim()->stack_size() > result_stack_size) {
    if (result_stack_size == 0 || sim()->get_slot(0) != fpu_num(preserve)) {
      insert_free(0);
    } else {
      // move "preserve" to bottom of stack so that all other stack slots can be popped
      insert_exchange(sim()->stack_size() - 1);
    }
  }
}

void FpuStackAllocator::handle_op1(LIR_Op1* op1) {
  LIR_Opr in  = op1->in_opr();
  LIR_Opr res = op1->result_opr();

  LIR_Opr new_in  = in;  // new operands relative to the actual fpu stack top
  LIR_Opr new_res = res;

  // Note: this switch is processed for all LIR_Op1, regardless if they have FPU-arguments,
  //       so checks for is_float_kind() are necessary inside the cases
  switch (op1->code()) {
    case lir_return: {
      // FPU-Stack must only contain the (optional) fpu return value.
      // All remaining dead values are popped from the stack
      // If the input operand is a fpu-register, it is exchanged to the bottom of the stack

      clear_fpu_stack(in);
      if (in->is_fpu_register() && !in->is_xmm_register()) {
        new_in = to_fpu_stack_top(in);
      }

      break;
    }

    case lir_move: {
      if (in->is_fpu_register() && !in->is_xmm_register()) {
        if (res->is_xmm_register()) {
          // move from fpu register to xmm register (necessary for operations that
          // are not available in the SSE instruction set)
          insert_exchange(in);
          new_in = to_fpu_stack_top(in);
          pop_always(op1, in);
        } else if (res->is_fpu_register() && !res->is_xmm_register()) {
          // move from fpu-register to fpu-register:
          // * input and result register equal:
          //   nothing to do
          // * input register is last use:
          //   rename the input register to result register -> input register
          //   not present on fpu-stack afterwards
          // * input register not last use:
          //   duplicate input register to result register to preserve input
          //
          // Note: The LIR-Assembler does not produce any code for fpu register moves,
          //       so input and result stack index must be equal

          if (fpu_num(in) == fpu_num(res)) {
            // nothing to do
          } else if (in->is_last_use()) {
            insert_free_if_dead(res);//, in);
            do_rename(in, res);
          } else {
            insert_free_if_dead(res);
            insert_copy(in, res);
          }
          new_in = to_fpu_stack(res);
          new_res = new_in;
        } else {
          // move from fpu-register to memory
          // input operand must be on top of stack

          insert_exchange(in);

          // create debug information here because afterwards the register may have been popped
          compute_debug_information(op1);

          new_in = to_fpu_stack_top(in);
          pop_if_last_use(op1, in);
        }
      } else if (res->is_fpu_register() && !res->is_xmm_register()) {
        // move from memory/constant to fpu register
        // result is pushed on the stack

        insert_free_if_dead(res);

        // create debug information before register is pushed
        compute_debug_information(op1);

        do_push(res);
        new_res = to_fpu_stack_top(res);
      }
      break;
    }

    case lir_neg: {
      if (in->is_fpu_register() && !in->is_xmm_register()) {
        insert_free_if_dead(res, in);
        insert_exchange(in);
        new_in = to_fpu_stack_top(in);

        do_rename(in, res);
        new_res = to_fpu_stack_top(res);
      }
      break;
    }

    case lir_convert: {
      Bytecodes::Code bc = op1->as_OpConvert()->bytecode();
      switch (bc) {
        case Bytecodes::_d2f:
        case Bytecodes::_f2d:

          if (!in->is_xmm_register() && !res->is_xmm_register()) {
            // this is quite the same as a move from fpu-register to fpu-register
            // Note: input and result operands must have different types
            if (fpu_num(in) == fpu_num(res)) {
              // nothing to do
              new_in = to_fpu_stack(in);
            } else if (in->is_last_use()) {
              insert_free_if_dead(res);//, in);
              new_in = to_fpu_stack(in);
              do_rename(in, res);
            } else {
              insert_free_if_dead(res);
              insert_copy(in, res);
              new_in = to_fpu_stack_top(in, true);
            }
            new_res = to_fpu_stack(res);
          }

          break;

        case Bytecodes::_i2f:
        case Bytecodes::_l2f:
        case Bytecodes::_i2d:
        case Bytecodes::_l2d:
          if (!res->is_xmm_register()) {
            insert_free_if_dead(res);
            do_push(res);
            new_res = to_fpu_stack_top(res);
          }
          break;

        case Bytecodes::_f2i:
        case Bytecodes::_d2i:
          if (!in->is_xmm_register()) {
            insert_exchange(in);
            new_in = to_fpu_stack_top(in);

            // TODO: update registes of stub
          }
          break;

        case Bytecodes::_f2l:
        case Bytecodes::_d2l:
          if (!in->is_xmm_register()) {
            insert_exchange(in);
            new_in = to_fpu_stack_top(in);
            pop_always(op1, in);
          }
          break;

        case Bytecodes::_i2l:
        case Bytecodes::_l2i:
        case Bytecodes::_i2b:
        case Bytecodes::_i2c:
        case Bytecodes::_i2s:
          // no fpu operands
          break;

        default:
          ShouldNotReachHere();
      }
      break;
    }

    case lir_roundfp: {
      insert_exchange(in);
      new_in = to_fpu_stack_top(in);
      pop_if_last_use(op1, in);
      break;
    }

    default:
      break;
  }

  op1->set_in_opr(new_in);
  op1->set_result_opr(new_res);
}

void FpuStackAllocator::handle_op2(LIR_Op2* op2) {
  LIR_Opr left  = op2->in_opr1();
  if (!left->is_float_kind()) {
    return;
  }
  if (left->is_xmm_register()) {
    return;
  }

  LIR_Opr right = op2->in_opr2();
  LIR_Opr res   = op2->result_opr();
  LIR_Opr new_left  = left;  // new operands relative to the actual fpu stack top
  LIR_Opr new_right = right;
  LIR_Opr new_res   = res;

  switch (op2->code()) {
    case lir_cmp:
    case lir_cmp_fd2i:
    case lir_ucmp_fd2i:
    case lir_assert: {
      // the left-hand side must be on top of stack.
      // the right-hand side is never popped, even if is_last_use is set
      insert_exchange(left);
      new_left = to_fpu_stack_top(left);
      new_right = to_fpu_stack(right);
      pop_if_last_use(op2, left);
      break;
    }

    case lir_mul_strictfp:
    case lir_div_strictfp: {
      insert_free_if_dead(op2->tmp1_opr());
      // fall-through: continue with the normal handling of lir_mul and lir_div
    }
    case lir_add:
    case lir_sub:
    case lir_mul:
    case lir_div: {
      // either the left-hand or the right-hand side must be on top of stack
      // (if right is not a register, left must be on top)
      if (!right->is_fpu_register()) {
        insert_exchange(left);
        new_left = to_fpu_stack_top(left);
      } else {
        // no exchange necessary if right is alredy on top of stack
        if (tos_offset(right) == 0) {
          new_left = to_fpu_stack(left);
          new_right = to_fpu_stack_top(right);
        } else {
          insert_exchange(left);
          new_left = to_fpu_stack_top(left);
          new_right = to_fpu_stack(right);
        }

        if (right->is_last_use()) {
          op2->set_fpu_pop_count(1);

          if (tos_offset(right) == 0) {
            sim()->pop();
          } else {
            // if left is on top of stack, the result is placed in the stack
            // slot of right, so a renaming from right to res is necessary
            sim()->pop();
            do_rename(right, res);
          }
        }
      }
      new_res = to_fpu_stack(res);

      break;
    }

    case lir_rem: {
      // Must bring both operands to top of stack with following operand ordering:
      // * fpu stack before rem: ... right left
      // * fpu stack after rem:  ... left
      if (tos_offset(right) != 1) {
        insert_exchange(right);
        insert_exchange(1);
      }
      insert_exchange(left);

      new_left = to_fpu_stack_top(left);
      new_right = to_fpu_stack(right);

      op2->set_fpu_pop_count(1);
      sim()->pop();
      do_rename(right, res);

      new_res = to_fpu_stack_top(res);
      break;
    }

    case lir_abs:
    case lir_sqrt: {
      insert_free_if_dead(res, left);
      insert_exchange(left);
      do_rename(left, res);

      new_left = to_fpu_stack_top(res);
      new_res = new_left;

      op2->set_fpu_stack_size(sim()->stack_size());
      break;
    }

    default: {
      ShouldNotReachHere();
    }
  }

  op2->set_in_opr1(new_left);
  op2->set_in_opr2(new_right);
  op2->set_result_opr(new_res);
}

void FpuStackAllocator::handle_opCall(LIR_OpCall* opCall) {
  LIR_Opr res = opCall->result_opr();

  // clear fpu-stack before call
  // it may contain dead values that could not have been remved by previous operations
  clear_fpu_stack(LIR_OprFact::illegalOpr);

  // compute debug information before (possible) fpu result is pushed
  compute_debug_information(opCall);

  if (res->is_fpu_register() && !res->is_xmm_register()) {
    do_push(res);
    opCall->set_result_opr(to_fpu_stack_top(res));
  }
}

void FpuStackAllocator::merge_insert_add(LIR_List* instrs, FpuStackSim* cur_sim, int reg) {
  LIR_Op1* move = new LIR_Op1(lir_move, LIR_OprFact::doubleConst(0), LIR_OprFact::double_fpu(reg)->make_fpu_stack_offset());

  instrs->instructions_list()->push(move);

  cur_sim->push(reg);
  move->set_result_opr(to_fpu_stack(move->result_opr()));
}

void FpuStackAllocator::merge_insert_xchg(LIR_List* instrs, FpuStackSim* cur_sim, int slot) {
  LIR_Op1* fxch = new LIR_Op1(lir_fxch, LIR_OprFact::intConst(slot));
  instrs->instructions_list()->push(fxch);
  cur_sim->swap(slot);
}

void FpuStackAllocator::merge_insert_pop(LIR_List* instrs, FpuStackSim* cur_sim) {
  int reg = cur_sim->get_slot(0);

  LIR_Op* fpop = new LIR_Op0(lir_fpop_raw);
  instrs->instructions_list()->push(fpop);
  cur_sim->pop(reg);
}

bool FpuStackAllocator::merge_rename(FpuStackSim* cur_sim, FpuStackSim* sux_sim, int start_slot, int change_slot) {
  int reg = cur_sim->get_slot(change_slot);

  for (int slot = start_slot; slot >= 0; slot--) {
    int new_reg = sux_sim->get_slot(slot);

    if (!cur_sim->contains(new_reg)) {
      cur_sim->set_slot(change_slot, new_reg);

      return true;
    }
  }
  return false;
}

void FpuStackAllocator::merge_fpu_stack(LIR_List* instrs, FpuStackSim* cur_sim, FpuStackSim* sux_sim) {
  // size difference between cur and sux that must be resolved by adding or removing values form the stack
  int size_diff = cur_sim->stack_size() - sux_sim->stack_size();

  if (!ComputeExactFPURegisterUsage) {
    // add slots that are currently free, but used in successor
    // When the exact FPU register usage is computed, the stack does
    // not contain dead values at merging -> no values must be added

    int sux_slot = sux_sim->stack_size() - 1;
    while (size_diff < 0) {
      int reg = sux_sim->get_slot(sux_slot);
      if (!cur_sim->contains(reg)) {
        merge_insert_add(instrs, cur_sim, reg);
        size_diff++;

        if (sux_slot + size_diff != 0) {
          merge_insert_xchg(instrs, cur_sim, sux_slot + size_diff);
        }
      }
     sux_slot--;
    }
  }

  // stack merge algorithm:
  // 1) as long as the current stack top is not in the right location (that meens
  //    it should not be on the stack top), exchange it into the right location
  // 2) if the stack top is right, but the remaining stack is not ordered correctly,
  //    the stack top is exchanged away to get another value on top ->
  //    now step 1) can be continued
  // the stack can also contain unused items -> these items are removed from stack

  int finished_slot = sux_sim->stack_size() - 1;
  while (finished_slot >= 0 || size_diff > 0) {
    while (size_diff > 0 || (cur_sim->stack_size() > 0 && cur_sim->get_slot(0) != sux_sim->get_slot(0))) {
      int reg = cur_sim->get_slot(0);
      if (sux_sim->contains(reg)) {
        int sux_slot = sux_sim->offset_from_tos(reg);
        merge_insert_xchg(instrs, cur_sim, sux_slot + size_diff);
      } else if (!merge_rename(cur_sim, sux_sim, finished_slot, 0)) {
        merge_insert_pop(instrs, cur_sim);
        size_diff--;
      }
    }

    while (finished_slot >= 0 && cur_sim->get_slot(finished_slot) == sux_sim->get_slot(finished_slot)) {
      finished_slot--;
    }

    if (finished_slot >= 0) {
      int reg = cur_sim->get_slot(finished_slot);

      if (sux_sim->contains(reg) || !merge_rename(cur_sim, sux_sim, finished_slot, finished_slot)) {
        merge_insert_xchg(instrs, cur_sim, finished_slot);
      }
    }
  }
}

void FpuStackAllocator::merge_cleanup_fpu_stack(LIR_List* instrs, FpuStackSim* cur_sim, BitMap& live_fpu_regs) {
  int slot = 0;
  while (slot < cur_sim->stack_size()) {
    int reg = cur_sim->get_slot(slot);
    if (!live_fpu_regs.at(reg)) {
      if (slot != 0) {
        merge_insert_xchg(instrs, cur_sim, slot);
      }
      merge_insert_pop(instrs, cur_sim);
    } else {
      slot++;
    }
  }
}

bool FpuStackAllocator::merge_fpu_stack_with_successors(BlockBegin* block) {
  bool changed = false;
  int number_of_sux = block->number_of_sux();

  if (number_of_sux == 1 && block->sux_at(0)->number_of_preds() > 1) {
    // The successor has at least two incoming edges, so a stack merge will be necessary
    // If this block is the first predecessor, cleanup the current stack and propagate it
    // If this block is not the first predecessor, a stack merge will be necessary

    BlockBegin* sux = block->sux_at(0);
    intArray* state = sux->fpu_stack_state();
    LIR_List* instrs = new LIR_List(_compilation);

    if (state != NULL) {
      // Merge with a successors that already has a FPU stack state
      // the block must only have one successor because critical edges must been split
      FpuStackSim* cur_sim = sim();
      FpuStackSim* sux_sim = temp_sim();
      sux_sim->read_state(state);

      merge_fpu_stack(instrs, cur_sim, sux_sim);
    } else {
      // propagate current FPU stack state to successor without state
      // clean up stack first so that there are no dead values on the stack
      if (ComputeExactFPURegisterUsage) {
        FpuStackSim* cur_sim = sim();
        ResourceBitMap live_fpu_regs = block->sux_at(0)->fpu_register_usage();

        merge_cleanup_fpu_stack(instrs, cur_sim, live_fpu_regs);
      }

      intArray* state = sim()->write_state();
      if (TraceFPUStack) {
        tty->print_cr("Setting FPU stack state of B%d (merge path)", sux->block_id());
        sim()->print(); tty->cr();
      }
      sux->set_fpu_stack_state(state);
    }

    if (instrs->instructions_list()->length() > 0) {
      lir()->insert_before(pos(), instrs);
      set_pos(instrs->instructions_list()->length() + pos());
      changed = true;
    }
  } else {
    // Propagate unmodified Stack to successors where a stack merge is not necessary
    intArray* state = sim()->write_state();
    for (int i = 0; i < number_of_sux; i++) {
      BlockBegin* sux = block->sux_at(i);

      sux->set_fpu_stack_state(state);
    }
  }

  return changed;
}
