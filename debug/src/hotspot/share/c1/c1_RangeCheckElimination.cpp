#include "precompiled.hpp"

#include "c1/c1_ValueStack.hpp"
#include "c1/c1_RangeCheckElimination.hpp"
#include "c1/c1_IR.hpp"
#include "c1/c1_Canonicalizer.hpp"
#include "c1/c1_ValueMap.hpp"
#include "ci/ciMethodData.hpp"

// Entry point for the optimization
void RangeCheckElimination::eliminate(IR *ir) {
  bool do_elimination = ir->compilation()->has_access_indexed();
  if (do_elimination) {
    RangeCheckEliminator rce(ir);
  }
}

// Constructor
RangeCheckEliminator::RangeCheckEliminator(IR *ir) :
  _bounds(Instruction::number_of_instructions(), Instruction::number_of_instructions(), NULL),
  _access_indexed_info(Instruction::number_of_instructions(), Instruction::number_of_instructions(), NULL)
{
  _visitor.set_range_check_eliminator(this);
  _ir = ir;
  _number_of_instructions = Instruction::number_of_instructions();

  // Set process block flags
  // Optimization so a blocks is only processed if it contains an access indexed instruction or if
  // one of its children in the dominator tree contains an access indexed instruction.
  set_process_block_flags(ir->start());

  // Pass over instructions in the dominator tree
  calc_bounds(ir->start(), NULL);
}

// Instruction specific work for some instructions
// Constant
void RangeCheckEliminator::Visitor::do_Constant(Constant *c) {
  IntConstant *ic = c->type()->as_IntConstant();
  if (ic != NULL) {
    int value = ic->value();
    _bound = new Bound(value, NULL, value, NULL);
  }
}

// LogicOp
void RangeCheckEliminator::Visitor::do_LogicOp(LogicOp *lo) {
  if (lo->type()->as_IntType() && lo->op() == Bytecodes::_iand && (lo->x()->as_Constant() || lo->y()->as_Constant())) {
    int constant = 0;
    Constant *c = lo->x()->as_Constant();
    if (c != NULL) {
      constant = c->type()->as_IntConstant()->value();
    } else {
      constant = lo->y()->as_Constant()->type()->as_IntConstant()->value();
    }
    if (constant >= 0) {
      _bound = new Bound(0, NULL, constant, NULL);
    }
  }
}

// Phi
void RangeCheckEliminator::Visitor::do_Phi(Phi *phi) {
  if (!phi->type()->as_IntType() && !phi->type()->as_ObjectType()) return;

  BlockBegin *block = phi->block();
  int op_count = phi->operand_count();
  bool has_upper = true;
  bool has_lower = true;
  Bound *bound = NULL;

  // TODO: support more difficult phis
  for (int i = 0; i<op_count; i++) {
    Value v = phi->operand_at(i);

    if (v == phi) continue;

    // Check if instruction is connected with phi itself
    Op2 *op2 = v->as_Op2();
    if (op2 != NULL) {
      Value x = op2->x();
      Value y = op2->y();
      if ((x == phi || y == phi)) {
        Value other = x;
        if (other == phi) {
          other = y;
        }
        ArithmeticOp *ao = v->as_ArithmeticOp();
        if (ao != NULL && ao->op() == Bytecodes::_iadd) {
          if (ao->type()->as_IntType()) {
            Constant *c = other->as_Constant();
            if (c != NULL) {
              int value = c->type()->as_IntConstant()->value();
              if (value == 1) {
                has_upper = false;
              } else if (value > 1) {
                // Overflow not guaranteed
                has_upper = false;
                has_lower = false;
              } else if (value < 0) {
                has_lower = false;
              }
              continue;
            }
          }
        }
      }
    }

    // No connection -> new bound
    Bound *v_bound = _rce->get_bound(v);
    Bound *cur_bound;
    int cur_constant = 0;
    Value cur_value = v;

    if (v->type()->as_IntConstant()) {
      cur_constant = v->type()->as_IntConstant()->value();
      cur_value = NULL;
    }
    if (!v_bound->has_upper() || !v_bound->has_lower()) {
      cur_bound = new Bound(cur_constant, cur_value, cur_constant, cur_value);
    } else {
      cur_bound = v_bound;
    }
    if (cur_bound) {
      if (!bound) {
        bound = cur_bound->copy();
      } else {
        bound->or_op(cur_bound);
      }
    } else {
      // No bound!
      bound = NULL;
      break;
    }
  }

  if (bound) {
    if (!has_upper) {
      bound->remove_upper();
    }
    if (!has_lower) {
      bound->remove_lower();
    }
    _bound = bound;
  } else {
    _bound = new Bound();
  }
}

// ArithmeticOp
void RangeCheckEliminator::Visitor::do_ArithmeticOp(ArithmeticOp *ao) {
  Value x = ao->x();
  Value y = ao->y();

  if (ao->op() == Bytecodes::_irem) {
    Bound* x_bound = _rce->get_bound(x);
    Bound* y_bound = _rce->get_bound(y);
    if (x_bound->lower() >= 0 && x_bound->lower_instr() == NULL && y->as_ArrayLength() != NULL) {
      _bound = new Bound(0, NULL, -1, y);
    } else {
      _bound = new Bound();
    }
  } else if (!x->as_Constant() || !y->as_Constant()) {
    if (((x->as_Constant() || y->as_Constant()) && (ao->op() == Bytecodes::_iadd)) || (y->as_Constant() && ao->op() == Bytecodes::_isub)) {
      if (y->as_Constant()) {
        Value tmp = x;
        x = y;
        y = tmp;
      }

      // Constant now in x
      int const_value = x->as_Constant()->type()->as_IntConstant()->value();
      if (ao->op() == Bytecodes::_iadd || const_value != min_jint) {
        if (ao->op() == Bytecodes::_isub) {
          const_value = -const_value;
        }

        Bound * bound = _rce->get_bound(y);
        if (bound->has_upper() && bound->has_lower()) {
          int new_lower = bound->lower() + const_value;
          jlong new_lowerl = ((jlong)bound->lower()) + const_value;
          int new_upper = bound->upper() + const_value;
          jlong new_upperl = ((jlong)bound->upper()) + const_value;

          if (((jlong)new_lower) == new_lowerl && ((jlong)new_upper == new_upperl)) {
            Bound *newBound = new Bound(new_lower, bound->lower_instr(), new_upper, bound->upper_instr());
            _bound = newBound;
          } else {
            // overflow
            _bound = new Bound();
          }
        } else {
          _bound = new Bound();
        }
      } else {
        _bound = new Bound();
      }
    } else {
      Bound *bound = _rce->get_bound(x);
      if (ao->op() == Bytecodes::_isub) {
        if (bound->lower_instr() == y) {
          _bound = new Bound(Instruction::geq, NULL, bound->lower());
        } else {
          _bound = new Bound();
        }
      } else {
        _bound = new Bound();
      }
    }
  }
}

// IfOp
void RangeCheckEliminator::Visitor::do_IfOp(IfOp *ifOp)
{
  if (ifOp->tval()->type()->as_IntConstant() && ifOp->fval()->type()->as_IntConstant()) {
    int min = ifOp->tval()->type()->as_IntConstant()->value();
    int max = ifOp->fval()->type()->as_IntConstant()->value();
    if (min > max) {
      // min ^= max ^= min ^= max;
      int tmp = min;
      min = max;
      max = tmp;
    }
    _bound = new Bound(min, NULL, max, NULL);
  }
}

// Get bound. Returns the current bound on Value v. Normally this is the topmost element on the bound stack.
RangeCheckEliminator::Bound *RangeCheckEliminator::get_bound(Value v) {
  // Wrong type or NULL -> No bound
  if (!v || (!v->type()->as_IntType() && !v->type()->as_ObjectType())) return NULL;

  if (!_bounds.at(v->id())) {
    // First (default) bound is calculated
    // Create BoundStack
    _bounds.at_put(v->id(), new BoundStack());
    _visitor.clear_bound();
    Value visit_value = v;
    visit_value->visit(&_visitor);
    Bound *bound = _visitor.bound();
    if (bound) {
      _bounds.at(v->id())->push(bound);
    }
    if (_bounds.at(v->id())->length() == 0) {
      _bounds.at(v->id())->push(new Bound());
    }
  } else if (_bounds.at(v->id())->length() == 0) {
    // To avoid endless loops, bound is currently in calculation -> nothing known about it
    return new Bound();
  }

  // Return bound
  return _bounds.at(v->id())->top();
}

// Update bound
void RangeCheckEliminator::update_bound(IntegerStack &pushed, Value v, Instruction::Condition cond, Value value, int constant) {
  if (cond == Instruction::gtr) {
    cond = Instruction::geq;
    constant++;
  } else if (cond == Instruction::lss) {
    cond = Instruction::leq;
    constant--;
  }
  Bound *bound = new Bound(cond, value, constant);
  update_bound(pushed, v, bound);
}

// Checks for loop invariance. Returns true if the instruction is outside of the loop which is identified by loop_header.
bool RangeCheckEliminator::loop_invariant(BlockBegin *loop_header, Instruction *instruction) {
  if (!instruction) return true;
  return instruction->dominator_depth() < loop_header->dominator_depth();
}

// Update bound. Pushes a new bound onto the stack. Tries to do a conjunction with the current bound.
void RangeCheckEliminator::update_bound(IntegerStack &pushed, Value v, Bound *bound) {
  if (v->as_Constant()) {
    // No bound update for constants
    return;
  }
  if (!_bounds.at(v->id())) {
    get_bound(v);
  }
  Bound *top = NULL;
  if (_bounds.at(v->id())->length() > 0) {
    top = _bounds.at(v->id())->top();
  }
  if (top) {
    bound->and_op(top);
  }
  _bounds.at(v->id())->push(bound);
  pushed.append(v->id());
}

// Add instruction + idx for in block motion
void RangeCheckEliminator::add_access_indexed_info(InstructionList &indices, int idx, Value instruction, AccessIndexed *ai) {
  int id = instruction->id();
  AccessIndexedInfo *aii = _access_indexed_info.at(id);
  if (aii == NULL) {
    aii = new AccessIndexedInfo();
    _access_indexed_info.at_put(id, aii);
    indices.append(instruction);
    aii->_min = idx;
    aii->_max = idx;
    aii->_list = new AccessIndexedList();
  } else if (idx >= aii->_min && idx <= aii->_max) {
    remove_range_check(ai);
    return;
  }
  aii->_min = MIN2(aii->_min, idx);
  aii->_max = MAX2(aii->_max, idx);
  aii->_list->append(ai);
}

// In block motion. Tries to reorder checks in order to reduce some of them.
// Example:
// a[i] = 0;
// a[i+2] = 0;
// a[i+1] = 0;
// In this example the check for a[i+1] would be considered as unnecessary during the first iteration.
// After this i is only checked once for i >= 0 and i+2 < a.length before the first array access. If this
// check fails, deoptimization is called.
void RangeCheckEliminator::in_block_motion(BlockBegin *block, AccessIndexedList &accessIndexed, InstructionList &arrays) {
  InstructionList indices;

  // Now iterate over all arrays
  for (int i = 0; i<arrays.length(); i++) {
    int max_constant = -1;
    AccessIndexedList list_constant;
    Value array = arrays.at(i);

    // For all AccessIndexed-instructions in this block concerning the current array.
    for (int j = 0; j<accessIndexed.length(); j++) {
      AccessIndexed *ai = accessIndexed.at(j);
      if (ai->array() != array || !ai->check_flag(Instruction::NeedsRangeCheckFlag)) continue;

      Value index = ai->index();
      Constant *c = index->as_Constant();
      if (c != NULL) {
        int constant_value = c->type()->as_IntConstant()->value();
        if (constant_value >= 0) {
          if (constant_value <= max_constant) {
            // No range check needed for this
            remove_range_check(ai);
          } else {
            max_constant = constant_value;
            list_constant.append(ai);
          }
        }
      } else {
        int last_integer = 0;
        Instruction *last_instruction = index;
        int base = 0;
        ArithmeticOp *ao = index->as_ArithmeticOp();

        while (ao != NULL && (ao->x()->as_Constant() || ao->y()->as_Constant()) && (ao->op() == Bytecodes::_iadd || ao->op() == Bytecodes::_isub)) {
          c = ao->y()->as_Constant();
          Instruction *other = ao->x();
          if (!c && ao->op() == Bytecodes::_iadd) {
            c = ao->x()->as_Constant();
            other = ao->y();
          }

          if (c) {
            int value = c->type()->as_IntConstant()->value();
            if (value != min_jint) {
              if (ao->op() == Bytecodes::_isub) {
                value = -value;
              }
              base += value;
              last_integer = base;
              last_instruction = other;
            }
            index = other;
          } else {
            break;
          }
          ao = index->as_ArithmeticOp();
        }
        add_access_indexed_info(indices, last_integer, last_instruction, ai);
      }
    }

    // Clear data structures for next array
    for (int i = 0; i < indices.length(); i++) {
      Instruction *index_instruction = indices.at(i);
      _access_indexed_info.at_put(index_instruction->id(), NULL);
    }
    indices.clear();
  }
}

bool RangeCheckEliminator::set_process_block_flags(BlockBegin *block) {
  Instruction *cur = block;
  bool process = false;

  while (cur) {
    process |= (cur->as_AccessIndexed() != NULL);
    cur = cur->next();
  }

  BlockList *dominates = block->dominates();
  for (int i = 0; i<dominates->length(); i++) {
    BlockBegin *next = dominates->at(i);
    process |= set_process_block_flags(next);
  }

  if (!process) {
    block->set(BlockBegin::donot_eliminate_range_checks);
  }
  return process;
}

bool RangeCheckEliminator::is_ok_for_deoptimization(Instruction *insert_position, Instruction *array_instr, Instruction *length_instr, Instruction *lower_instr, int lower, Instruction *upper_instr, int upper) {
  bool upper_check = true;

  if (upper_instr && upper_instr->as_ArrayLength() && upper_instr->as_ArrayLength()->array() == array_instr) {
    // static check
    if (upper >= 0) return false; // would always trigger a deopt:
                                  // array_length + x >= array_length, x >= 0 is always true
    upper_check = false;
  }
  if (lower_instr && lower_instr->as_ArrayLength() && lower_instr->as_ArrayLength()->array() == array_instr) {
    if (lower > 0) return false;
  }
  // No upper check required -> skip
  if (upper_check && upper_instr && upper_instr->type()->as_ObjectType() && upper_instr == array_instr) {
    // upper_instr is object means that the upper bound is the length
    // of the upper_instr.
    return false;
  }
  return true;
}

Instruction* RangeCheckEliminator::insert_after(Instruction* insert_position, Instruction* instr, int bci) {
  if (bci != -1) {
    return insert_position->insert_after(instr);
  } else {
    return insert_position->insert_after_same_bci(instr);
  }
}

Instruction* RangeCheckEliminator::predicate(Instruction* left, Instruction::Condition cond, Instruction* right, ValueStack* state, Instruction *insert_position, int bci) {
  RangeCheckPredicate *deoptimize = new RangeCheckPredicate(left, cond, true, right, state->copy());
  return insert_after(insert_position, deoptimize, bci);
}

Instruction* RangeCheckEliminator::predicate_cmp_with_const(Instruction* instr, Instruction::Condition cond, int constant, ValueStack* state, Instruction *insert_position, int bci) {
  Constant *const_instr = new Constant(new IntConstant(constant));
  insert_position = insert_after(insert_position, const_instr, bci);
  return predicate(instr, cond, const_instr, state, insert_position);
}

Instruction* RangeCheckEliminator::predicate_add(Instruction* left, int left_const, Instruction::Condition cond, Instruction* right, ValueStack* state, Instruction *insert_position, int bci) {
  Constant *constant = new Constant(new IntConstant(left_const));
  insert_position = insert_after(insert_position, constant, bci);
  ArithmeticOp *ao = new ArithmeticOp(Bytecodes::_iadd, constant, left, false, NULL);
  insert_position = insert_position->insert_after_same_bci(ao);
  return predicate(ao, cond, right, state, insert_position);
}

Instruction* RangeCheckEliminator::predicate_add_cmp_with_const(Instruction* left, int left_const, Instruction::Condition cond, int constant, ValueStack* state, Instruction *insert_position, int bci) {
  Constant *const_instr = new Constant(new IntConstant(constant));
  insert_position = insert_after(insert_position, const_instr, bci);
  return predicate_add(left, left_const, cond, const_instr, state, insert_position);
}

// Insert deoptimization
void RangeCheckEliminator::insert_deoptimization(ValueStack *state, Instruction *insert_position, Instruction *array_instr, Instruction *length_instr, Instruction *lower_instr, int lower, Instruction *upper_instr, int upper, AccessIndexed *ai) {
  bool upper_check = !(upper_instr && upper_instr->as_ArrayLength() && upper_instr->as_ArrayLength()->array() == array_instr);

  int bci = -1;
  if (lower_instr) {
    if (lower == 0) {
      // Compare for less than 0
      insert_position = predicate_cmp_with_const(lower_instr, Instruction::lss, 0, state, insert_position, bci);
    } else if (lower > 0) {
      // Compare for smaller 0
      insert_position = predicate_add_cmp_with_const(lower_instr, lower, Instruction::lss, 0, state, insert_position, bci);
    } else {
      // Add 1
      lower++;
      lower = -lower;
      // Compare for smaller or equal 0
      insert_position = predicate_cmp_with_const(lower_instr, Instruction::leq, lower, state, insert_position, bci);
    }
  }

  // No upper check required -> skip
  if (!upper_check) return;

  // We need to know length of array
  if (!length_instr) {
    // Load length if necessary
    ArrayLength *length = new ArrayLength(array_instr, state->copy());
    length->set_exception_state(length->state_before());
    length->set_flag(Instruction::DeoptimizeOnException, true);
    insert_position = insert_position->insert_after(length);
    length_instr = length;
  }

  if (!upper_instr) {
    // Compare for geq array.length
    insert_position = predicate_cmp_with_const(length_instr, Instruction::leq, upper, state, insert_position, bci);
  } else {
    if (upper_instr->type()->as_ObjectType()) {
      ArrayLength *length = new ArrayLength(upper_instr, state->copy());
      length->set_flag(Instruction::DeoptimizeOnException, true);
      length->set_exception_state(length->state_before());
      insert_position = insert_position->insert_after(length);
      upper_instr = length;
    }

    if (upper == 0) {
      // Compare for geq array.length
      insert_position = predicate(upper_instr, Instruction::geq, length_instr, state, insert_position, bci);
    } else if (upper < 0) {
      // Compare for geq array.length
      insert_position = predicate_add(upper_instr, upper, Instruction::geq, length_instr, state, insert_position, bci);
    } else {
      upper = -upper;
      // Compare for geq array.length
      insert_position = predicate_add(length_instr, upper, Instruction::leq, upper_instr, state, insert_position, bci);
    }
  }
}

// Add if condition
void RangeCheckEliminator::add_if_condition(IntegerStack &pushed, Value x, Value y, Instruction::Condition condition) {
  if (y->as_Constant()) return;

  int const_value = 0;
  Value instr_value = x;
  Constant *c = x->as_Constant();
  ArithmeticOp *ao = x->as_ArithmeticOp();

  if (c != NULL) {
    const_value = c->type()->as_IntConstant()->value();
    instr_value = NULL;
  } else if (ao != NULL && (!ao->x()->as_Constant() || !ao->y()->as_Constant()) && ((ao->op() == Bytecodes::_isub && ao->y()->as_Constant()) || ao->op() == Bytecodes::_iadd)) {
    c = ao->x()->as_Constant();
    if (c != NULL) {
      const_value = c->type()->as_IntConstant()->value();
      instr_value = ao->y();
    } else {
      c = ao->y()->as_Constant();
      if (c != NULL) {
        const_value = c->type()->as_IntConstant()->value();
        instr_value = ao->x();
      }
    }
    if (ao->op() == Bytecodes::_isub) {
      if (const_value > min_jint) {
        const_value = -const_value;
      } else {
        const_value = 0;
        instr_value = x;
      }
    }
  }

  update_bound(pushed, y, condition, instr_value, const_value);
}

// Process If
void RangeCheckEliminator::process_if(IntegerStack &pushed, BlockBegin *block, If *cond) {
  // Only if we are direct true / false successor and NOT both ! (even this may occur)
  if ((cond->tsux() == block || cond->fsux() == block) && cond->tsux() != cond->fsux()) {
    Instruction::Condition condition = cond->cond();
    if (cond->fsux() == block) {
      condition = Instruction::negate(condition);
    }
    Value x = cond->x();
    Value y = cond->y();
    if (x->type()->as_IntType() && y->type()->as_IntType()) {
      add_if_condition(pushed, y, x, condition);
      add_if_condition(pushed, x, y, Instruction::mirror(condition));
    }
  }
}

// Process access indexed
void RangeCheckEliminator::process_access_indexed(BlockBegin *loop_header, BlockBegin *block, AccessIndexed *ai) {
  if (ai->check_flag(Instruction::NeedsRangeCheckFlag)) {
    Bound *index_bound = get_bound(ai->index());
    if (!index_bound->has_lower() || !index_bound->has_upper()) {
      return;
    }

    Bound *array_bound;
    if (ai->length()) {
      array_bound = get_bound(ai->length());
    } else {
      array_bound = get_bound(ai->array());
    }

    if (in_array_bound(index_bound, ai->array()) || (index_bound && array_bound && index_bound->is_smaller(array_bound) && !index_bound->lower_instr() && index_bound->lower() >= 0)) {
        remove_range_check(ai);
    }
  }
}

void RangeCheckEliminator::remove_range_check(AccessIndexed *ai) {
  ai->set_flag(Instruction::NeedsRangeCheckFlag, false);
  // no range check, no need for the length instruction anymore
  ai->clear_length();
}

// Calculate bounds for instruction in this block and children blocks in the dominator tree
void RangeCheckEliminator::calc_bounds(BlockBegin *block, BlockBegin *loop_header) {
  // Pushed stack for conditions
  IntegerStack pushed;
  // Process If
  BlockBegin *parent = block->dominator();
  if (parent != NULL) {
    If *cond = parent->end()->as_If();
    if (cond != NULL) {
      process_if(pushed, block, cond);
    }
  }

  // Interate over current block
  InstructionList arrays;
  AccessIndexedList accessIndexed;
  Instruction *cur = block;

  while (cur) {
    // Ensure cur wasn't inserted during the elimination
    if (cur->id() < this->_bounds.length()) {
      // Process only if it is an access indexed instruction
      AccessIndexed *ai = cur->as_AccessIndexed();
      if (ai != NULL) {
        process_access_indexed(loop_header, block, ai);
        accessIndexed.append(ai);
        if (!arrays.contains(ai->array())) {
          arrays.append(ai->array());
        }
        Bound *b = get_bound(ai->index());
        if (!b->lower_instr()) {
          // Lower bound is constant
          update_bound(pushed, ai->index(), Instruction::geq, NULL, 0);
        }
        if (!b->has_upper()) {
          if (ai->length() && ai->length()->type()->as_IntConstant()) {
            int value = ai->length()->type()->as_IntConstant()->value();
            update_bound(pushed, ai->index(), Instruction::lss, NULL, value);
          } else {
            // Has no upper bound
            Instruction *instr = ai->length();
            if (instr != NULL) instr = ai->array();
            update_bound(pushed, ai->index(), Instruction::lss, instr, 0);
          }
        }
      }
    }
    cur = cur->next();
  }

  // Do in block motion of range checks
  in_block_motion(block, accessIndexed, arrays);

  // Call all dominated blocks
  for (int i = 0; i<block->dominates()->length(); i++) {
    BlockBegin *next = block->dominates()->at(i);
    if (!next->is_set(BlockBegin::donot_eliminate_range_checks)) {
      // if current block is a loop header and:
      // - next block belongs to the same loop
      // or
      // - next block belongs to an inner loop
      // then current block is the loop header for next block
      if (block->is_set(BlockBegin::linear_scan_loop_header_flag) && (block->loop_index() == next->loop_index() || next->loop_depth() > block->loop_depth())) {
        calc_bounds(next, block);
      } else {
        calc_bounds(next, loop_header);
      }
    }
  }

  // Reset stack
  for (int i = 0; i<pushed.length(); i++) {
    _bounds.at(pushed.at(i))->pop();
  }
}

// Verification or the IR
RangeCheckEliminator::Verification::Verification(IR *ir) : _used(BlockBegin::number_of_blocks(), BlockBegin::number_of_blocks(), false) {
  this->_ir = ir;
  ir->iterate_linear_scan_order(this);
}

// Verify this block
void RangeCheckEliminator::Verification::block_do(BlockBegin *block) {
  If *cond = block->end()->as_If();
  // Watch out: tsux and fsux can be the same!
  if (block->number_of_sux() > 1) {
    for (int i = 0; i<block->number_of_sux(); i++) {
      BlockBegin *sux = block->sux_at(i);
      BlockBegin *pred = NULL;
      for (int j = 0; j<sux->number_of_preds(); j++) {
        BlockBegin *cur = sux->pred_at(j);
        if (!pred) {
          pred = cur;
        }
      }
    }
  }

  if (block->is_set(BlockBegin::linear_scan_loop_header_flag)) {
    int loop_index = block->loop_index();
    BlockList *all_blocks = _ir->linear_scan_order();
    // Sometimes, the backbranch comes from an exception handler. In
    // this case, loop indexes/loop depths may not appear correct.
    bool loop_through_xhandler = false;
    for (int i = 0; i < block->number_of_exception_handlers(); i++) {
      BlockBegin *xhandler = block->exception_handler_at(i);
      for (int j = 0; j < block->number_of_preds(); j++) {
        if (dominates(xhandler, block->pred_at(j)) || xhandler == block->pred_at(j)) {
          loop_through_xhandler = true;
        }
      }
    }
  }

  Instruction *cur = block;
  while (cur) {
    cur = cur->next();
  }
}

// Loop header must dominate all loop blocks
bool RangeCheckEliminator::Verification::dominates(BlockBegin *dominator, BlockBegin *block) {
  BlockBegin *cur = block->dominator();
  while (cur && cur != dominator) {
    cur = cur->dominator();
  }
  return cur == dominator;
}

// Try to reach Block end beginning in Block start and not using Block dont_use
bool RangeCheckEliminator::Verification::can_reach(BlockBegin *start, BlockBegin *end, BlockBegin *dont_use /* = NULL */) {
  if (start == end) return start != dont_use;
  // Simple BSF from start to end
  //  BlockBeginList _current;
  for (int i = 0; i < _used.length(); i++) {
    _used.at_put(i, false);
  }
  _current.trunc_to(0);
  _successors.trunc_to(0);
  if (start != dont_use) {
    _current.push(start);
    _used.at_put(start->block_id(), true);
  }

  //  BlockBeginList _successors;
  while (_current.length() > 0) {
    BlockBegin *cur = _current.pop();
    // Add exception handlers to list
    for (int i = 0; i<cur->number_of_exception_handlers(); i++) {
      BlockBegin *xhandler = cur->exception_handler_at(i);
      _successors.push(xhandler);
      // Add exception handlers of _successors to list
      for (int j = 0; j<xhandler->number_of_exception_handlers(); j++) {
        BlockBegin *sux_xhandler = xhandler->exception_handler_at(j);
        _successors.push(sux_xhandler);
      }
    }
    // Add normal _successors to list
    for (int i = 0; i<cur->number_of_sux(); i++) {
      BlockBegin *sux = cur->sux_at(i);
      _successors.push(sux);
      // Add exception handlers of _successors to list
      for (int j = 0; j<sux->number_of_exception_handlers(); j++) {
        BlockBegin *xhandler = sux->exception_handler_at(j);
        _successors.push(xhandler);
      }
    }
    for (int i = 0; i<_successors.length(); i++) {
      BlockBegin *sux = _successors.at(i);
      if (sux == end) {
        return true;
      }
      if (sux != dont_use && !_used.at(sux->block_id())) {
        _used.at_put(sux->block_id(), true);
        _current.push(sux);
      }
    }
    _successors.trunc_to(0);
  }

  return false;
}

// Bound
RangeCheckEliminator::Bound::~Bound() { }

// Bound constructor
RangeCheckEliminator::Bound::Bound() {
  init();
  this->_lower = min_jint;
  this->_upper = max_jint;
  this->_lower_instr = NULL;
  this->_upper_instr = NULL;
}

// Bound constructor
RangeCheckEliminator::Bound::Bound(int lower, Value lower_instr, int upper, Value upper_instr) {
  init();
  this->_lower = lower;
  this->_upper = upper;
  this->_lower_instr = lower_instr;
  this->_upper_instr = upper_instr;
}

// Bound constructor
RangeCheckEliminator::Bound::Bound(Instruction::Condition cond, Value v, int constant) {
  init();
  if (cond == Instruction::eql) {
    _lower = constant;
    _lower_instr = v;
    _upper = constant;
    _upper_instr = v;
  } else if (cond == Instruction::neq) {
    _lower = min_jint;
    _upper = max_jint;
    _lower_instr = NULL;
    _upper_instr = NULL;
    if (v == NULL) {
      if (constant == min_jint) {
        _lower++;
      }
      if (constant == max_jint) {
        _upper--;
      }
    }
  } else if (cond == Instruction::geq) {
    _lower = constant;
    _lower_instr = v;
    _upper = max_jint;
    _upper_instr = NULL;
  } else if (cond == Instruction::leq) {
    _lower = min_jint;
    _lower_instr = NULL;
    _upper = constant;
    _upper_instr = v;
  } else {
    ShouldNotReachHere();
  }
}

// Set lower
void RangeCheckEliminator::Bound::set_lower(int value, Value v) {
  this->_lower = value;
  this->_lower_instr = v;
}

// Set upper
void RangeCheckEliminator::Bound::set_upper(int value, Value v) {
  this->_upper = value;
  this->_upper_instr = v;
}

// Add constant -> no overflow may occur
void RangeCheckEliminator::Bound::add_constant(int value) {
  this->_lower += value;
  this->_upper += value;
}

// Init
void RangeCheckEliminator::Bound::init() { }

// or
void RangeCheckEliminator::Bound::or_op(Bound *b) {
  // Watch out, bound is not guaranteed not to overflow!
  // Update lower bound
  if (_lower_instr != b->_lower_instr || (_lower_instr && _lower != b->_lower)) {
    _lower_instr = NULL;
    _lower = min_jint;
  } else {
    _lower = MIN2(_lower, b->_lower);
  }
  // Update upper bound
  if (_upper_instr != b->_upper_instr || (_upper_instr && _upper != b->_upper)) {
    _upper_instr = NULL;
    _upper = max_jint;
  } else {
    _upper = MAX2(_upper, b->_upper);
  }
}

// and
void RangeCheckEliminator::Bound::and_op(Bound *b) {
  // Update lower bound
  if (_lower_instr == b->_lower_instr) {
    _lower = MAX2(_lower, b->_lower);
  }
  if (b->has_lower()) {
    bool set = true;
    if (_lower_instr != NULL && b->_lower_instr != NULL) {
      set = (_lower_instr->dominator_depth() > b->_lower_instr->dominator_depth());
    }
    if (set) {
      _lower = b->_lower;
      _lower_instr = b->_lower_instr;
    }
  }
  // Update upper bound
  if (_upper_instr == b->_upper_instr) {
    _upper = MIN2(_upper, b->_upper);
  }
  if (b->has_upper()) {
    bool set = true;
    if (_upper_instr != NULL && b->_upper_instr != NULL) {
      set = (_upper_instr->dominator_depth() > b->_upper_instr->dominator_depth());
    }
    if (set) {
      _upper = b->_upper;
      _upper_instr = b->_upper_instr;
    }
  }
}

// has_upper
bool RangeCheckEliminator::Bound::has_upper() {
  return _upper_instr != NULL || _upper < max_jint;
}

// is_smaller
bool RangeCheckEliminator::Bound::is_smaller(Bound *b) {
  if (b->_lower_instr != _upper_instr) {
    return false;
  }
  return _upper < b->_lower;
}

// has_lower
bool RangeCheckEliminator::Bound::has_lower() {
  return _lower_instr != NULL || _lower > min_jint;
}

// in_array_bound
bool RangeCheckEliminator::in_array_bound(Bound *bound, Value array) {
  if (!bound) return false;
  if (bound->lower() >=0 && bound->lower_instr() == NULL && bound->upper() < 0 && bound->upper_instr() != NULL) {
    ArrayLength *len = bound->upper_instr()->as_ArrayLength();
    if (bound->upper_instr() == array || (len != NULL && len->array() == array)) {
      return true;
    }
  }
  return false;
}

// remove_lower
void RangeCheckEliminator::Bound::remove_lower() {
  _lower = min_jint;
  _lower_instr = NULL;
}

// remove_upper
void RangeCheckEliminator::Bound::remove_upper() {
  _upper = max_jint;
  _upper_instr = NULL;
}

// upper
int RangeCheckEliminator::Bound::upper() {
  return _upper;
}

// lower
int RangeCheckEliminator::Bound::lower() {
  return _lower;
}

// upper_instr
Value RangeCheckEliminator::Bound::upper_instr() {
  return _upper_instr;
}

// lower_instr
Value RangeCheckEliminator::Bound::lower_instr() {
  return _lower_instr;
}

// print
void RangeCheckEliminator::Bound::print() {
  tty->print("%s", "");
  if (this->_lower_instr || this->_lower != min_jint) {
    if (this->_lower_instr) {
      tty->print("i%d", this->_lower_instr->id());
      if (this->_lower > 0) {
        tty->print("+%d", _lower);
      }
      if (this->_lower < 0) {
        tty->print("%d", _lower);
      }
    } else {
      tty->print("%d", _lower);
    }
    tty->print(" <= ");
  }
  tty->print("x");
  if (this->_upper_instr || this->_upper != max_jint) {
    tty->print(" <= ");
    if (this->_upper_instr) {
      tty->print("i%d", this->_upper_instr->id());
      if (this->_upper > 0) {
        tty->print("+%d", _upper);
      }
      if (this->_upper < 0) {
        tty->print("%d", _upper);
      }
    } else {
      tty->print("%d", _upper);
    }
  }
}

// Copy
RangeCheckEliminator::Bound *RangeCheckEliminator::Bound::copy() {
  Bound *b = new Bound();
  b->_lower = _lower;
  b->_lower_instr = _lower_instr;
  b->_upper = _upper;
  b->_upper_instr = _upper_instr;
  return b;
}
