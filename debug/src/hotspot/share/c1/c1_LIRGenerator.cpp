#include "precompiled.hpp"

#include "c1/c1_Compilation.hpp"
#include "c1/c1_Defs.hpp"
#include "c1/c1_FrameMap.hpp"
#include "c1/c1_Instruction.hpp"
#include "c1/c1_LIRAssembler.hpp"
#include "c1/c1_LIRGenerator.hpp"
#include "c1/c1_ValueStack.hpp"
#include "ci/ciArrayKlass.hpp"
#include "ci/ciInstance.hpp"
#include "ci/ciObjArray.hpp"
#include "ci/ciUtilities.hpp"
#include "gc/shared/barrierSet.hpp"
#include "gc/shared/c1/barrierSetC1.hpp"
#include "runtime/arguments.hpp"
#include "runtime/sharedRuntime.hpp"
#include "runtime/stubRoutines.hpp"
#include "runtime/vm_version.hpp"
#include "utilities/bitMap.inline.hpp"
#include "utilities/macros.hpp"

#define __ gen()->lir()->

#ifndef PATCHED_ADDR
#define PATCHED_ADDR  (max_jint)
#endif

void PhiResolverState::reset(int max_vregs) {
  // Initialize array sizes
  _virtual_operands.at_put_grow(max_vregs - 1, NULL, NULL);
  _virtual_operands.trunc_to(0);
  _other_operands.at_put_grow(max_vregs - 1, NULL, NULL);
  _other_operands.trunc_to(0);
  _vreg_table.at_put_grow(max_vregs - 1, NULL, NULL);
  _vreg_table.trunc_to(0);
}

//--------------------------------------------------------------
// PhiResolver

// Resolves cycles:
//
//  r1 := r2  becomes  temp := r1
//  r2 := r1           r1 := r2
//                     r2 := temp
// and orders moves:
//
//  r2 := r3  becomes  r1 := r2
//  r1 := r2           r2 := r3

PhiResolver::PhiResolver(LIRGenerator* gen, int max_vregs)
 : _gen(gen)
 , _state(gen->resolver_state())
 , _temp(LIR_OprFact::illegalOpr)
{
  // reinitialize the shared state arrays
  _state.reset(max_vregs);
}

void PhiResolver::emit_move(LIR_Opr src, LIR_Opr dest) {
  __ move(src, dest);
}

void PhiResolver::move_temp_to(LIR_Opr dest) {
  emit_move(_temp, dest);
}

void PhiResolver::move_to_temp(LIR_Opr src) {
  _temp = _gen->new_register(src->type());
  emit_move(src, _temp);
}

// Traverse assignment graph in depth first order and generate moves in post order
// ie. two assignments: b := c, a := b start with node c:
// Call graph: move(NULL, c) -> move(c, b) -> move(b, a)
// Generates moves in this order: move b to a and move c to b
// ie. cycle a := b, b := a start with node a
// Call graph: move(NULL, a) -> move(a, b) -> move(b, a)
// Generates moves in this order: move b to temp, move a to b, move temp to a
void PhiResolver::move(ResolveNode* src, ResolveNode* dest) {
  if (!dest->visited()) {
    dest->set_visited();
    for (int i = dest->no_of_destinations()-1; i >= 0; i--) {
      move(dest, dest->destination_at(i));
    }
  } else if (!dest->start_node()) {
    // cylce in graph detected
    _loop = dest;
    move_to_temp(src->operand());
    return;
  }

  if (!dest->assigned()) {
    if (_loop == dest) {
      move_temp_to(dest->operand());
      dest->set_assigned();
    } else if (src != NULL) {
      emit_move(src->operand(), dest->operand());
      dest->set_assigned();
    }
  }
}

PhiResolver::~PhiResolver() {
  int i;
  // resolve any cycles in moves from and to virtual registers
  for (i = virtual_operands().length() - 1; i >= 0; i--) {
    ResolveNode* node = virtual_operands().at(i);
    if (!node->visited()) {
      _loop = NULL;
      move(NULL, node);
      node->set_start_node();
    }
  }

  // generate move for move from non virtual register to abitrary destination
  for (i = other_operands().length() - 1; i >= 0; i--) {
    ResolveNode* node = other_operands().at(i);
    for (int j = node->no_of_destinations() - 1; j >= 0; j--) {
      emit_move(node->operand(), node->destination_at(j)->operand());
    }
  }
}

ResolveNode* PhiResolver::create_node(LIR_Opr opr, bool source) {
  ResolveNode* node;
  if (opr->is_virtual()) {
    int vreg_num = opr->vreg_number();
    node = vreg_table().at_grow(vreg_num, NULL);
    if (node == NULL) {
      node = new ResolveNode(opr);
      vreg_table().at_put(vreg_num, node);
    }
    // Make sure that all virtual operands show up in the list when
    // they are used as the source of a move.
    if (source && !virtual_operands().contains(node)) {
      virtual_operands().append(node);
    }
  } else {
    node = new ResolveNode(opr);
    other_operands().append(node);
  }
  return node;
}

void PhiResolver::move(LIR_Opr src, LIR_Opr dest) {
  ResolveNode* source = source_node(src);
  source->append(destination_node(dest));
}

//--------------------------------------------------------------
// LIRItem

void LIRItem::set_result(LIR_Opr opr) {
  value()->set_operand(opr);

  if (opr->is_virtual()) {
    _gen->_instruction_for_operand.at_put_grow(opr->vreg_number(), value(), NULL);
  }

  _result = opr;
}

void LIRItem::load_item() {
  if (result()->is_illegal()) {
    // update the items result
    _result = value()->operand();
  }
  if (!result()->is_register()) {
    LIR_Opr reg = _gen->new_register(value()->type());
    __ move(result(), reg);
    if (result()->is_constant()) {
      _result = reg;
    } else {
      set_result(reg);
    }
  }
}

void LIRItem::load_for_store(BasicType type) {
  if (_gen->can_store_as_constant(value(), type)) {
    _result = value()->operand();
    if (!_result->is_constant()) {
      _result = LIR_OprFact::value_type(value()->type());
    }
  } else if (type == T_BYTE || type == T_BOOLEAN) {
    load_byte_item();
  } else {
    load_item();
  }
}

void LIRItem::load_item_force(LIR_Opr reg) {
  LIR_Opr r = result();
  if (r != reg) {
#if !defined(ARM) && !defined(E500V2)
    if (r->type() != reg->type()) {
      // moves between different types need an intervening spill slot
      r = _gen->force_to_spill(r, reg->type());
    }
#endif
    __ move(r, reg);
    _result = reg;
  }
}

ciObject* LIRItem::get_jobject_constant() const {
  ObjectType* oc = type()->as_ObjectType();
  if (oc) {
    return oc->constant_value();
  }
  return NULL;
}

jint LIRItem::get_jint_constant() const {
  return type()->as_IntConstant()->value();
}

jint LIRItem::get_address_constant() const {
  return type()->as_AddressConstant()->value();
}

jfloat LIRItem::get_jfloat_constant() const {
  return type()->as_FloatConstant()->value();
}

jdouble LIRItem::get_jdouble_constant() const {
  return type()->as_DoubleConstant()->value();
}

jlong LIRItem::get_jlong_constant() const {
  return type()->as_LongConstant()->value();
}

//--------------------------------------------------------------

void LIRGenerator::block_do_prolog(BlockBegin* block) {
  // set up the list of LIR instructions
  _lir = new LIR_List(compilation(), block);
  block->set_lir(_lir);

  __ branch_destination(block->label());
}

void LIRGenerator::block_do_epilog(BlockBegin* block) {
  // LIR_Opr for unpinned constants shouldn't be referenced by other
  // blocks so clear them out after processing the block.
  for (int i = 0; i < _unpinned_constants.length(); i++) {
    _unpinned_constants.at(i)->clear_operand();
  }
  _unpinned_constants.trunc_to(0);

  // clear our any registers for other local constants
  _constants.trunc_to(0);
  _reg_for_constants.trunc_to(0);
}

void LIRGenerator::block_do(BlockBegin* block) {
  CHECK_BAILOUT();

  block_do_prolog(block);
  set_block(block);

  for (Instruction* instr = block; instr != NULL; instr = instr->next()) {
    if (instr->is_pinned()) do_root(instr);
  }

  set_block(NULL);
  block_do_epilog(block);
}

//-------------------------LIRGenerator-----------------------------

// This is where the tree-walk starts; instr must be root;
void LIRGenerator::do_root(Value instr) {
  CHECK_BAILOUT();

  InstructionMark im(compilation(), instr);

  instr->visit(this);
}

// This is called for each node in tree; the walk stops if a root is reached
void LIRGenerator::walk(Value instr) {
  InstructionMark im(compilation(), instr);
  //stop walk when encounter a root
  if ((instr->is_pinned() && instr->as_Phi() == NULL) || instr->operand()->is_valid()) {
  } else {
    instr->visit(this);
  }
}

CodeEmitInfo* LIRGenerator::state_for(Instruction* x, ValueStack* state, bool ignore_xhandler) {
  ValueStack* s = state;
  for_each_state(s) {
    if (s->kind() == ValueStack::EmptyExceptionState) {
      continue;
    }

    int index;
    Value value;
    for_each_stack_value(s, index, value) {
      if (!value->is_pinned() && value->as_Constant() == NULL && value->as_Local() == NULL) {
        walk(value);
      }
    }

    int bci = s->bci();
    IRScope* scope = s->scope();
    ciMethod* method = scope->method();

    MethodLivenessResult liveness = method->liveness_at_bci(bci);
    if (bci == SynchronizationEntryBCI) {
      if (x->as_ExceptionObject() || x->as_Throw()) {
        // all locals are dead on exit from the synthetic unlocker
        liveness.clear();
      }
    }
    if (!liveness.is_valid()) {
      // Degenerate or breakpointed method.
      bailout("Degenerate or breakpointed method");
    } else {
      for_each_local_value(s, index, value) {
        if (liveness.at(index) && !value->type()->is_illegal()) {
          if (!value->is_pinned() && value->as_Constant() == NULL && value->as_Local() == NULL) {
            walk(value);
          }
        } else {
          // NULL out this local so that linear scan can assume that all non-NULL values are live.
          s->invalidate_local(index);
        }
      }
    }
  }

  return new CodeEmitInfo(state, ignore_xhandler ? NULL : x->exception_handlers(), x->check_flag(Instruction::DeoptimizeOnException));
}

CodeEmitInfo* LIRGenerator::state_for(Instruction* x) {
  return state_for(x, x->exception_state());
}

void LIRGenerator::klass2reg_with_patching(LIR_Opr r, ciMetadata* obj, CodeEmitInfo* info, bool need_resolve) {
  /* C2 relies on constant pool entries being resolved (ciTypeFlow), so if ... is active
   * and the class hasn't yet been resolved we need to emit a patch that resolves the class.
   */
  if (!obj->is_loaded()) {
    __ klass2reg_patch(NULL, r, info);
  } else {
    // no patching needed
    __ metadata2reg(obj->constant_encoding(), r);
  }
}

void LIRGenerator::array_range_check(LIR_Opr array, LIR_Opr index, CodeEmitInfo* null_check_info, CodeEmitInfo* range_check_info) {
  CodeStub* stub = new RangeCheckStub(range_check_info, index, array);
  if (index->is_constant()) {
    cmp_mem_int(lir_cond_belowEqual, array, arrayOopDesc::length_offset_in_bytes(), index->as_jint(), null_check_info);
    __ branch(lir_cond_belowEqual, T_INT, stub); // forward branch
  } else {
    cmp_reg_mem(lir_cond_aboveEqual, index, array, arrayOopDesc::length_offset_in_bytes(), T_INT, null_check_info);
    __ branch(lir_cond_aboveEqual, T_INT, stub); // forward branch
  }
}

void LIRGenerator::nio_range_check(LIR_Opr buffer, LIR_Opr index, LIR_Opr result, CodeEmitInfo* info) {
  CodeStub* stub = new RangeCheckStub(info, index);
  if (index->is_constant()) {
    cmp_mem_int(lir_cond_belowEqual, buffer, java_nio_Buffer::limit_offset(), index->as_jint(), info);
    __ branch(lir_cond_belowEqual, T_INT, stub); // forward branch
  } else {
    cmp_reg_mem(lir_cond_aboveEqual, index, buffer,
                java_nio_Buffer::limit_offset(), T_INT, info);
    __ branch(lir_cond_aboveEqual, T_INT, stub); // forward branch
  }
  __ move(index, result);
}

void LIRGenerator::arithmetic_op(Bytecodes::Code code, LIR_Opr result, LIR_Opr left, LIR_Opr right, bool is_strictfp, LIR_Opr tmp_op, CodeEmitInfo* info) {
  LIR_Opr result_op = result;
  LIR_Opr left_op   = left;
  LIR_Opr right_op  = right;

  if (TwoOperandLIRForm && left_op != result_op) {
    __ move(left_op, result_op);
    left_op = result_op;
  }

  switch (code) {
    case Bytecodes::_dadd:
    case Bytecodes::_fadd:
    case Bytecodes::_ladd:
    case Bytecodes::_iadd:  __ add(left_op, right_op, result_op); break;
    case Bytecodes::_fmul:
    case Bytecodes::_lmul:  __ mul(left_op, right_op, result_op); break;

    case Bytecodes::_dmul:
      {
        if (is_strictfp) {
          __ mul_strictfp(left_op, right_op, result_op, tmp_op); break;
        } else {
          __ mul(left_op, right_op, result_op); break;
        }
      }
      break;

    case Bytecodes::_imul:
      {
        bool did_strength_reduce = false;

        if (right->is_constant()) {
          jint c = right->as_jint();
          if (c > 0 && is_power_of_2(c)) {
            // do not need tmp here
            __ shift_left(left_op, exact_log2(c), result_op);
            did_strength_reduce = true;
          } else {
            did_strength_reduce = strength_reduce_multiply(left_op, c, result_op, tmp_op);
          }
        }
        // we couldn't strength reduce so just emit the multiply
        if (!did_strength_reduce) {
          __ mul(left_op, right_op, result_op);
        }
      }
      break;

    case Bytecodes::_dsub:
    case Bytecodes::_fsub:
    case Bytecodes::_lsub:
    case Bytecodes::_isub: __ sub(left_op, right_op, result_op); break;

    case Bytecodes::_fdiv: __ div (left_op, right_op, result_op); break;
    // ldiv and lrem are implemented with a direct runtime call

    case Bytecodes::_ddiv:
      {
        if (is_strictfp) {
          __ div_strictfp (left_op, right_op, result_op, tmp_op); break;
        } else {
          __ div (left_op, right_op, result_op); break;
        }
      }
      break;

    case Bytecodes::_drem:
    case Bytecodes::_frem: __ rem (left_op, right_op, result_op); break;

    default: ShouldNotReachHere();
  }
}

void LIRGenerator::arithmetic_op_int(Bytecodes::Code code, LIR_Opr result, LIR_Opr left, LIR_Opr right, LIR_Opr tmp) {
  arithmetic_op(code, result, left, right, false, tmp);
}

void LIRGenerator::arithmetic_op_long(Bytecodes::Code code, LIR_Opr result, LIR_Opr left, LIR_Opr right, CodeEmitInfo* info) {
  arithmetic_op(code, result, left, right, false, LIR_OprFact::illegalOpr, info);
}

void LIRGenerator::arithmetic_op_fpu(Bytecodes::Code code, LIR_Opr result, LIR_Opr left, LIR_Opr right, bool is_strictfp, LIR_Opr tmp) {
  arithmetic_op(code, result, left, right, is_strictfp, tmp);
}

void LIRGenerator::shift_op(Bytecodes::Code code, LIR_Opr result_op, LIR_Opr value, LIR_Opr count, LIR_Opr tmp) {
  if (TwoOperandLIRForm && value != result_op) {
    __ move(value, result_op);
    value = result_op;
  }

  switch (code) {
  case Bytecodes::_ishl:
  case Bytecodes::_lshl: __ shift_left(value, count, result_op, tmp); break;
  case Bytecodes::_ishr:
  case Bytecodes::_lshr: __ shift_right(value, count, result_op, tmp); break;
  case Bytecodes::_iushr:
  case Bytecodes::_lushr: __ unsigned_shift_right(value, count, result_op, tmp); break;
  default: ShouldNotReachHere();
  }
}

void LIRGenerator::logic_op(Bytecodes::Code code, LIR_Opr result_op, LIR_Opr left_op, LIR_Opr right_op) {
  if (TwoOperandLIRForm && left_op != result_op) {
    __ move(left_op, result_op);
    left_op = result_op;
  }

  switch (code) {
    case Bytecodes::_iand:
    case Bytecodes::_land:  __ logical_and(left_op, right_op, result_op); break;

    case Bytecodes::_ior:
    case Bytecodes::_lor:   __ logical_or(left_op, right_op, result_op);  break;

    case Bytecodes::_ixor:
    case Bytecodes::_lxor:  __ logical_xor(left_op, right_op, result_op); break;

    default: ShouldNotReachHere();
  }
}

void LIRGenerator::monitor_enter(LIR_Opr object, LIR_Opr lock, LIR_Opr hdr, LIR_Opr scratch, int monitor_no, CodeEmitInfo* info_for_exception, CodeEmitInfo* info) {
  if (!GenerateSynchronizationCode) return;
  // for slow path, use debug info for state after successful locking
  CodeStub* slow_path = new MonitorEnterStub(object, lock, info);
  __ load_stack_address_monitor(monitor_no, lock);
  // for handling NullPointerException, use debug info representing just the lock stack before this monitorenter
  __ lock_object(hdr, object, lock, scratch, slow_path, info_for_exception);
}

void LIRGenerator::monitor_exit(LIR_Opr object, LIR_Opr lock, LIR_Opr new_hdr, LIR_Opr scratch, int monitor_no) {
  if (!GenerateSynchronizationCode) return;
  // setup registers
  LIR_Opr hdr = lock;
  lock = new_hdr;
  CodeStub* slow_path = new MonitorExitStub(lock, UseFastLocking, monitor_no);
  __ load_stack_address_monitor(monitor_no, lock);
  __ unlock_object(hdr, object, lock, scratch, slow_path);
}

void LIRGenerator::new_instance(LIR_Opr dst, ciInstanceKlass* klass, bool is_unresolved, LIR_Opr scratch1, LIR_Opr scratch2, LIR_Opr scratch3, LIR_Opr scratch4, LIR_Opr klass_reg, CodeEmitInfo* info) {
  klass2reg_with_patching(klass_reg, klass, info, is_unresolved);
  // If klass is not loaded we do not know if the klass has finalizers:
  if (UseFastNewInstance && klass->is_loaded() && !Klass::layout_helper_needs_slow_path(klass->layout_helper())) {
    Runtime1::StubID stub_id = klass->is_initialized() ? Runtime1::fast_new_instance_id : Runtime1::fast_new_instance_init_check_id;

    CodeStub* slow_path = new NewInstanceStub(klass_reg, dst, klass, info, stub_id);

    // allocate space for instance
    const int instance_size = align_object_size(klass->size_helper());
    __ allocate_object(dst, scratch1, scratch2, scratch3, scratch4, oopDesc::header_size(), instance_size, klass_reg, !klass->is_initialized(), slow_path);
  } else {
    CodeStub* slow_path = new NewInstanceStub(klass_reg, dst, klass, info, Runtime1::new_instance_id);
    __ branch(lir_cond_always, T_ILLEGAL, slow_path);
    __ branch_destination(slow_path->continuation());
  }
}

static bool is_constant_zero(Instruction* inst) {
  IntConstant* c = inst->type()->as_IntConstant();
  if (c) {
    return (c->value() == 0);
  }
  return false;
}

static bool positive_constant(Instruction* inst) {
  IntConstant* c = inst->type()->as_IntConstant();
  if (c) {
    return (c->value() >= 0);
  }
  return false;
}

static ciArrayKlass* as_array_klass(ciType* type) {
  if (type != NULL && type->is_array_klass() && type->is_loaded()) {
    return (ciArrayKlass*)type;
  } else {
    return NULL;
  }
}

static ciType* phi_declared_type(Phi* phi) {
  ciType* t = phi->operand_at(0)->declared_type();
  if (t == NULL) {
    return NULL;
  }
  for (int i = 1; i < phi->operand_count(); i++) {
    if (t != phi->operand_at(i)->declared_type()) {
      return NULL;
    }
  }
  return t;
}

void LIRGenerator::arraycopy_helper(Intrinsic* x, int* flagsp, ciArrayKlass** expected_typep) {
  Instruction* src     = x->argument_at(0);
  Instruction* src_pos = x->argument_at(1);
  Instruction* dst     = x->argument_at(2);
  Instruction* dst_pos = x->argument_at(3);
  Instruction* length  = x->argument_at(4);

  // first try to identify the likely type of the arrays involved
  ciArrayKlass* expected_type = NULL;
  bool is_exact = false, src_objarray = false, dst_objarray = false;
  {
    ciArrayKlass* src_exact_type    = as_array_klass(src->exact_type());
    ciArrayKlass* src_declared_type = as_array_klass(src->declared_type());
    Phi* phi;
    if (src_declared_type == NULL && (phi = src->as_Phi()) != NULL) {
      src_declared_type = as_array_klass(phi_declared_type(phi));
    }
    ciArrayKlass* dst_exact_type    = as_array_klass(dst->exact_type());
    ciArrayKlass* dst_declared_type = as_array_klass(dst->declared_type());
    if (dst_declared_type == NULL && (phi = dst->as_Phi()) != NULL) {
      dst_declared_type = as_array_klass(phi_declared_type(phi));
    }

    if (src_exact_type != NULL && src_exact_type == dst_exact_type) {
      // the types exactly match so the type is fully known
      is_exact = true;
      expected_type = src_exact_type;
    } else if (dst_exact_type != NULL && dst_exact_type->is_obj_array_klass()) {
      ciArrayKlass* dst_type = (ciArrayKlass*) dst_exact_type;
      ciArrayKlass* src_type = NULL;
      if (src_exact_type != NULL && src_exact_type->is_obj_array_klass()) {
        src_type = (ciArrayKlass*) src_exact_type;
      } else if (src_declared_type != NULL && src_declared_type->is_obj_array_klass()) {
        src_type = (ciArrayKlass*) src_declared_type;
      }
      if (src_type != NULL) {
        if (src_type->element_type()->is_subtype_of(dst_type->element_type())) {
          is_exact = true;
          expected_type = dst_type;
        }
      }
    }
    // at least pass along a good guess
    if (expected_type == NULL) expected_type = dst_exact_type;
    if (expected_type == NULL) expected_type = src_declared_type;
    if (expected_type == NULL) expected_type = dst_declared_type;

    src_objarray = (src_exact_type && src_exact_type->is_obj_array_klass()) || (src_declared_type && src_declared_type->is_obj_array_klass());
    dst_objarray = (dst_exact_type && dst_exact_type->is_obj_array_klass()) || (dst_declared_type && dst_declared_type->is_obj_array_klass());
  }

  // if a probable array type has been identified, figure out if any
  // of the required checks for a fast case can be elided.
  int flags = LIR_OpArrayCopy::all_flags;

  if (!src_objarray)
    flags &= ~LIR_OpArrayCopy::src_objarray;
  if (!dst_objarray)
    flags &= ~LIR_OpArrayCopy::dst_objarray;

  if (!x->arg_needs_null_check(0))
    flags &= ~LIR_OpArrayCopy::src_null_check;
  if (!x->arg_needs_null_check(2))
    flags &= ~LIR_OpArrayCopy::dst_null_check;

  if (expected_type != NULL) {
    Value length_limit = NULL;

    IfOp* ifop = length->as_IfOp();
    if (ifop != NULL) {
      // look for expressions like min(v, a.length) which ends up as
      //   x > y ? y : x  or  x >= y ? y : x
      if ((ifop->cond() == If::gtr || ifop->cond() == If::geq) && ifop->x() == ifop->fval() && ifop->y() == ifop->tval()) {
        length_limit = ifop->y();
      }
    }

    // try to skip null checks and range checks
    NewArray* src_array = src->as_NewArray();
    if (src_array != NULL) {
      flags &= ~LIR_OpArrayCopy::src_null_check;
      if (length_limit != NULL && src_array->length() == length_limit && is_constant_zero(src_pos)) {
        flags &= ~LIR_OpArrayCopy::src_range_check;
      }
    }

    NewArray* dst_array = dst->as_NewArray();
    if (dst_array != NULL) {
      flags &= ~LIR_OpArrayCopy::dst_null_check;
      if (length_limit != NULL && dst_array->length() == length_limit && is_constant_zero(dst_pos)) {
        flags &= ~LIR_OpArrayCopy::dst_range_check;
      }
    }

    // check from incoming constant values
    if (positive_constant(src_pos))
      flags &= ~LIR_OpArrayCopy::src_pos_positive_check;
    if (positive_constant(dst_pos))
      flags &= ~LIR_OpArrayCopy::dst_pos_positive_check;
    if (positive_constant(length))
      flags &= ~LIR_OpArrayCopy::length_positive_check;

    // see if the range check can be elided, which might also imply
    // that src or dst is non-null.
    ArrayLength* al = length->as_ArrayLength();
    if (al != NULL) {
      if (al->array() == src) {
        // it's the length of the source array
        flags &= ~LIR_OpArrayCopy::length_positive_check;
        flags &= ~LIR_OpArrayCopy::src_null_check;
        if (is_constant_zero(src_pos))
          flags &= ~LIR_OpArrayCopy::src_range_check;
      }
      if (al->array() == dst) {
        // it's the length of the destination array
        flags &= ~LIR_OpArrayCopy::length_positive_check;
        flags &= ~LIR_OpArrayCopy::dst_null_check;
        if (is_constant_zero(dst_pos))
          flags &= ~LIR_OpArrayCopy::dst_range_check;
      }
    }
    if (is_exact) {
      flags &= ~LIR_OpArrayCopy::type_check;
    }
  }

  IntConstant* src_int = src_pos->type()->as_IntConstant();
  IntConstant* dst_int = dst_pos->type()->as_IntConstant();
  if (src_int && dst_int) {
    int s_offs = src_int->value();
    int d_offs = dst_int->value();
    if (src_int->value() >= dst_int->value()) {
      flags &= ~LIR_OpArrayCopy::overlapping;
    }
    if (expected_type != NULL) {
      BasicType t = expected_type->element_type()->basic_type();
      int element_size = type2aelembytes(t);
      if (((arrayOopDesc::base_offset_in_bytes(t) + s_offs * element_size) % HeapWordSize == 0) &&
          ((arrayOopDesc::base_offset_in_bytes(t) + d_offs * element_size) % HeapWordSize == 0)) {
        flags &= ~LIR_OpArrayCopy::unaligned;
      }
    }
  } else if (src_pos == dst_pos || is_constant_zero(dst_pos)) {
    // src and dest positions are the same, or dst is zero so assume
    // nonoverlapping copy.
    flags &= ~LIR_OpArrayCopy::overlapping;
  }

  if (src == dst) {
    // moving within a single array so no type checks are needed
    if (flags & LIR_OpArrayCopy::type_check) {
      flags &= ~LIR_OpArrayCopy::type_check;
    }
  }
  *flagsp = flags;
  *expected_typep = (ciArrayKlass*)expected_type;
}

LIR_Opr LIRGenerator::round_item(LIR_Opr opr) {
  if (RoundFPResults && UseSSE < 1 && opr->is_single_fpu()) {
    LIR_Opr result = new_register(T_FLOAT);
    set_vreg_flag(result, must_start_in_memory);
    __ roundfp(opr, LIR_OprFact::illegalOpr, result);
    return result;
  }
  return opr;
}

LIR_Opr LIRGenerator::force_to_spill(LIR_Opr value, BasicType t) {
  if (!value->is_register()) {
    // force into a register
    LIR_Opr r = new_register(value->type());
    __ move(value, r);
    value = r;
  }

  // create a spill location
  LIR_Opr tmp = new_register(t);
  set_vreg_flag(tmp, LIRGenerator::must_start_in_memory);

  // move from register to spill
  __ move(value, tmp);
  return tmp;
}

// Phi technique:
// This is about passing live values from one basic block to the other.
// In code generated with Java it is rather rare that more than one
// value is on the stack from one basic block to the other.
// We optimize our technique for efficient passing of one value
// (of type long, int, double..) but it can be extended.
// When entering or leaving a basic block, all registers and all spill
// slots are release and empty. We use the released registers
// and spill slots to pass the live values from one block
// to the other. The topmost value, i.e., the value on TOS of expression
// stack is passed in registers. All other values are stored in spilling
// area. Every Phi has an index which designates its spill slot
// At exit of a basic block, we fill the register(s) and spill slots.
// At entry of a basic block, the block_prolog sets up the content of phi nodes
// and locks necessary registers and spilling slots.

// move current value to referenced phi function
void LIRGenerator::move_to_phi(PhiResolver* resolver, Value cur_val, Value sux_val) {
  Phi* phi = sux_val->as_Phi();
  // cur_val can be null without phi being null in conjunction with inlining
  if (phi != NULL && cur_val != NULL && cur_val != phi && !phi->is_illegal()) {
    Phi* cur_phi = cur_val->as_Phi();
    if (cur_phi != NULL && cur_phi->is_illegal()) {
      // Phi and local would need to get invalidated
      // (which is unexpected for Linear Scan).
      // But this case is very rare so we simply bail out.
      bailout("propagation of illegal phi");
      return;
    }
    LIR_Opr operand = cur_val->operand();
    if (operand->is_illegal()) {
      operand = operand_for_instruction(cur_val);
    }
    resolver->move(operand, operand_for_instruction(phi));
  }
}

// Moves all stack values into their PHI position
void LIRGenerator::move_to_phi(ValueStack* cur_state) {
  BlockBegin* bb = block();
  if (bb->number_of_sux() == 1) {
    BlockBegin* sux = bb->sux_at(0);

    // a block with only one predecessor never has phi functions
    if (sux->number_of_preds() > 1) {
      int max_phis = cur_state->stack_size() + cur_state->locals_size();
      PhiResolver resolver(this, _virtual_register_number + max_phis * 2);

      ValueStack* sux_state = sux->state();
      Value sux_value;
      int index;

      for_each_stack_value(sux_state, index, sux_value) {
        move_to_phi(&resolver, cur_state->stack_at(index), sux_value);
      }

      for_each_local_value(sux_state, index, sux_value) {
        move_to_phi(&resolver, cur_state->local_at(index), sux_value);
      }
    }
  }
}

LIR_Opr LIRGenerator::new_register(BasicType type) {
  int vreg = _virtual_register_number;
  // add a little fudge factor for the bailout, since the bailout is
  // only checked periodically.  This gives a few extra registers to
  // hand out before we really run out, which helps us keep from
  // tripping over assertions.
  if (vreg + 20 >= LIR_OprDesc::vreg_max) {
    bailout("out of virtual registers");
    if (vreg + 2 >= LIR_OprDesc::vreg_max) {
      // wrap it around
      _virtual_register_number = LIR_OprDesc::vreg_base;
    }
  }
  _virtual_register_number += 1;
  return LIR_OprFact::virtual_register(vreg, type);
}

// Try to lock using register in hint
LIR_Opr LIRGenerator::rlock(Value instr) {
  return new_register(instr->type());
}

// does an rlock and sets result
LIR_Opr LIRGenerator::rlock_result(Value x) {
  LIR_Opr reg = rlock(x);
  set_result(x, reg);
  return reg;
}

// does an rlock and sets result
LIR_Opr LIRGenerator::rlock_result(Value x, BasicType type) {
  LIR_Opr reg;
  switch (type) {
  case T_BYTE:
  case T_BOOLEAN:
    reg = rlock_byte(type);
    break;
  default:
    reg = rlock(x);
    break;
  }

  set_result(x, reg);
  return reg;
}

//---------------------------------------------------------------------
ciObject* LIRGenerator::get_jobject_constant(Value value) {
  ObjectType* oc = value->type()->as_ObjectType();
  if (oc) {
    return oc->constant_value();
  }
  return NULL;
}

//----------------------------------------------------------------------
//----------------------------------------------------------------------
//----------------------------------------------------------------------
//----------------------------------------------------------------------
//                        visitor functions
//----------------------------------------------------------------------
//----------------------------------------------------------------------
//----------------------------------------------------------------------
//----------------------------------------------------------------------

void LIRGenerator::do_Phi(Phi* x) {
  // phi functions are never visited directly
  ShouldNotReachHere();
}

// Code for a constant is generated lazily unless the constant is frequently used and can't be inlined.
void LIRGenerator::do_Constant(Constant* x) {
  if (x->state_before() != NULL) {
    // Any constant with a ValueStack requires patching so emit the patch here
    LIR_Opr reg = rlock_result(x);
    CodeEmitInfo* info = state_for(x, x->state_before());
    __ oop2reg_patch(NULL, reg, info);
  } else if (x->use_count() > 1 && !can_inline_as_constant(x)) {
    if (!x->is_pinned()) {
      // unpinned constants are handled specially so that they can be
      // put into registers when they are used multiple times within a
      // block.  After the block completes their operand will be
      // cleared so that other blocks can't refer to that register.
      set_result(x, load_constant(x));
    } else {
      LIR_Opr res = x->operand();
      if (!res->is_valid()) {
        res = LIR_OprFact::value_type(x->type());
      }
      if (res->is_constant()) {
        LIR_Opr reg = rlock_result(x);
        __ move(res, reg);
      } else {
        set_result(x, res);
      }
    }
  } else {
    set_result(x, LIR_OprFact::value_type(x->type()));
  }
}

void LIRGenerator::do_Local(Local* x) {
  // operand_for_instruction has the side effect of setting the result
  // so there's no need to do it here.
  operand_for_instruction(x);
}

void LIRGenerator::do_IfInstanceOf(IfInstanceOf* x) {
  Unimplemented();
}

void LIRGenerator::do_Return(Return* x) {
  if (x->type()->is_void()) {
    __ return_op(LIR_OprFact::illegalOpr);
  } else {
    LIR_Opr reg = result_register_for(x->type(), /*callee=*/true);
    LIRItem result(x->result(), this);

    result.load_item_force(reg);
    __ return_op(result.result());
  }
  set_no_result(x);
}

// Examble: ref.get()
// Combination of LoadField and g1 pre-write barrier
void LIRGenerator::do_Reference_get(Intrinsic* x) {
  const int referent_offset = java_lang_ref_Reference::referent_offset;
  guarantee(referent_offset > 0, "referent offset not initialized");

  LIRItem reference(x->argument_at(0), this);
  reference.load_item();

  // need to perform the null check on the reference objecy
  CodeEmitInfo* info = NULL;
  if (x->needs_null_check()) {
    info = state_for(x);
  }

  LIR_Opr result = rlock_result(x, T_OBJECT);
  access_load_at(IN_HEAP | ON_WEAK_OOP_REF, T_OBJECT, reference, LIR_OprFact::intConst(referent_offset), result);
}

// Example: clazz.isInstance(object)
void LIRGenerator::do_isInstance(Intrinsic* x) {
  // TODO could try to substitute this node with an equivalent InstanceOf
  // if clazz is known to be a constant Class. This will pick up newly found
  // constants after HIR construction. I'll leave this to a future change.

  // as a first cut, make a simple leaf call to runtime to stay platform independent.
  // could follow the aastore example in a future change.

  LIRItem clazz(x->argument_at(0), this);
  LIRItem object(x->argument_at(1), this);
  clazz.load_item();
  object.load_item();
  LIR_Opr result = rlock_result(x);

  // need to perform null check on clazz
  if (x->needs_null_check()) {
    CodeEmitInfo* info = state_for(x);
    __ null_check(clazz.result(), info);
  }

  LIR_Opr call_result = call_runtime(clazz.value(), object.value(), CAST_FROM_FN_PTR(address, Runtime1::is_instance_of), x->type(), NULL); // NULL CodeEmitInfo results in a leaf call
  __ move(call_result, result);
}

// Example: object.getClass ()
void LIRGenerator::do_getClass(Intrinsic* x) {
  LIRItem rcvr(x->argument_at(0), this);
  rcvr.load_item();
  LIR_Opr temp = new_register(T_METADATA);
  LIR_Opr result = rlock_result(x);

  // need to perform the null check on the rcvr
  CodeEmitInfo* info = NULL;
  if (x->needs_null_check()) {
    info = state_for(x);
  }

  // FIXME T_ADDRESS should actually be T_METADATA but it can't because the
  // meaning of these two is mixed up (see JDK-8026837).
  __ move(new LIR_Address(rcvr.result(), oopDesc::klass_offset_in_bytes(), T_ADDRESS), temp, info);
  __ move_wide(new LIR_Address(temp, in_bytes(Klass::java_mirror_offset()), T_ADDRESS), result);
  // mirror = ((OopHandle)mirror)->resolve();
  __ move_wide(new LIR_Address(result, T_OBJECT), result);
}

// java.lang.Class::isPrimitive()
void LIRGenerator::do_isPrimitive(Intrinsic* x) {
  LIRItem rcvr(x->argument_at(0), this);
  rcvr.load_item();
  LIR_Opr temp = new_register(T_METADATA);
  LIR_Opr result = rlock_result(x);

  CodeEmitInfo* info = NULL;
  if (x->needs_null_check()) {
    info = state_for(x);
  }

  __ move(new LIR_Address(rcvr.result(), java_lang_Class::klass_offset_in_bytes(), T_ADDRESS), temp, info);
  __ cmp(lir_cond_notEqual, temp, LIR_OprFact::intConst(0));
  __ cmove(lir_cond_notEqual, LIR_OprFact::intConst(0), LIR_OprFact::intConst(1), result, T_BOOLEAN);
}

// Example: Thread.currentThread()
void LIRGenerator::do_currentThread(Intrinsic* x) {
  LIR_Opr reg = rlock_result(x);
  __ move_wide(new LIR_Address(getThreadPointer(), in_bytes(JavaThread::threadObj_offset()), T_OBJECT), reg);
}

void LIRGenerator::do_RegisterFinalizer(Intrinsic* x) {
  LIRItem receiver(x->argument_at(0), this);

  receiver.load_item();
  BasicTypeList signature;
  signature.append(T_OBJECT); // receiver
  LIR_OprList* args = new LIR_OprList();
  args->append(receiver.result());
  CodeEmitInfo* info = state_for(x, x->state());
  call_runtime(&signature, args,
               CAST_FROM_FN_PTR(address, Runtime1::entry_for(Runtime1::register_finalizer_id)),
               voidType, info);

  set_no_result(x);
}

//------------------------local access--------------------------------------

LIR_Opr LIRGenerator::operand_for_instruction(Instruction* x) {
  if (x->operand()->is_illegal()) {
    Constant* c = x->as_Constant();
    if (c != NULL) {
      x->set_operand(LIR_OprFact::value_type(c->type()));
    } else {
      // allocate a virtual register for this local or phi
      x->set_operand(rlock(x));
      _instruction_for_operand.at_put_grow(x->operand()->vreg_number(), x, NULL);
    }
  }
  return x->operand();
}

Instruction* LIRGenerator::instruction_for_opr(LIR_Opr opr) {
  if (opr->is_virtual()) {
    return instruction_for_vreg(opr->vreg_number());
  }
  return NULL;
}

Instruction* LIRGenerator::instruction_for_vreg(int reg_num) {
  if (reg_num < _instruction_for_operand.length()) {
    return _instruction_for_operand.at(reg_num);
  }
  return NULL;
}

void LIRGenerator::set_vreg_flag(int vreg_num, VregFlag f) {
  if (_vreg_flags.size_in_bits() == 0) {
    BitMap2D temp(100, num_vreg_flags);
    _vreg_flags = temp;
  }
  _vreg_flags.at_put_grow(vreg_num, f, true);
}

bool LIRGenerator::is_vreg_flag_set(int vreg_num, VregFlag f) {
  if (!_vreg_flags.is_valid_index(vreg_num, f)) {
    return false;
  }
  return _vreg_flags.at(vreg_num, f);
}

// Block local constant handling.  This code is useful for keeping
// unpinned constants and constants which aren't exposed in the IR in
// registers.  Unpinned Constant instructions have their operands
// cleared when the block is finished so that other blocks can't end
// up referring to their registers.

LIR_Opr LIRGenerator::load_constant(Constant* x) {
  _unpinned_constants.append(x);
  return load_constant(LIR_OprFact::value_type(x->type())->as_constant_ptr());
}

LIR_Opr LIRGenerator::load_constant(LIR_Const* c) {
  BasicType t = c->type();
  for (int i = 0; i < _constants.length(); i++) {
    LIR_Const* other = _constants.at(i);
    if (t == other->type()) {
      switch (t) {
      case T_INT:
      case T_FLOAT:
        if (c->as_jint_bits() != other->as_jint_bits()) continue;
        break;
      case T_LONG:
      case T_DOUBLE:
        if (c->as_jint_hi_bits() != other->as_jint_hi_bits()) continue;
        if (c->as_jint_lo_bits() != other->as_jint_lo_bits()) continue;
        break;
      case T_OBJECT:
        if (c->as_jobject() != other->as_jobject()) continue;
        break;
      default:
        break;
      }
      return _reg_for_constants.at(i);
    }
  }

  LIR_Opr result = new_register(t);
  __ move((LIR_Opr)c, result);
  _constants.append(c);
  _reg_for_constants.append(result);
  return result;
}

//------------------------field access--------------------------------------

void LIRGenerator::do_CompareAndSwap(Intrinsic* x, ValueType* type) {
  LIRItem obj   (x->argument_at(0), this);  // object
  LIRItem offset(x->argument_at(1), this);  // offset of field
  LIRItem cmp   (x->argument_at(2), this);  // value to compare with field
  LIRItem val   (x->argument_at(3), this);  // replace field with val if matches cmp

  LIR_Opr result = access_atomic_cmpxchg_at(IN_HEAP, as_BasicType(type), obj, offset, cmp, val);
  set_result(x, result);
}

// Comment copied form templateTable_i486.cpp
// ----------------------------------------------------------------------------
// Volatile variables demand their effects be made known to all CPU's in
// order.  Store buffers on most chips allow reads & writes to reorder; the
// JMM's ReadAfterWrite.java test fails in -Xint mode without some kind of
// memory barrier (i.e., it's not sufficient that the interpreter does not
// reorder volatile references, the hardware also must not reorder them).
//
// According to the new Java Memory Model (JMM):
// (1) All volatiles are serialized wrt to each other.
// ALSO reads & writes act as aquire & release, so:
// (2) A read cannot let unrelated NON-volatile memory refs that happen after
// the read float up to before the read.  It's OK for non-volatile memory refs
// that happen before the volatile read to float down below it.
// (3) Similar a volatile write cannot let unrelated NON-volatile memory refs
// that happen BEFORE the write float down to after the write.  It's OK for
// non-volatile memory refs that happen after the volatile write to float up
// before it.
//
// We only put in barriers around volatile refs (they are expensive), not
// _between_ memory refs (that would require us to track the flavor of the
// previous memory refs).  Requirements (2) and (3) require some barriers
// before volatile stores and after volatile loads.  These nearly cover
// requirement (1) but miss the volatile-store-volatile-load case.  This final
// case is placed after volatile-stores although it could just as well go
// before volatile-loads.

void LIRGenerator::do_StoreField(StoreField* x) {
  bool needs_patching = x->needs_patching();
  bool is_volatile = x->field()->is_volatile();
  BasicType field_type = x->field_type();

  CodeEmitInfo* info = NULL;
  if (needs_patching) {
    info = state_for(x, x->state_before());
  } else if (x->needs_null_check()) {
    NullCheck* nc = x->explicit_null_check();
    if (nc == NULL) {
      info = state_for(x);
    } else {
      info = state_for(nc);
    }
  }

  LIRItem object(x->obj(), this);
  LIRItem value(x->value(),  this);

  object.load_item();

  if (is_volatile || needs_patching) {
    // load item if field is volatile (fewer special cases for volatiles)
    // load item if field not initialized
    // load item if field not constant
    // because of code patching we cannot inline constants
    if (field_type == T_BYTE || field_type == T_BOOLEAN) {
      value.load_byte_item();
    } else {
      value.load_item();
    }
  } else {
    value.load_for_store(field_type);
  }

  set_no_result(x);

  if (x->needs_null_check() && (needs_patching || MacroAssembler::needs_explicit_null_check(x->offset()))) {
    // Emit an explicit null check because the offset is too large.
    // If the class is not loaded and the object is NULL, we need to deoptimize to throw a
    // NoClassDefFoundError in the interpreter instead of an implicit NPE from compiled code.
    __ null_check(object.result(), new CodeEmitInfo(info), /* deoptimize */ needs_patching);
  }

  DecoratorSet decorators = IN_HEAP;
  if (is_volatile) {
    decorators |= MO_SEQ_CST;
  }
  if (needs_patching) {
    decorators |= C1_NEEDS_PATCHING;
  }

  access_store_at(decorators, field_type, object, LIR_OprFact::intConst(x->offset()),
                  value.result(), info != NULL ? new CodeEmitInfo(info) : NULL, info);
}

void LIRGenerator::do_StoreIndexed(StoreIndexed* x) {
  bool needs_range_check = x->compute_needs_range_check();
  bool use_length = x->length() != NULL;
  bool obj_store = x->elt_type() == T_ARRAY || x->elt_type() == T_OBJECT;
  bool needs_store_check = obj_store && (x->value()->as_Constant() == NULL || !get_jobject_constant(x->value())->is_null_object());

  LIRItem array(x->array(), this);
  LIRItem index(x->index(), this);
  LIRItem value(x->value(), this);
  LIRItem length(this);

  array.load_item();
  index.load_nonconstant();

  if (use_length && needs_range_check) {
    length.set_instruction(x->length());
    length.load_item();
  }
  if (needs_store_check || x->check_boolean()) {
    value.load_item();
  } else {
    value.load_for_store(x->elt_type());
  }

  set_no_result(x);

  // the CodeEmitInfo must be duplicated for each different
  // LIR-instruction because spilling can occur anywhere between two
  // instructions and so the debug information must be different
  CodeEmitInfo* range_check_info = state_for(x);
  CodeEmitInfo* null_check_info = NULL;
  if (x->needs_null_check()) {
    null_check_info = new CodeEmitInfo(range_check_info);
  }

  if (GenerateRangeChecks && needs_range_check) {
    if (use_length) {
      __ cmp(lir_cond_belowEqual, length.result(), index.result());
      __ branch(lir_cond_belowEqual, T_INT, new RangeCheckStub(range_check_info, index.result(), array.result()));
    } else {
      array_range_check(array.result(), index.result(), null_check_info, range_check_info);
      // range_check also does the null check
      null_check_info = NULL;
    }
  }

  if (GenerateArrayStoreCheck && needs_store_check) {
    CodeEmitInfo* store_check_info = new CodeEmitInfo(range_check_info);
    array_store_check(value.result(), array.result(), store_check_info);
  }

  DecoratorSet decorators = IN_HEAP | IS_ARRAY;
  if (x->check_boolean()) {
    decorators |= C1_MASK_BOOLEAN;
  }

  access_store_at(decorators, x->elt_type(), array, index.result(), value.result(),
                  NULL, null_check_info);
}

void LIRGenerator::access_load_at(DecoratorSet decorators, BasicType type, LIRItem& base, LIR_Opr offset, LIR_Opr result, CodeEmitInfo* patch_info, CodeEmitInfo* load_emit_info) {
  decorators |= C1_READ_ACCESS;
  LIRAccess access(this, decorators, base, offset, type, patch_info, load_emit_info);
  if (access.is_raw()) {
    _barrier_set->BarrierSetC1::load_at(access, result);
  } else {
    _barrier_set->load_at(access, result);
  }
}

void LIRGenerator::access_store_at(DecoratorSet decorators, BasicType type, LIRItem& base, LIR_Opr offset, LIR_Opr value, CodeEmitInfo* patch_info, CodeEmitInfo* store_emit_info) {
  decorators |= C1_WRITE_ACCESS;
  LIRAccess access(this, decorators, base, offset, type, patch_info, store_emit_info);
  if (access.is_raw()) {
    _barrier_set->BarrierSetC1::store_at(access, value);
  } else {
    _barrier_set->store_at(access, value);
  }
}

LIR_Opr LIRGenerator::access_atomic_cmpxchg_at(DecoratorSet decorators, BasicType type, LIRItem& base, LIRItem& offset, LIRItem& cmp_value, LIRItem& new_value) {
  // Atomic operations are SEQ_CST by default
  decorators |= C1_READ_ACCESS;
  decorators |= C1_WRITE_ACCESS;
  decorators |= ((decorators & MO_DECORATOR_MASK) != 0) ? MO_SEQ_CST : 0;
  LIRAccess access(this, decorators, base, offset, type);
  if (access.is_raw()) {
    return _barrier_set->BarrierSetC1::atomic_cmpxchg_at(access, cmp_value, new_value);
  } else {
    return _barrier_set->atomic_cmpxchg_at(access, cmp_value, new_value);
  }
}

LIR_Opr LIRGenerator::access_atomic_xchg_at(DecoratorSet decorators, BasicType type, LIRItem& base, LIRItem& offset, LIRItem& value) {
  // Atomic operations are SEQ_CST by default
  decorators |= C1_READ_ACCESS;
  decorators |= C1_WRITE_ACCESS;
  decorators |= ((decorators & MO_DECORATOR_MASK) != 0) ? MO_SEQ_CST : 0;
  LIRAccess access(this, decorators, base, offset, type);
  if (access.is_raw()) {
    return _barrier_set->BarrierSetC1::atomic_xchg_at(access, value);
  } else {
    return _barrier_set->atomic_xchg_at(access, value);
  }
}

LIR_Opr LIRGenerator::access_atomic_add_at(DecoratorSet decorators, BasicType type, LIRItem& base, LIRItem& offset, LIRItem& value) {
  // Atomic operations are SEQ_CST by default
  decorators |= C1_READ_ACCESS;
  decorators |= C1_WRITE_ACCESS;
  decorators |= ((decorators & MO_DECORATOR_MASK) != 0) ? MO_SEQ_CST : 0;
  LIRAccess access(this, decorators, base, offset, type);
  if (access.is_raw()) {
    return _barrier_set->BarrierSetC1::atomic_add_at(access, value);
  } else {
    return _barrier_set->atomic_add_at(access, value);
  }
}

void LIRGenerator::do_LoadField(LoadField* x) {
  bool needs_patching = x->needs_patching();
  bool is_volatile = x->field()->is_volatile();
  BasicType field_type = x->field_type();

  CodeEmitInfo* info = NULL;
  if (needs_patching) {
    info = state_for(x, x->state_before());
  } else if (x->needs_null_check()) {
    NullCheck* nc = x->explicit_null_check();
    if (nc == NULL) {
      info = state_for(x);
    } else {
      info = state_for(nc);
    }
  }

  LIRItem object(x->obj(), this);

  object.load_item();

  if (x->needs_null_check() && (needs_patching || MacroAssembler::needs_explicit_null_check(x->offset()))) {
    LIR_Opr obj = object.result();
    // Emit an explicit null check because the offset is too large.
    // If the class is not loaded and the object is NULL, we need to deoptimize to throw a
    // NoClassDefFoundError in the interpreter instead of an implicit NPE from compiled code.
    __ null_check(obj, new CodeEmitInfo(info), /* deoptimize */ needs_patching);
  }

  DecoratorSet decorators = IN_HEAP;
  if (is_volatile) {
    decorators |= MO_SEQ_CST;
  }
  if (needs_patching) {
    decorators |= C1_NEEDS_PATCHING;
  }

  LIR_Opr result = rlock_result(x, field_type);
  access_load_at(decorators, field_type, object, LIR_OprFact::intConst(x->offset()), result, info ? new CodeEmitInfo(info) : NULL, info);
}

//------------------------java.nio.Buffer.checkIndex------------------------

// int java.nio.Buffer.checkIndex(int)
void LIRGenerator::do_NIOCheckIndex(Intrinsic* x) {
  // NOTE: by the time we are in checkIndex() we are guaranteed that
  // the buffer is non-null (because checkIndex is package-private and
  // only called from within other methods in the buffer).
  LIRItem buf  (x->argument_at(0), this);
  LIRItem index(x->argument_at(1), this);
  buf.load_item();
  index.load_item();

  LIR_Opr result = rlock_result(x);
  if (GenerateRangeChecks) {
    CodeEmitInfo* info = state_for(x);
    CodeStub* stub = new RangeCheckStub(info, index.result());
    if (index.result()->is_constant()) {
      cmp_mem_int(lir_cond_belowEqual, buf.result(), java_nio_Buffer::limit_offset(), index.result()->as_jint(), info);
      __ branch(lir_cond_belowEqual, T_INT, stub);
    } else {
      cmp_reg_mem(lir_cond_aboveEqual, index.result(), buf.result(),
                  java_nio_Buffer::limit_offset(), T_INT, info);
      __ branch(lir_cond_aboveEqual, T_INT, stub);
    }
    __ move(index.result(), result);
  } else {
    // Just load the index into the result register
    __ move(index.result(), result);
  }
}

//------------------------array access--------------------------------------

void LIRGenerator::do_ArrayLength(ArrayLength* x) {
  LIRItem array(x->array(), this);
  array.load_item();
  LIR_Opr reg = rlock_result(x);

  CodeEmitInfo* info = NULL;
  if (x->needs_null_check()) {
    NullCheck* nc = x->explicit_null_check();
    if (nc == NULL) {
      info = state_for(x);
    } else {
      info = state_for(nc);
    }
  }
  __ load(new LIR_Address(array.result(), arrayOopDesc::length_offset_in_bytes(), T_INT), reg, info, lir_patch_none);
}

void LIRGenerator::do_LoadIndexed(LoadIndexed* x) {
  bool use_length = x->length() != NULL;
  LIRItem array(x->array(), this);
  LIRItem index(x->index(), this);
  LIRItem length(this);
  bool needs_range_check = x->compute_needs_range_check();

  if (use_length && needs_range_check) {
    length.set_instruction(x->length());
    length.load_item();
  }

  array.load_item();
  if (index.is_constant() && can_inline_as_constant(x->index())) {
    // let it be a constant
    index.dont_load_item();
  } else {
    index.load_item();
  }

  CodeEmitInfo* range_check_info = state_for(x);
  CodeEmitInfo* null_check_info = NULL;
  if (x->needs_null_check()) {
    NullCheck* nc = x->explicit_null_check();
    if (nc != NULL) {
      null_check_info = state_for(nc);
    } else {
      null_check_info = range_check_info;
    }
  }

  if (GenerateRangeChecks && needs_range_check) {
    if (use_length) {
      // TODO: use a (modified) version of array_range_check that does not require a
      //       constant length to be loaded to a register
      __ cmp(lir_cond_belowEqual, length.result(), index.result());
      __ branch(lir_cond_belowEqual, T_INT, new RangeCheckStub(range_check_info, index.result(), array.result()));
    } else {
      array_range_check(array.result(), index.result(), null_check_info, range_check_info);
      // The range check performs the null check, so clear it out for the load
      null_check_info = NULL;
    }
  }

  DecoratorSet decorators = IN_HEAP | IS_ARRAY;

  LIR_Opr result = rlock_result(x, x->elt_type());
  access_load_at(decorators, x->elt_type(),
                 array, index.result(), result,
                 NULL, null_check_info);
}

void LIRGenerator::do_NullCheck(NullCheck* x) {
  if (x->can_trap()) {
    LIRItem value(x->obj(), this);
    value.load_item();
    CodeEmitInfo* info = state_for(x);
    __ null_check(value.result(), info);
  }
}

void LIRGenerator::do_TypeCast(TypeCast* x) {
  LIRItem value(x->obj(), this);
  value.load_item();
  // the result is the same as from the node we are casting
  set_result(x, value.result());
}

void LIRGenerator::do_Throw(Throw* x) {
  LIRItem exception(x->exception(), this);
  exception.load_item();
  set_no_result(x);
  LIR_Opr exception_opr = exception.result();
  CodeEmitInfo* info = state_for(x, x->state());

  // check if the instruction has an xhandler in any of the nested scopes
  bool unwind = false;
  if (info->exception_handlers()->length() == 0) {
    // this throw is not inside an xhandler
    unwind = true;
  } else {
    // get some idea of the throw type
    bool type_is_exact = true;
    ciType* throw_type = x->exception()->exact_type();
    if (throw_type == NULL) {
      type_is_exact = false;
      throw_type = x->exception()->declared_type();
    }
    if (throw_type != NULL && throw_type->is_instance_klass()) {
      ciInstanceKlass* throw_klass = (ciInstanceKlass*)throw_type;
      unwind = !x->exception_handlers()->could_catch(throw_klass, type_is_exact);
    }
  }

  // do null check before moving exception oop into fixed register
  // to avoid a fixed interval with an oop during the null check.
  // Use a copy of the CodeEmitInfo because debug information is
  // different for null_check and throw.
  if (x->exception()->as_NewInstance() == NULL && x->exception()->as_ExceptionObject() == NULL) {
    // if the exception object wasn't created using new then it might be null.
    __ null_check(exception_opr, new CodeEmitInfo(info, x->state()->copy(ValueStack::ExceptionState, x->state()->bci())));
  }

  // move exception oop into fixed register
  __ move(exception_opr, exceptionOopOpr());

  if (unwind) {
    __ unwind_exception(exceptionOopOpr());
  } else {
    __ throw_exception(exceptionPcOpr(), exceptionOopOpr(), info);
  }
}

void LIRGenerator::do_SwitchRanges(SwitchRangeArray* x, LIR_Opr value, BlockBegin* default_sux) {
  int lng = x->length();

  for (int i = 0; i < lng; i++) {
    SwitchRange* one_range = x->at(i);
    int low_key = one_range->low_key();
    int high_key = one_range->high_key();
    BlockBegin* dest = one_range->sux();
    if (low_key == high_key) {
      __ cmp(lir_cond_equal, value, low_key);
      __ branch(lir_cond_equal, T_INT, dest);
    } else if (high_key - low_key == 1) {
      __ cmp(lir_cond_equal, value, low_key);
      __ branch(lir_cond_equal, T_INT, dest);
      __ cmp(lir_cond_equal, value, high_key);
      __ branch(lir_cond_equal, T_INT, dest);
    } else {
      LabelObj* L = new LabelObj();
      __ cmp(lir_cond_less, value, low_key);
      __ branch(lir_cond_less, T_INT, L->label());
      __ cmp(lir_cond_lessEqual, value, high_key);
      __ branch(lir_cond_lessEqual, T_INT, dest);
      __ branch_destination(L->label());
    }
  }
  __ jump(default_sux);
}

SwitchRangeArray* LIRGenerator::create_lookup_ranges(TableSwitch* x) {
  SwitchRangeList* res = new SwitchRangeList();
  int len = x->length();
  if (len > 0) {
    BlockBegin* sux = x->sux_at(0);
    int key = x->lo_key();
    BlockBegin* default_sux = x->default_sux();
    SwitchRange* range = new SwitchRange(key, sux);
    for (int i = 0; i < len; i++, key++) {
      BlockBegin* new_sux = x->sux_at(i);
      if (sux == new_sux) {
        // still in same range
        range->set_high_key(key);
      } else {
        // skip tests which explicitly dispatch to the default
        if (sux != default_sux) {
          res->append(range);
        }
        range = new SwitchRange(key, new_sux);
      }
      sux = new_sux;
    }
    if (res->length() == 0 || res->last() != range)  res->append(range);
  }
  return res;
}

// we expect the keys to be sorted by increasing value
SwitchRangeArray* LIRGenerator::create_lookup_ranges(LookupSwitch* x) {
  SwitchRangeList* res = new SwitchRangeList();
  int len = x->length();
  if (len > 0) {
    BlockBegin* default_sux = x->default_sux();
    int key = x->key_at(0);
    BlockBegin* sux = x->sux_at(0);
    SwitchRange* range = new SwitchRange(key, sux);
    for (int i = 1; i < len; i++) {
      int new_key = x->key_at(i);
      BlockBegin* new_sux = x->sux_at(i);
      if (key + 1 == new_key && sux == new_sux) {
        // still in same range
        range->set_high_key(new_key);
      } else {
        // skip tests which explicitly dispatch to the default
        if (range->sux() != default_sux) {
          res->append(range);
        }
        range = new SwitchRange(new_key, new_sux);
      }
      key = new_key;
      sux = new_sux;
    }
    if (res->length() == 0 || res->last() != range)  res->append(range);
  }
  return res;
}

void LIRGenerator::do_TableSwitch(TableSwitch* x) {
  LIRItem tag(x->tag(), this);
  tag.load_item();
  set_no_result(x);

  if (x->is_safepoint()) {
    __ safepoint(safepoint_poll_register(), state_for(x, x->state_before()));
  }

  // move values into phi locations
  move_to_phi(x->state());

  int lo_key = x->lo_key();
  int len = x->length();
  LIR_Opr value = tag.result();

  if (UseTableRanges) {
    do_SwitchRanges(create_lookup_ranges(x), value, x->default_sux());
  } else {
    for (int i = 0; i < len; i++) {
      __ cmp(lir_cond_equal, value, i + lo_key);
      __ branch(lir_cond_equal, T_INT, x->sux_at(i));
    }
    __ jump(x->default_sux());
  }
}

void LIRGenerator::do_LookupSwitch(LookupSwitch* x) {
  LIRItem tag(x->tag(), this);
  tag.load_item();
  set_no_result(x);

  if (x->is_safepoint()) {
    __ safepoint(safepoint_poll_register(), state_for(x, x->state_before()));
  }

  // move values into phi locations
  move_to_phi(x->state());

  LIR_Opr value = tag.result();
  int len = x->length();

  if (UseTableRanges) {
    do_SwitchRanges(create_lookup_ranges(x), value, x->default_sux());
  } else {
    int len = x->length();
    for (int i = 0; i < len; i++) {
      __ cmp(lir_cond_equal, value, x->key_at(i));
      __ branch(lir_cond_equal, T_INT, x->sux_at(i));
    }
    __ jump(x->default_sux());
  }
}

void LIRGenerator::do_Goto(Goto* x) {
  set_no_result(x);

  if (x->is_safepoint()) {
    ValueStack* state = x->state_before() ? x->state_before() : x->state();

    __ safepoint(safepoint_poll_register(), state_for(x, state));
  }

  // emit phi-instruction move after safepoint since this simplifies
  // describing the state as the safepoint.
  move_to_phi(x->state());

  __ jump(x->default_sux());
}

void LIRGenerator::do_Base(Base* x) {
  __ std_entry(LIR_OprFact::illegalOpr);
  // Emit moves from physical registers / stack slots to virtual registers
  CallingConvention* args = compilation()->frame_map()->incoming_arguments();
  IRScope* irScope = compilation()->hir()->top_scope();
  int java_index = 0;
  for (int i = 0; i < args->length(); i++) {
    LIR_Opr src = args->at(i);
    BasicType t = src->type();

    // Types which are smaller than int are passed as int, so
    // correct the type which passed.
    switch (t) {
    case T_BYTE:
    case T_BOOLEAN:
    case T_SHORT:
    case T_CHAR:
      t = T_INT;
      break;
    default:
      break;
    }

    LIR_Opr dest = new_register(t);
    __ move(src, dest);

    // Assign new location to Local instruction for this local
    Local* local = x->state()->local_at(java_index)->as_Local();
    local->set_operand(dest);
    _instruction_for_operand.at_put_grow(dest->vreg_number(), local, NULL);
    java_index += type2size[t];
  }

  if (method()->is_synchronized()) {
    LIR_Opr obj;
    if (method()->is_static()) {
      obj = new_register(T_OBJECT);
      __ oop2reg(method()->holder()->java_mirror()->constant_encoding(), obj);
    } else {
      Local* receiver = x->state()->local_at(0)->as_Local();
      obj = receiver->operand();
    }

    if (method()->is_synchronized() && GenerateSynchronizationCode) {
      LIR_Opr lock = syncLockOpr();
      __ load_stack_address_monitor(0, lock);

      CodeEmitInfo* info = new CodeEmitInfo(scope()->start()->state()->copy(ValueStack::StateBefore, SynchronizationEntryBCI), NULL, x->check_flag(Instruction::DeoptimizeOnException));
      CodeStub* slow_path = new MonitorEnterStub(obj, lock, info);

      // receiver is guaranteed non-NULL so don't need CodeEmitInfo
      __ lock_object(syncTempOpr(), obj, lock, new_register(T_OBJECT), slow_path, NULL);
    }
  }

  // all blocks with a successor must end with an unconditional jump
  // to the successor even if they are consecutive
  __ jump(x->default_sux());
}

void LIRGenerator::invoke_load_arguments(Invoke* x, LIRItemList* args, const LIR_OprList* arg_list) {
  for (int i = x->has_receiver() ? 1 : 0; i < args->length(); i++) {
    LIRItem* param = args->at(i);
    LIR_Opr loc = arg_list->at(i);
    if (loc->is_register()) {
      param->load_item_force(loc);
    } else {
      LIR_Address* addr = loc->as_address_ptr();
      param->load_for_store(addr->type());
      if (addr->type() == T_OBJECT) {
        __ move_wide(param->result(), addr);
      } else
        if (addr->type() == T_LONG || addr->type() == T_DOUBLE) {
          __ unaligned_move(param->result(), addr);
        } else {
          __ move(param->result(), addr);
        }
    }
  }

  if (x->has_receiver()) {
    LIRItem* receiver = args->at(0);
    LIR_Opr loc = arg_list->at(0);
    if (loc->is_register()) {
      receiver->load_item_force(loc);
    } else {
      receiver->load_for_store(T_OBJECT);
      __ move_wide(receiver->result(), loc->as_address_ptr());
    }
  }
}

// Visits all arguments, returns appropriate items without loading them
LIRItemList* LIRGenerator::invoke_visit_arguments(Invoke* x) {
  LIRItemList* argument_items = new LIRItemList();
  if (x->has_receiver()) {
    LIRItem* receiver = new LIRItem(x->receiver(), this);
    argument_items->append(receiver);
  }
  for (int i = 0; i < x->number_of_arguments(); i++) {
    LIRItem* param = new LIRItem(x->argument_at(i), this);
    argument_items->append(param);
  }
  return argument_items;
}

// The invoke with receiver has following phases:
//   a) traverse and load/lock receiver;
//   b) traverse all arguments -> item-array (invoke_visit_argument)
//   c) push receiver on stack
//   d) load each of the items and push on stack
//   e) unlock receiver
//   f) move receiver into receiver-register %o0
//   g) lock result registers and emit call operation
//
// Before issuing a call, we must spill-save all values on stack
// that are in caller-save register. "spill-save" moves those registers
// either in a free callee-save register or spills them if no free
// callee save register is available.
//
// The problem is where to invoke spill-save.
// - if invoked between e) and f), we may lock callee save
//   register in "spill-save" that destroys the receiver register
//   before f) is executed
// - if we rearrange f) to be earlier (by loading %o0) it
//   may destroy a value on the stack that is currently in %o0
//   and is waiting to be spilled
// - if we keep the receiver locked while doing spill-save,
//   we cannot spill it as it is spill-locked
//
void LIRGenerator::do_Invoke(Invoke* x) {
  CallingConvention* cc = frame_map()->java_calling_convention(x->signature(), true);

  LIR_OprList* arg_list = cc->args();
  LIRItemList* args = invoke_visit_arguments(x);
  LIR_Opr receiver = LIR_OprFact::illegalOpr;

  // setup result register
  LIR_Opr result_register = LIR_OprFact::illegalOpr;
  if (x->type() != voidType) {
    result_register = result_register_for(x->type());
  }

  CodeEmitInfo* info = state_for(x, x->state());

  invoke_load_arguments(x, args, arg_list);

  if (x->has_receiver()) {
    args->at(0)->load_item_force(LIR_Assembler::receiverOpr());
    receiver = args->at(0)->result();
  }

  // JSR 292
  // Preserve the SP over MethodHandle call sites, if needed.
  ciMethod* target = x->target();

  switch (x->code()) {
    case Bytecodes::_invokestatic:
      __ call_static(target, result_register, SharedRuntime::get_resolve_static_call_stub(), arg_list, info);
      break;
    case Bytecodes::_invokespecial:
    case Bytecodes::_invokevirtual:
    case Bytecodes::_invokeinterface:
      // for loaded and final (method or class) target we still produce an inline cache,
      // in order to be able to call mixed mode
      if (x->code() == Bytecodes::_invokespecial || x->target_is_final()) {
        __ call_opt_virtual(target, receiver, result_register, SharedRuntime::get_resolve_opt_virtual_call_stub(), arg_list, info);
      } else if (x->vtable_index() < 0) {
        __ call_icvirtual(target, receiver, result_register, SharedRuntime::get_resolve_virtual_call_stub(), arg_list, info);
      } else {
        int entry_offset = in_bytes(Klass::vtable_start_offset()) + x->vtable_index() * vtableEntry::size_in_bytes();
        int vtable_offset = entry_offset + vtableEntry::method_offset_in_bytes();
        __ call_virtual(target, receiver, result_register, vtable_offset, arg_list, info);
      }
      break;
    default:
      fatal("unexpected bytecode: %s", Bytecodes::name(x->code()));
      break;
  }

  if (x->type()->is_float() || x->type()->is_double()) {
    // Force rounding of results from non-strictfp when in strictfp
    // scope (or when we don't know the strictness of the callee, to
    // be safe.)
    if (method()->is_strict()) {
      if (!x->target_is_loaded() || !x->target_is_strictfp()) {
        result_register = round_item(result_register);
      }
    }
  }

  if (result_register->is_valid()) {
    LIR_Opr result = rlock_result(x);
    __ move(result_register, result);
  }
}

void LIRGenerator::do_FPIntrinsics(Intrinsic* x) {
  LIRItem value       (x->argument_at(0), this);
  LIR_Opr reg = rlock_result(x);
  value.load_item();
  LIR_Opr tmp = force_to_spill(value.result(), as_BasicType(x->type()));
  __ move(tmp, reg);
}

// Code for  :  x->x() {x->cond()} x->y() ? x->tval() : x->fval()
void LIRGenerator::do_IfOp(IfOp* x) {
  LIRItem left(x->x(), this);
  LIRItem right(x->y(), this);
  left.load_item();
  if (can_inline_as_constant(right.value())) {
    right.dont_load_item();
  } else {
    right.load_item();
  }

  LIRItem t_val(x->tval(), this);
  LIRItem f_val(x->fval(), this);
  t_val.dont_load_item();
  f_val.dont_load_item();
  LIR_Opr reg = rlock_result(x);

  __ cmp(lir_cond(x->cond()), left.result(), right.result());
  __ cmove(lir_cond(x->cond()), t_val.result(), f_val.result(), reg, as_BasicType(x->x()->type()));
}

LIR_Opr LIRGenerator::call_runtime(Value arg1, address entry, ValueType* result_type, CodeEmitInfo* info) {
  LIRItemList args(1);
  LIRItem value(arg1, this);
  args.append(&value);
  BasicTypeList signature;
  signature.append(as_BasicType(arg1->type()));

  return call_runtime(&signature, &args, entry, result_type, info);
}

LIR_Opr LIRGenerator::call_runtime(Value arg1, Value arg2, address entry, ValueType* result_type, CodeEmitInfo* info) {
  LIRItemList args(2);
  LIRItem value1(arg1, this);
  LIRItem value2(arg2, this);
  args.append(&value1);
  args.append(&value2);
  BasicTypeList signature;
  signature.append(as_BasicType(arg1->type()));
  signature.append(as_BasicType(arg2->type()));

  return call_runtime(&signature, &args, entry, result_type, info);
}

LIR_Opr LIRGenerator::call_runtime(BasicTypeArray* signature, LIR_OprList* args, address entry, ValueType* result_type, CodeEmitInfo* info) {
  // get a result register
  LIR_Opr phys_reg = LIR_OprFact::illegalOpr;
  LIR_Opr result = LIR_OprFact::illegalOpr;
  if (result_type->tag() != voidTag) {
    result = new_register(result_type);
    phys_reg = result_register_for(result_type);
  }

  // move the arguments into the correct location
  CallingConvention* cc = frame_map()->c_calling_convention(signature);
  for (int i = 0; i < args->length(); i++) {
    LIR_Opr arg = args->at(i);
    LIR_Opr loc = cc->at(i);
    if (loc->is_register()) {
      __ move(arg, loc);
    } else {
      LIR_Address* addr = loc->as_address_ptr();
      if (addr->type() == T_LONG || addr->type() == T_DOUBLE) {
        __ unaligned_move(arg, addr);
      } else {
        __ move(arg, addr);
      }
    }
  }

  if (info) {
    __ call_runtime(entry, getThreadTemp(), phys_reg, cc->args(), info);
  } else {
    __ call_runtime_leaf(entry, getThreadTemp(), phys_reg, cc->args());
  }
  if (result->is_valid()) {
    __ move(phys_reg, result);
  }
  return result;
}

LIR_Opr LIRGenerator::call_runtime(BasicTypeArray* signature, LIRItemList* args, address entry, ValueType* result_type, CodeEmitInfo* info) {
  // get a result register
  LIR_Opr phys_reg = LIR_OprFact::illegalOpr;
  LIR_Opr result = LIR_OprFact::illegalOpr;
  if (result_type->tag() != voidTag) {
    result = new_register(result_type);
    phys_reg = result_register_for(result_type);
  }

  // move the arguments into the correct location
  CallingConvention* cc = frame_map()->c_calling_convention(signature);

  for (int i = 0; i < args->length(); i++) {
    LIRItem* arg = args->at(i);
    LIR_Opr loc = cc->at(i);
    if (loc->is_register()) {
      arg->load_item_force(loc);
    } else {
      LIR_Address* addr = loc->as_address_ptr();
      arg->load_for_store(addr->type());
      if (addr->type() == T_LONG || addr->type() == T_DOUBLE) {
        __ unaligned_move(arg->result(), addr);
      } else {
        __ move(arg->result(), addr);
      }
    }
  }

  if (info) {
    __ call_runtime(entry, getThreadTemp(), phys_reg, cc->args(), info);
  } else {
    __ call_runtime_leaf(entry, getThreadTemp(), phys_reg, cc->args());
  }
  if (result->is_valid()) {
    __ move(phys_reg, result);
  }
  return result;
}

LIR_Opr LIRGenerator::mask_boolean(LIR_Opr array, LIR_Opr value, CodeEmitInfo*& null_check_info) {
  LIR_Opr value_fixed = rlock_byte(T_BYTE);
  if (TwoOperandLIRForm) {
    __ move(value, value_fixed);
    __ logical_and(value_fixed, LIR_OprFact::intConst(1), value_fixed);
  } else {
    __ logical_and(value, LIR_OprFact::intConst(1), value_fixed);
  }
  LIR_Opr klass = new_register(T_METADATA);
  __ move(new LIR_Address(array, oopDesc::klass_offset_in_bytes(), T_ADDRESS), klass, null_check_info);
  null_check_info = NULL;
  LIR_Opr layout = new_register(T_INT);
  __ move(new LIR_Address(klass, in_bytes(Klass::layout_helper_offset()), T_INT), layout);
  int diffbit = Klass::layout_helper_boolean_diffbit();
  __ logical_and(layout, LIR_OprFact::intConst(diffbit), layout);
  __ cmp(lir_cond_notEqual, layout, LIR_OprFact::intConst(0));
  __ cmove(lir_cond_notEqual, value_fixed, value, value_fixed, T_BYTE);
  value = value_fixed;
  return value;
}

LIR_Opr LIRGenerator::maybe_mask_boolean(StoreIndexed* x, LIR_Opr array, LIR_Opr value, CodeEmitInfo*& null_check_info) {
  if (x->check_boolean()) {
    value = mask_boolean(array, value, null_check_info);
  }
  return value;
}
