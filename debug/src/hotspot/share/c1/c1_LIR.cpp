#include "precompiled.hpp"

#include "c1/c1_LIR.hpp"
#include "c1/c1_LIRAssembler.hpp"
#include "c1/c1_ValueStack.hpp"
#include "ci/ciInstance.hpp"
#include "runtime/sharedRuntime.hpp"

Register LIR_OprDesc::as_register() const {
  return FrameMap::cpu_rnr2reg(cpu_regnr());
}

Register LIR_OprDesc::as_register_lo() const {
  return FrameMap::cpu_rnr2reg(cpu_regnrLo());
}

Register LIR_OprDesc::as_register_hi() const {
  return FrameMap::cpu_rnr2reg(cpu_regnrHi());
}

LIR_Opr LIR_OprFact::illegalOpr = LIR_OprFact::illegal();

LIR_Opr LIR_OprFact::value_type(ValueType* type) {
  ValueTag tag = type->tag();
  switch (tag) {
  case metaDataTag : {
    ClassConstant* c = type->as_ClassConstant();
    if (c != NULL && !c->value()->is_loaded()) {
      return LIR_OprFact::metadataConst(NULL);
    } else if (c != NULL) {
      return LIR_OprFact::metadataConst(c->value()->constant_encoding());
    } else {
      MethodConstant* m = type->as_MethodConstant();
      return LIR_OprFact::metadataConst(m->value()->constant_encoding());
    }
  }
  case objectTag : {
      return LIR_OprFact::oopConst(type->as_ObjectType()->encoding());
    }
  case addressTag: return LIR_OprFact::addressConst(type->as_AddressConstant()->value());
  case intTag    : return LIR_OprFact::intConst(type->as_IntConstant()->value());
  case floatTag  : return LIR_OprFact::floatConst(type->as_FloatConstant()->value());
  case longTag   : return LIR_OprFact::longConst(type->as_LongConstant()->value());
  case doubleTag : return LIR_OprFact::doubleConst(type->as_DoubleConstant()->value());
  default: ShouldNotReachHere(); return LIR_OprFact::intConst(-1);
  }
}

LIR_Opr LIR_OprFact::dummy_value_type(ValueType* type) {
  switch (type->tag()) {
    case objectTag: return LIR_OprFact::oopConst(NULL);
    case addressTag:return LIR_OprFact::addressConst(0);
    case intTag:    return LIR_OprFact::intConst(0);
    case floatTag:  return LIR_OprFact::floatConst(0.0);
    case longTag:   return LIR_OprFact::longConst(0);
    case doubleTag: return LIR_OprFact::doubleConst(0.0);
    default:        ShouldNotReachHere(); return LIR_OprFact::intConst(-1);
  }
  return illegalOpr;
}

//---------------------------------------------------

LIR_Address::Scale LIR_Address::scale(BasicType type) {
  int elem_size = type2aelembytes(type);
  switch (elem_size) {
  case 1: return LIR_Address::times_1;
  case 2: return LIR_Address::times_2;
  case 4: return LIR_Address::times_4;
  case 8: return LIR_Address::times_8;
  }
  ShouldNotReachHere();
  return LIR_Address::times_1;
}

//---------------------------------------------------

char LIR_OprDesc::type_char(BasicType t) {
  switch (t) {
    case T_ARRAY:
      t = T_OBJECT;
    case T_BOOLEAN:
    case T_CHAR:
    case T_FLOAT:
    case T_DOUBLE:
    case T_BYTE:
    case T_SHORT:
    case T_INT:
    case T_LONG:
    case T_OBJECT:
    case T_ADDRESS:
    case T_VOID:
      return ::type2char(t);
    case T_METADATA:
      return 'M';
    case T_ILLEGAL:
      return '?';

    default:
      ShouldNotReachHere();
      return '?';
  }
}

bool LIR_OprDesc::is_oop() const {
  if (is_pointer()) {
    return pointer()->is_oop_pointer();
  } else {
    OprType t= type_field();
    return t == object_type;
  }
}

LIR_OpBranch::LIR_OpBranch(LIR_Condition cond, BasicType type, BlockBegin* block)
  : LIR_Op(lir_branch, LIR_OprFact::illegalOpr, (CodeEmitInfo*)NULL)
  , _cond(cond)
  , _type(type)
  , _label(block->label())
  , _block(block)
  , _ublock(NULL)
  , _stub(NULL) {
}

LIR_OpBranch::LIR_OpBranch(LIR_Condition cond, BasicType type, CodeStub* stub) :
  LIR_Op(lir_branch, LIR_OprFact::illegalOpr, (CodeEmitInfo*)NULL)
  , _cond(cond)
  , _type(type)
  , _label(stub->entry())
  , _block(NULL)
  , _ublock(NULL)
  , _stub(stub) {
}

LIR_OpBranch::LIR_OpBranch(LIR_Condition cond, BasicType type, BlockBegin* block, BlockBegin* ublock)
  : LIR_Op(lir_cond_float_branch, LIR_OprFact::illegalOpr, (CodeEmitInfo*)NULL)
  , _cond(cond)
  , _type(type)
  , _label(block->label())
  , _block(block)
  , _ublock(ublock)
  , _stub(NULL)
{ }

void LIR_OpBranch::change_block(BlockBegin* b) {
  _block = b;
  _label = b->label();
}

void LIR_OpBranch::change_ublock(BlockBegin* b) {
  _ublock = b;
}

void LIR_OpBranch::negate_cond() {
  switch (_cond) {
    case lir_cond_equal:        _cond = lir_cond_notEqual;     break;
    case lir_cond_notEqual:     _cond = lir_cond_equal;        break;
    case lir_cond_less:         _cond = lir_cond_greaterEqual; break;
    case lir_cond_lessEqual:    _cond = lir_cond_greater;      break;
    case lir_cond_greaterEqual: _cond = lir_cond_less;         break;
    case lir_cond_greater:      _cond = lir_cond_lessEqual;    break;
    default: ShouldNotReachHere();
  }
}

LIR_OpTypeCheck::LIR_OpTypeCheck(LIR_Code code, LIR_Opr result, LIR_Opr object, ciKlass* klass, LIR_Opr tmp1, LIR_Opr tmp2, LIR_Opr tmp3,
                                 bool fast_check, CodeEmitInfo* info_for_exception, CodeEmitInfo* info_for_patch, CodeStub* stub)

  : LIR_Op(code, result, NULL)
  , _object(object)
  , _array(LIR_OprFact::illegalOpr)
  , _klass(klass)
  , _tmp1(tmp1)
  , _tmp2(tmp2)
  , _tmp3(tmp3)
  , _fast_check(fast_check)
  , _stub(stub)
  , _info_for_patch(info_for_patch)
  , _info_for_exception(info_for_exception)
  , _profiled_method(NULL)
  , _profiled_bci(-1)
  , _should_profile(false)
{
  if (code == lir_checkcast) {
  } else if (code == lir_instanceof) {
  } else {
    ShouldNotReachHere();
  }
}

LIR_OpTypeCheck::LIR_OpTypeCheck(LIR_Code code, LIR_Opr object, LIR_Opr array, LIR_Opr tmp1, LIR_Opr tmp2, LIR_Opr tmp3, CodeEmitInfo* info_for_exception)
  : LIR_Op(code, LIR_OprFact::illegalOpr, NULL)
  , _object(object)
  , _array(array)
  , _klass(NULL)
  , _tmp1(tmp1)
  , _tmp2(tmp2)
  , _tmp3(tmp3)
  , _fast_check(false)
  , _stub(NULL)
  , _info_for_patch(NULL)
  , _info_for_exception(info_for_exception)
  , _profiled_method(NULL)
  , _profiled_bci(-1)
  , _should_profile(false)
{
  if (code == lir_store_check) {
    _stub = new ArrayStoreExceptionStub(object, info_for_exception);
  } else {
    ShouldNotReachHere();
  }
}

LIR_OpArrayCopy::LIR_OpArrayCopy(LIR_Opr src, LIR_Opr src_pos, LIR_Opr dst, LIR_Opr dst_pos, LIR_Opr length,
                                 LIR_Opr tmp, ciArrayKlass* expected_type, int flags, CodeEmitInfo* info)
  : LIR_Op(lir_arraycopy, LIR_OprFact::illegalOpr, info)
  , _tmp(tmp)
  , _src(src)
  , _src_pos(src_pos)
  , _dst(dst)
  , _dst_pos(dst_pos)
  , _flags(flags)
  , _expected_type(expected_type)
  , _length(length) {
  _stub = new ArrayCopyStub(this);
}

LIR_OpUpdateCRC32::LIR_OpUpdateCRC32(LIR_Opr crc, LIR_Opr val, LIR_Opr res)
  : LIR_Op(lir_updatecrc32, res, NULL)
  , _crc(crc)
  , _val(val) {
}

//-------------------visits--------------------------

// complete rework of LIR instruction visitor.
// The virtual call for each instruction type is replaced by a big
// switch that adds the operands for each instruction

void LIR_OpVisitState::visit(LIR_Op* op) {
  // copy information from the LIR_Op
  reset();
  set_op(op);

  switch (op->code()) {
// LIR_Op0
    case lir_word_align:               // result and info always invalid
    case lir_backwardbranch_target:    // result and info always invalid
    case lir_build_frame:              // result and info always invalid
    case lir_fpop_raw:                 // result and info always invalid
    case lir_24bit_FPU:                // result and info always invalid
    case lir_reset_FPU:                // result and info always invalid
    case lir_breakpoint:               // result and info always invalid
    case lir_membar:                   // result and info always invalid
    case lir_membar_acquire:           // result and info always invalid
    case lir_membar_release:           // result and info always invalid
    case lir_membar_loadload:          // result and info always invalid
    case lir_membar_storestore:        // result and info always invalid
    case lir_membar_loadstore:         // result and info always invalid
    case lir_membar_storeload:         // result and info always invalid
    case lir_on_spin_wait:
    {
      break;
    }

    case lir_nop:                      // may have info, result always invalid
    case lir_std_entry:                // may have result, info always invalid
    case lir_get_thread:               // may have result, info always invalid
    {
      if (op->_info != NULL)           do_info(op->_info);
      if (op->_result->is_valid())     do_output(op->_result);
      break;
    }

// LIR_OpLabel
    case lir_label:                    // result and info always invalid
    {
      break;
    }

// LIR_Op1
    case lir_fxch:           // input always valid, result and info always invalid
    case lir_fld:            // input always valid, result and info always invalid
    case lir_ffree:          // input always valid, result and info always invalid
    case lir_push:           // input always valid, result and info always invalid
    case lir_pop:            // input always valid, result and info always invalid
    case lir_return:         // input always valid, result and info always invalid
    case lir_leal:           // input and result always valid, info always invalid
    case lir_monaddr:        // input and result always valid, info always invalid
    case lir_null_check:     // input and info always valid, result always invalid
    case lir_move:           // input and result always valid, may have info
    case lir_pack64:         // input and result always valid
    case lir_unpack64:       // input and result always valid
    {
      LIR_Op1* op1 = (LIR_Op1*)op;

      if (op1->_info)                  do_info(op1->_info);
      if (op1->_opr->is_valid())       do_input(op1->_opr);
      if (op1->_result->is_valid())    do_output(op1->_result);

      break;
    }

    case lir_safepoint:
    {
      LIR_Op1* op1 = (LIR_Op1*)op;

      do_info(op1->_info);
      if (op1->_opr->is_valid())       do_temp(op1->_opr); // safepoints on SPARC need temporary register

      break;
    }

// LIR_OpConvert;
    case lir_convert:        // input and result always valid, info always invalid
    {
      LIR_OpConvert* opConvert = (LIR_OpConvert*)op;

      if (opConvert->_opr->is_valid())       do_input(opConvert->_opr);
      if (opConvert->_result->is_valid())    do_output(opConvert->_result);
      do_stub(opConvert->_stub);

      break;
    }

// LIR_OpBranch;
    case lir_branch:                   // may have info, input and result register always invalid
    case lir_cond_float_branch:        // may have info, input and result register always invalid
    {
      LIR_OpBranch* opBranch = (LIR_OpBranch*)op;

      if (opBranch->_info != NULL)     do_info(opBranch->_info);
      if (opBranch->_stub != NULL)     opBranch->stub()->visit(this);

      break;
    }

// LIR_OpAllocObj
    case lir_alloc_object:
    {
      LIR_OpAllocObj* opAllocObj = (LIR_OpAllocObj*)op;

      if (opAllocObj->_info)                     do_info(opAllocObj->_info);
      if (opAllocObj->_opr->is_valid()) {        do_input(opAllocObj->_opr);
                                                 do_temp(opAllocObj->_opr);
                                        }
      if (opAllocObj->_tmp1->is_valid())         do_temp(opAllocObj->_tmp1);
      if (opAllocObj->_tmp2->is_valid())         do_temp(opAllocObj->_tmp2);
      if (opAllocObj->_tmp3->is_valid())         do_temp(opAllocObj->_tmp3);
      if (opAllocObj->_tmp4->is_valid())         do_temp(opAllocObj->_tmp4);
      if (opAllocObj->_result->is_valid())       do_output(opAllocObj->_result);
                                                 do_stub(opAllocObj->_stub);
      break;
    }

// LIR_OpRoundFP;
    case lir_roundfp: {
      LIR_OpRoundFP* opRoundFP = (LIR_OpRoundFP*)op;

      do_input(opRoundFP->_opr);
      do_output(opRoundFP->_result);

      break;
    }

// LIR_Op2
    case lir_cmp:
    case lir_cmp_l2i:
    case lir_ucmp_fd2i:
    case lir_cmp_fd2i:
    case lir_add:
    case lir_sub:
    case lir_mul:
    case lir_div:
    case lir_rem:
    case lir_sqrt:
    case lir_abs:
    case lir_neg:
    case lir_logic_and:
    case lir_logic_or:
    case lir_logic_xor:
    case lir_shl:
    case lir_shr:
    case lir_ushr:
    case lir_xadd:
    case lir_xchg:
    case lir_assert:
    {
      LIR_Op2* op2 = (LIR_Op2*)op;

      if (op2->_info)                     do_info(op2->_info);
      if (op2->_opr1->is_valid())         do_input(op2->_opr1);
      if (op2->_opr2->is_valid())         do_input(op2->_opr2);
      if (op2->_tmp1->is_valid())         do_temp(op2->_tmp1);
      if (op2->_result->is_valid())       do_output(op2->_result);
      if (op->code() == lir_xchg || op->code() == lir_xadd) {
        // on ARM and PPC, return value is loaded first so could
        // destroy inputs. On other platforms that implement those
        // (x86, sparc), the extra constrainsts are harmless.
        if (op2->_opr1->is_valid())       do_temp(op2->_opr1);
        if (op2->_opr2->is_valid())       do_temp(op2->_opr2);
      }

      break;
    }

    // special handling for cmove: right input operand must not be equal
    // to the result operand, otherwise the backend fails
    case lir_cmove:
    {
      LIR_Op2* op2 = (LIR_Op2*)op;

      do_input(op2->_opr1);
      do_input(op2->_opr2);
      do_temp(op2->_opr2);
      do_output(op2->_result);

      break;
    }

    // vspecial handling for strict operations: register input operands
    // as temp to guarantee that they do not overlap with other
    // registers
    case lir_mul_strictfp:
    case lir_div_strictfp:
    {
      LIR_Op2* op2 = (LIR_Op2*)op;

      do_input(op2->_opr1); do_temp(op2->_opr1);
      do_input(op2->_opr2); do_temp(op2->_opr2);
      if (op2->_tmp1->is_valid()) do_temp(op2->_tmp1);
      do_output(op2->_result);

      break;
    }

    case lir_throw: {
      LIR_Op2* op2 = (LIR_Op2*)op;

      if (op2->_info)                     do_info(op2->_info);
      if (op2->_opr1->is_valid())         do_temp(op2->_opr1);
      if (op2->_opr2->is_valid())         do_input(op2->_opr2); // exception object is input parameter

      break;
    }

    case lir_unwind: {
      LIR_Op1* op1 = (LIR_Op1*)op;

      do_input(op1->_opr);

      break;
    }

// LIR_Op3
    case lir_idiv:
    case lir_irem: {
      LIR_Op3* op3= (LIR_Op3*)op;

      if (op3->_info)                     do_info(op3->_info);
      if (op3->_opr1->is_valid())         do_input(op3->_opr1);

      // second operand is input and temp, so ensure that second operand
      // and third operand get not the same register
      if (op3->_opr2->is_valid())         do_input(op3->_opr2);
      if (op3->_opr2->is_valid())         do_temp(op3->_opr2);
      if (op3->_opr3->is_valid())         do_temp(op3->_opr3);

      if (op3->_result->is_valid())       do_output(op3->_result);

      break;
    }

    case lir_fmad:
    case lir_fmaf: {
      LIR_Op3* op3= (LIR_Op3*)op;
      do_input(op3->_opr1);
      do_input(op3->_opr2);
      do_input(op3->_opr3);
      do_output(op3->_result);
      break;
    }

// LIR_OpJavaCall
    case lir_static_call:
    case lir_optvirtual_call:
    case lir_icvirtual_call:
    case lir_virtual_call:
    case lir_dynamic_call: {
      LIR_OpJavaCall* opJavaCall = op->as_OpJavaCall();

      if (opJavaCall->_receiver->is_valid())     do_input(opJavaCall->_receiver);

      // only visit register parameters
      int n = opJavaCall->_arguments->length();
      for (int i = opJavaCall->_receiver->is_valid() ? 1 : 0; i < n; i++) {
        if (!opJavaCall->_arguments->at(i)->is_pointer()) {
          do_input(*opJavaCall->_arguments->adr_at(i));
        }
      }

      if (opJavaCall->_info)                     do_info(opJavaCall->_info);
      do_call();
      if (opJavaCall->_result->is_valid())       do_output(opJavaCall->_result);

      break;
    }

// LIR_OpRTCall
    case lir_rtcall: {
      LIR_OpRTCall* opRTCall = (LIR_OpRTCall*)op;

      // only visit register parameters
      int n = opRTCall->_arguments->length();
      for (int i = 0; i < n; i++) {
        if (!opRTCall->_arguments->at(i)->is_pointer()) {
          do_input(*opRTCall->_arguments->adr_at(i));
        }
      }
      if (opRTCall->_info)                     do_info(opRTCall->_info);
      if (opRTCall->_tmp->is_valid())          do_temp(opRTCall->_tmp);
      do_call();
      if (opRTCall->_result->is_valid())       do_output(opRTCall->_result);

      break;
    }

// LIR_OpArrayCopy
    case lir_arraycopy: {
      LIR_OpArrayCopy* opArrayCopy = (LIR_OpArrayCopy*)op;

      do_input(opArrayCopy->_src);     do_temp(opArrayCopy->_src);
      do_input(opArrayCopy->_src_pos); do_temp(opArrayCopy->_src_pos);
      do_input(opArrayCopy->_dst);     do_temp(opArrayCopy->_dst);
      do_input(opArrayCopy->_dst_pos); do_temp(opArrayCopy->_dst_pos);
      do_input(opArrayCopy->_length);  do_temp(opArrayCopy->_length);
      do_temp(opArrayCopy->_tmp);
      if (opArrayCopy->_info)                     do_info(opArrayCopy->_info);

      // the implementation of arraycopy always has a call into the runtime
      do_call();

      break;
    }

// LIR_OpUpdateCRC32
    case lir_updatecrc32: {
      LIR_OpUpdateCRC32* opUp = (LIR_OpUpdateCRC32*)op;

      do_input(opUp->_crc);     do_temp(opUp->_crc);
      do_input(opUp->_val);     do_temp(opUp->_val);
      do_output(opUp->_result);

      break;
    }

// LIR_OpLock
    case lir_lock:
    case lir_unlock: {
      LIR_OpLock* opLock = (LIR_OpLock*)op;

      if (opLock->_info)                          do_info(opLock->_info);

      // TODO: check if these operands really have to be temp
      // (or if input is sufficient). This may have influence on the oop map!
      do_temp(opLock->_lock);
      do_temp(opLock->_hdr);
      do_temp(opLock->_obj);

      if (opLock->_scratch->is_valid())           do_temp(opLock->_scratch);

      do_stub(opLock->_stub);

      break;
    }

// LIR_OpDelay
    case lir_delay_slot: {
      LIR_OpDelay* opDelay = (LIR_OpDelay*)op;

      visit(opDelay->delay_op());
      break;
    }

// LIR_OpTypeCheck
    case lir_instanceof:
    case lir_checkcast:
    case lir_store_check: {
      LIR_OpTypeCheck* opTypeCheck = (LIR_OpTypeCheck*)op;

      if (opTypeCheck->_info_for_exception)       do_info(opTypeCheck->_info_for_exception);
      if (opTypeCheck->_info_for_patch)           do_info(opTypeCheck->_info_for_patch);
      if (opTypeCheck->_object->is_valid())       do_input(opTypeCheck->_object);
      if (op->code() == lir_store_check && opTypeCheck->_object->is_valid()) {
        do_temp(opTypeCheck->_object);
      }
      if (opTypeCheck->_array->is_valid())        do_input(opTypeCheck->_array);
      if (opTypeCheck->_tmp1->is_valid())         do_temp(opTypeCheck->_tmp1);
      if (opTypeCheck->_tmp2->is_valid())         do_temp(opTypeCheck->_tmp2);
      if (opTypeCheck->_tmp3->is_valid())         do_temp(opTypeCheck->_tmp3);
      if (opTypeCheck->_result->is_valid())       do_output(opTypeCheck->_result);
                                                  do_stub(opTypeCheck->_stub);
      break;
    }

// LIR_OpCompareAndSwap
    case lir_cas_long:
    case lir_cas_obj:
    case lir_cas_int: {
      LIR_OpCompareAndSwap* opCompareAndSwap = (LIR_OpCompareAndSwap*)op;

      if (opCompareAndSwap->_info)                    do_info(opCompareAndSwap->_info);
                                                      do_input(opCompareAndSwap->_addr);
                                                      do_temp(opCompareAndSwap->_addr);
                                                      do_input(opCompareAndSwap->_cmp_value);
                                                      do_temp(opCompareAndSwap->_cmp_value);
                                                      do_input(opCompareAndSwap->_new_value);
                                                      do_temp(opCompareAndSwap->_new_value);
      if (opCompareAndSwap->_tmp1->is_valid())        do_temp(opCompareAndSwap->_tmp1);
      if (opCompareAndSwap->_tmp2->is_valid())        do_temp(opCompareAndSwap->_tmp2);
      if (opCompareAndSwap->_result->is_valid())      do_output(opCompareAndSwap->_result);

      break;
    }

// LIR_OpAllocArray;
    case lir_alloc_array: {
      LIR_OpAllocArray* opAllocArray = (LIR_OpAllocArray*)op;

      if (opAllocArray->_info)                        do_info(opAllocArray->_info);
      if (opAllocArray->_klass->is_valid())           do_input(opAllocArray->_klass); do_temp(opAllocArray->_klass);
      if (opAllocArray->_len->is_valid())             do_input(opAllocArray->_len);   do_temp(opAllocArray->_len);
      if (opAllocArray->_tmp1->is_valid())            do_temp(opAllocArray->_tmp1);
      if (opAllocArray->_tmp2->is_valid())            do_temp(opAllocArray->_tmp2);
      if (opAllocArray->_tmp3->is_valid())            do_temp(opAllocArray->_tmp3);
      if (opAllocArray->_tmp4->is_valid())            do_temp(opAllocArray->_tmp4);
      if (opAllocArray->_result->is_valid())          do_output(opAllocArray->_result);
                                                      do_stub(opAllocArray->_stub);
      break;
    }

// LIR_OpProfileCall:
    case lir_profile_call: {
      LIR_OpProfileCall* opProfileCall = (LIR_OpProfileCall*)op;

      if (opProfileCall->_recv->is_valid())              do_temp(opProfileCall->_recv);
      do_temp(opProfileCall->_mdo);
      do_temp(opProfileCall->_tmp1);
      break;
    }

// LIR_OpProfileType:
    case lir_profile_type: {
      LIR_OpProfileType* opProfileType = (LIR_OpProfileType*)op;

      do_input(opProfileType->_mdp); do_temp(opProfileType->_mdp);
      do_input(opProfileType->_obj);
      do_temp(opProfileType->_tmp);
      break;
    }
  default:
    op->visit(this);
  }
}

void LIR_Op::visit(LIR_OpVisitState* state) {
  ShouldNotReachHere();
}

void LIR_OpVisitState::do_stub(CodeStub* stub) {
  if (stub != NULL) {
    stub->visit(this);
  }
}

XHandlers* LIR_OpVisitState::all_xhandler() {
  XHandlers* result = NULL;

  int i;
  for (i = 0; i < info_count(); i++) {
    if (info_at(i)->exception_handlers() != NULL) {
      result = info_at(i)->exception_handlers();
      break;
    }
  }

  if (result != NULL) {
    return result;
  } else {
    return new XHandlers();
  }

  return result;
}

//---------------------------------------------------

void LIR_OpJavaCall::emit_code(LIR_Assembler* masm) {
  masm->emit_call(this);
}

void LIR_OpRTCall::emit_code(LIR_Assembler* masm) {
  masm->emit_rtcall(this);
}

void LIR_OpLabel::emit_code(LIR_Assembler* masm) {
  masm->emit_opLabel(this);
}

void LIR_OpArrayCopy::emit_code(LIR_Assembler* masm) {
  masm->emit_arraycopy(this);
  masm->append_code_stub(stub());
}

void LIR_OpUpdateCRC32::emit_code(LIR_Assembler* masm) {
  masm->emit_updatecrc32(this);
}

void LIR_Op0::emit_code(LIR_Assembler* masm) {
  masm->emit_op0(this);
}

void LIR_Op1::emit_code(LIR_Assembler* masm) {
  masm->emit_op1(this);
}

void LIR_OpAllocObj::emit_code(LIR_Assembler* masm) {
  masm->emit_alloc_obj(this);
  masm->append_code_stub(stub());
}

void LIR_OpBranch::emit_code(LIR_Assembler* masm) {
  masm->emit_opBranch(this);
  if (stub()) {
    masm->append_code_stub(stub());
  }
}

void LIR_OpConvert::emit_code(LIR_Assembler* masm) {
  masm->emit_opConvert(this);
  if (stub() != NULL) {
    masm->append_code_stub(stub());
  }
}

void LIR_Op2::emit_code(LIR_Assembler* masm) {
  masm->emit_op2(this);
}

void LIR_OpAllocArray::emit_code(LIR_Assembler* masm) {
  masm->emit_alloc_array(this);
  masm->append_code_stub(stub());
}

void LIR_OpTypeCheck::emit_code(LIR_Assembler* masm) {
  masm->emit_opTypeCheck(this);
  if (stub()) {
    masm->append_code_stub(stub());
  }
}

void LIR_OpCompareAndSwap::emit_code(LIR_Assembler* masm) {
  masm->emit_compare_and_swap(this);
}

void LIR_Op3::emit_code(LIR_Assembler* masm) {
  masm->emit_op3(this);
}

void LIR_OpLock::emit_code(LIR_Assembler* masm) {
  masm->emit_lock(this);
  if (stub()) {
    masm->append_code_stub(stub());
  }
}

void LIR_OpDelay::emit_code(LIR_Assembler* masm) {
  masm->emit_delay(this);
}

void LIR_OpProfileCall::emit_code(LIR_Assembler* masm) {
  masm->emit_profile_call(this);
}

void LIR_OpProfileType::emit_code(LIR_Assembler* masm) {
  masm->emit_profile_type(this);
}

// LIR_List
LIR_List::LIR_List(Compilation* compilation, BlockBegin* block) : _operations(8), _compilation(compilation) { }

void LIR_List::append(LIR_InsertionBuffer* buffer) {
  const int n = _operations.length();

  if (buffer->number_of_ops() > 0) {
    // increase size of instructions list
    _operations.at_grow(n + buffer->number_of_ops() - 1, NULL);
    // insert ops from buffer into instructions list
    int op_index = buffer->number_of_ops() - 1;
    int ip_index = buffer->number_of_insertion_points() - 1;
    int from_index = n - 1;
    int to_index = _operations.length() - 1;
    for ( ; ip_index >= 0; ip_index--) {
      int index = buffer->index_at(ip_index);
      // make room after insertion point
      while (index < from_index) {
        _operations.at_put(to_index --, _operations.at(from_index--));
      }
      // insert ops from buffer
      for (int i = buffer->count_at(ip_index); i > 0; i--) {
        _operations.at_put(to_index --, buffer->op_at(op_index--));
      }
    }
  }

  buffer->finish();
}

void LIR_List::oop2reg_patch(jobject o, LIR_Opr reg, CodeEmitInfo* info) {
  append(new LIR_Op1(lir_move, LIR_OprFact::oopConst(o),  reg, T_OBJECT, lir_patch_normal, info));
}

void LIR_List::klass2reg_patch(Metadata* o, LIR_Opr reg, CodeEmitInfo* info) {
  append(new LIR_Op1(lir_move, LIR_OprFact::metadataConst(o), reg, T_METADATA, lir_patch_normal, info));
}

void LIR_List::load(LIR_Address* addr, LIR_Opr src, CodeEmitInfo* info, LIR_PatchCode patch_code) {
  append(new LIR_Op1(lir_move, LIR_OprFact::address(addr), src, addr->type(), patch_code, info));
}

void LIR_List::volatile_load_mem_reg(LIR_Address* address, LIR_Opr dst, CodeEmitInfo* info, LIR_PatchCode patch_code) {
  append(new LIR_Op1(lir_move, LIR_OprFact::address(address), dst, address->type(), patch_code, info, lir_move_volatile));
}

void LIR_List::volatile_load_unsafe_reg(LIR_Opr base, LIR_Opr offset, LIR_Opr dst, BasicType type, CodeEmitInfo* info, LIR_PatchCode patch_code) {
  append(new LIR_Op1(lir_move, LIR_OprFact::address(new LIR_Address(base, offset, type)), dst, type, patch_code, info, lir_move_volatile));
}

void LIR_List::store_mem_int(jint v, LIR_Opr base, int offset_in_bytes, BasicType type, CodeEmitInfo* info, LIR_PatchCode patch_code) {
  append(new LIR_Op1(lir_move, LIR_OprFact::intConst(v), LIR_OprFact::address(new LIR_Address(base, offset_in_bytes, type)), type, patch_code, info));
}

void LIR_List::store_mem_oop(jobject o, LIR_Opr base, int offset_in_bytes, BasicType type, CodeEmitInfo* info, LIR_PatchCode patch_code) {
  append(new LIR_Op1(lir_move, LIR_OprFact::oopConst(o), LIR_OprFact::address(new LIR_Address(base, offset_in_bytes, type)), type, patch_code, info));
}

void LIR_List::store(LIR_Opr src, LIR_Address* addr, CodeEmitInfo* info, LIR_PatchCode patch_code) {
  append(new LIR_Op1(lir_move, src, LIR_OprFact::address(addr), addr->type(), patch_code, info));
}

void LIR_List::volatile_store_mem_reg(LIR_Opr src, LIR_Address* addr, CodeEmitInfo* info, LIR_PatchCode patch_code) {
  append(new LIR_Op1(lir_move, src, LIR_OprFact::address(addr), addr->type(), patch_code, info, lir_move_volatile));
}

void LIR_List::volatile_store_unsafe_reg(LIR_Opr src, LIR_Opr base, LIR_Opr offset, BasicType type, CodeEmitInfo* info, LIR_PatchCode patch_code) {
  append(new LIR_Op1(lir_move, src, LIR_OprFact::address(new LIR_Address(base, offset, type)), type, patch_code, info, lir_move_volatile));
}

void LIR_List::idiv(LIR_Opr left, LIR_Opr right, LIR_Opr res, LIR_Opr tmp, CodeEmitInfo* info) {
  append(new LIR_Op3(lir_idiv, left, right, tmp, res, info));
}

void LIR_List::idiv(LIR_Opr left, int right, LIR_Opr res, LIR_Opr tmp, CodeEmitInfo* info) {
  append(new LIR_Op3(lir_idiv, left, LIR_OprFact::intConst(right), tmp, res, info));
}

void LIR_List::irem(LIR_Opr left, LIR_Opr right, LIR_Opr res, LIR_Opr tmp, CodeEmitInfo* info) {
  append(new LIR_Op3(lir_irem, left, right, tmp, res, info));
}

void LIR_List::irem(LIR_Opr left, int right, LIR_Opr res, LIR_Opr tmp, CodeEmitInfo* info) {
  append(new LIR_Op3(lir_irem, left, LIR_OprFact::intConst(right), tmp, res, info));
}

void LIR_List::cmp_mem_int(LIR_Condition condition, LIR_Opr base, int disp, int c, CodeEmitInfo* info) {
  append(new LIR_Op2(lir_cmp, condition, LIR_OprFact::address(new LIR_Address(base, disp, T_INT)), LIR_OprFact::intConst(c), info));
}

void LIR_List::cmp_reg_mem(LIR_Condition condition, LIR_Opr reg, LIR_Address* addr, CodeEmitInfo* info) {
  append(new LIR_Op2(lir_cmp, condition, reg, LIR_OprFact::address(addr), info));
}

void LIR_List::allocate_object(LIR_Opr dst, LIR_Opr t1, LIR_Opr t2, LIR_Opr t3, LIR_Opr t4, int header_size, int object_size, LIR_Opr klass, bool init_check, CodeStub* stub) {
  append(new LIR_OpAllocObj(klass, dst, t1, t2, t3, t4, header_size, object_size, init_check, stub));
}

void LIR_List::allocate_array(LIR_Opr dst, LIR_Opr len, LIR_Opr t1,LIR_Opr t2, LIR_Opr t3,LIR_Opr t4, BasicType type, LIR_Opr klass, CodeStub* stub) {
  append(new LIR_OpAllocArray(klass, len, dst, t1, t2, t3, t4, type, stub));
}

void LIR_List::shift_left(LIR_Opr value, LIR_Opr count, LIR_Opr dst, LIR_Opr tmp) {
 append(new LIR_Op2(lir_shl, value, count, dst, tmp));
}

void LIR_List::shift_right(LIR_Opr value, LIR_Opr count, LIR_Opr dst, LIR_Opr tmp) {
 append(new LIR_Op2(lir_shr, value, count, dst, tmp));
}

void LIR_List::unsigned_shift_right(LIR_Opr value, LIR_Opr count, LIR_Opr dst, LIR_Opr tmp) {
 append(new LIR_Op2(lir_ushr, value, count, dst, tmp));
}

void LIR_List::fcmp2int(LIR_Opr left, LIR_Opr right, LIR_Opr dst, bool is_unordered_less) {
  append(new LIR_Op2(is_unordered_less ? lir_ucmp_fd2i : lir_cmp_fd2i, left, right, dst));
}

void LIR_List::lock_object(LIR_Opr hdr, LIR_Opr obj, LIR_Opr lock, LIR_Opr scratch, CodeStub* stub, CodeEmitInfo* info) {
  append(new LIR_OpLock(lir_lock, hdr, obj, lock, scratch, stub, info));
}

void LIR_List::unlock_object(LIR_Opr hdr, LIR_Opr obj, LIR_Opr lock, LIR_Opr scratch, CodeStub* stub) {
  append(new LIR_OpLock(lir_unlock, hdr, obj, lock, scratch, stub, NULL));
}

void check_LIR() {
  // cannot do the proper checking as PRODUCT and other modes return different results
  // guarantee(sizeof(LIR_OprDesc) == wordSize, "may not have a v-table");
}

void LIR_List::checkcast(LIR_Opr result, LIR_Opr object, ciKlass* klass, LIR_Opr tmp1, LIR_Opr tmp2, LIR_Opr tmp3, bool fast_check, CodeEmitInfo* info_for_exception, CodeEmitInfo* info_for_patch, CodeStub* stub, ciMethod* profiled_method, int profiled_bci) {
  LIR_OpTypeCheck* c = new LIR_OpTypeCheck(lir_checkcast, result, object, klass,
                                           tmp1, tmp2, tmp3, fast_check, info_for_exception, info_for_patch, stub);
  if (profiled_method != NULL) {
    c->set_profiled_method(profiled_method);
    c->set_profiled_bci(profiled_bci);
    c->set_should_profile(true);
  }
  append(c);
}

void LIR_List::instanceof(LIR_Opr result, LIR_Opr object, ciKlass* klass, LIR_Opr tmp1, LIR_Opr tmp2, LIR_Opr tmp3, bool fast_check, CodeEmitInfo* info_for_patch, ciMethod* profiled_method, int profiled_bci) {
  LIR_OpTypeCheck* c = new LIR_OpTypeCheck(lir_instanceof, result, object, klass, tmp1, tmp2, tmp3, fast_check, NULL, info_for_patch, NULL);
  if (profiled_method != NULL) {
    c->set_profiled_method(profiled_method);
    c->set_profiled_bci(profiled_bci);
    c->set_should_profile(true);
  }
  append(c);
}

void LIR_List::store_check(LIR_Opr object, LIR_Opr array, LIR_Opr tmp1, LIR_Opr tmp2, LIR_Opr tmp3, CodeEmitInfo* info_for_exception, ciMethod* profiled_method, int profiled_bci) {
  LIR_OpTypeCheck* c = new LIR_OpTypeCheck(lir_store_check, object, array, tmp1, tmp2, tmp3, info_for_exception);
  if (profiled_method != NULL) {
    c->set_profiled_method(profiled_method);
    c->set_profiled_bci(profiled_bci);
    c->set_should_profile(true);
  }
  append(c);
}

void LIR_List::null_check(LIR_Opr opr, CodeEmitInfo* info) {
  // Emit an implicit null check
  append(new LIR_Op1(lir_null_check, opr, info));
}

void LIR_List::cas_long(LIR_Opr addr, LIR_Opr cmp_value, LIR_Opr new_value, LIR_Opr t1, LIR_Opr t2, LIR_Opr result) {
  append(new LIR_OpCompareAndSwap(lir_cas_long, addr, cmp_value, new_value, t1, t2, result));
}

void LIR_List::cas_obj(LIR_Opr addr, LIR_Opr cmp_value, LIR_Opr new_value, LIR_Opr t1, LIR_Opr t2, LIR_Opr result) {
  append(new LIR_OpCompareAndSwap(lir_cas_obj, addr, cmp_value, new_value, t1, t2, result));
}

void LIR_List::cas_int(LIR_Opr addr, LIR_Opr cmp_value, LIR_Opr new_value, LIR_Opr t1, LIR_Opr t2, LIR_Opr result) {
  append(new LIR_OpCompareAndSwap(lir_cas_int, addr, cmp_value, new_value, t1, t2, result));
}

void print_LIR(BlockList* blocks) { }

// Implementation of LIR_InsertionBuffer

void LIR_InsertionBuffer::append(int index, LIR_Op* op) {
  int i = number_of_insertion_points() - 1;
  if (i < 0 || index_at(i) < index) {
    append_new(index, 1);
  } else {
    set_count_at(i, count_at(i) + 1);
  }
  _ops.push(op);
}
