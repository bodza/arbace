#ifndef SHARE_VM_C1_C1_CODESTUBS_HPP
#define SHARE_VM_C1_C1_CODESTUBS_HPP

#include "c1/c1_FrameMap.hpp"
#include "c1/c1_IR.hpp"
#include "c1/c1_Instruction.hpp"
#include "c1/c1_LIR.hpp"
#include "c1/c1_Runtime1.hpp"
#include "code/nativeInst.hpp"
#include "utilities/growableArray.hpp"
#include "utilities/macros.hpp"

class CodeEmitInfo;
class LIR_Assembler;
class LIR_OpVisitState;

// CodeStubs are little 'out-of-line' pieces of code that
// usually handle slow cases of operations. All code stubs
// are collected and code is emitted at the end of the
// nmethod.

class CodeStub: public CompilationResourceObj {
 protected:
  Label _entry;                                  // label at the stub entry point
  Label _continuation;                           // label where stub continues, if any

 public:
  CodeStub() {}

  // code generation
  void assert_no_unbound_labels()                {
    assert(!_entry.is_unbound() && !_continuation.is_unbound(), "unbound label"); }
  virtual void emit_code(LIR_Assembler* e) = 0;
  virtual CodeEmitInfo* info() const             { return NULL; }
  virtual bool is_exception_throw_stub() const   { return false; }
  virtual bool is_range_check_stub() const       { return false; }
  virtual bool is_divbyzero_stub() const         { return false; }
  virtual bool is_simple_exception_stub() const  { return false; }

  // label access
  Label* entry()                                 { return &_entry; }
  Label* continuation()                          { return &_continuation; }
  // for LIR
  virtual void visit(LIR_OpVisitState* visit) {
  }
};

class CodeStubList: public GrowableArray<CodeStub*> {
 public:
  CodeStubList(): GrowableArray<CodeStub*>() {}

  void append(CodeStub* stub) {
    if (!contains(stub)) {
      GrowableArray<CodeStub*>::append(stub);
    }
  }
};

class CounterOverflowStub: public CodeStub {
 private:
  CodeEmitInfo* _info;
  int           _bci;
  LIR_Opr       _method;

public:
  CounterOverflowStub(CodeEmitInfo* info, int bci, LIR_Opr method) :  _info(info), _bci(bci), _method(method) {
  }

  virtual void emit_code(LIR_Assembler* e);

  virtual void visit(LIR_OpVisitState* visitor) {
    visitor->do_slow_case(_info);
    visitor->do_input(_method);
  }
};

class ConversionStub: public CodeStub {
 private:
  Bytecodes::Code _bytecode;
  LIR_Opr         _input;
  LIR_Opr         _result;

  static float float_zero;
  static double double_zero;
 public:
  ConversionStub(Bytecodes::Code bytecode, LIR_Opr input, LIR_Opr result)
    : _bytecode(bytecode), _input(input), _result(result) {
  }

  Bytecodes::Code bytecode() { return _bytecode; }
  LIR_Opr         input()    { return _input; }
  LIR_Opr         result()   { return _result; }

  virtual void emit_code(LIR_Assembler* e);
  virtual void visit(LIR_OpVisitState* visitor) {
    visitor->do_slow_case();
    visitor->do_input(_input);
    visitor->do_output(_result);
  }
};

// Throws ArrayIndexOutOfBoundsException by default but can be
// configured to throw IndexOutOfBoundsException in constructor
class RangeCheckStub: public CodeStub {
 private:
  CodeEmitInfo* _info;
  LIR_Opr       _index;
  LIR_Opr       _array;
  bool          _throw_index_out_of_bounds_exception;

 public:
  // For ArrayIndexOutOfBoundsException.
  RangeCheckStub(CodeEmitInfo* info, LIR_Opr index, LIR_Opr array);
  // For IndexOutOfBoundsException.
  RangeCheckStub(CodeEmitInfo* info, LIR_Opr index);
  virtual void emit_code(LIR_Assembler* e);
  virtual CodeEmitInfo* info() const             { return _info; }
  virtual bool is_exception_throw_stub() const   { return true; }
  virtual bool is_range_check_stub() const       { return true; }
  virtual void visit(LIR_OpVisitState* visitor) {
    visitor->do_slow_case(_info);
    visitor->do_input(_index);
    if (_array) { visitor->do_input(_array); }
  }
};

// stub used when predicate fails and deoptimization is needed
class PredicateFailedStub: public CodeStub {
 private:
  CodeEmitInfo* _info;

 public:
  PredicateFailedStub(CodeEmitInfo* info);
  virtual void emit_code(LIR_Assembler* e);
  virtual CodeEmitInfo* info() const             { return _info; }
  virtual void visit(LIR_OpVisitState* visitor) {
    visitor->do_slow_case(_info);
  }
};

class DivByZeroStub: public CodeStub {
 private:
  CodeEmitInfo* _info;
  int           _offset;

 public:
  DivByZeroStub(CodeEmitInfo* info)
    : _info(info), _offset(-1) {
  }
  DivByZeroStub(int offset, CodeEmitInfo* info)
    : _info(info), _offset(offset) {
  }
  virtual void emit_code(LIR_Assembler* e);
  virtual CodeEmitInfo* info() const             { return _info; }
  virtual bool is_exception_throw_stub() const   { return true; }
  virtual bool is_divbyzero_stub() const         { return true; }
  virtual void visit(LIR_OpVisitState* visitor) {
    visitor->do_slow_case(_info);
  }
};

class ImplicitNullCheckStub: public CodeStub {
 private:
  CodeEmitInfo* _info;
  int           _offset;

 public:
  ImplicitNullCheckStub(int offset, CodeEmitInfo* info)
    : _offset(offset), _info(info) {
  }
  virtual void emit_code(LIR_Assembler* e);
  virtual CodeEmitInfo* info() const             { return _info; }
  virtual bool is_exception_throw_stub() const   { return true; }
  virtual void visit(LIR_OpVisitState* visitor) {
    visitor->do_slow_case(_info);
  }
};

class NewInstanceStub: public CodeStub {
 private:
  ciInstanceKlass* _klass;
  LIR_Opr          _klass_reg;
  LIR_Opr          _result;
  CodeEmitInfo*    _info;
  Runtime1::StubID _stub_id;

 public:
  NewInstanceStub(LIR_Opr klass_reg, LIR_Opr result, ciInstanceKlass* klass, CodeEmitInfo* info, Runtime1::StubID stub_id);
  virtual void emit_code(LIR_Assembler* e);
  virtual CodeEmitInfo* info() const             { return _info; }
  virtual void visit(LIR_OpVisitState* visitor) {
    visitor->do_slow_case(_info);
    visitor->do_input(_klass_reg);
    visitor->do_output(_result);
  }
};

class NewTypeArrayStub: public CodeStub {
 private:
  LIR_Opr       _klass_reg;
  LIR_Opr       _length;
  LIR_Opr       _result;
  CodeEmitInfo* _info;

 public:
  NewTypeArrayStub(LIR_Opr klass_reg, LIR_Opr length, LIR_Opr result, CodeEmitInfo* info);
  virtual void emit_code(LIR_Assembler* e);
  virtual CodeEmitInfo* info() const             { return _info; }
  virtual void visit(LIR_OpVisitState* visitor) {
    visitor->do_slow_case(_info);
    visitor->do_input(_klass_reg);
    visitor->do_input(_length);
    assert(_result->is_valid(), "must be valid");
    visitor->do_output(_result);
  }
};

class NewObjectArrayStub: public CodeStub {
 private:
  LIR_Opr        _klass_reg;
  LIR_Opr        _length;
  LIR_Opr        _result;
  CodeEmitInfo*  _info;

 public:
  NewObjectArrayStub(LIR_Opr klass_reg, LIR_Opr length, LIR_Opr result, CodeEmitInfo* info);
  virtual void emit_code(LIR_Assembler* e);
  virtual CodeEmitInfo* info() const             { return _info; }
  virtual void visit(LIR_OpVisitState* visitor) {
    visitor->do_slow_case(_info);
    visitor->do_input(_klass_reg);
    visitor->do_input(_length);
    assert(_result->is_valid(), "must be valid");
    visitor->do_output(_result);
  }
};

class MonitorAccessStub: public CodeStub {
 protected:
  LIR_Opr _obj_reg;
  LIR_Opr _lock_reg;

 public:
  MonitorAccessStub(LIR_Opr obj_reg, LIR_Opr lock_reg) {
    _obj_reg  = obj_reg;
    _lock_reg  = lock_reg;
  }
};

class MonitorEnterStub: public MonitorAccessStub {
 private:
  CodeEmitInfo* _info;

 public:
  MonitorEnterStub(LIR_Opr obj_reg, LIR_Opr lock_reg, CodeEmitInfo* info);

  virtual void emit_code(LIR_Assembler* e);
  virtual CodeEmitInfo* info() const             { return _info; }
  virtual void visit(LIR_OpVisitState* visitor) {
    visitor->do_input(_obj_reg);
    visitor->do_input(_lock_reg);
    visitor->do_slow_case(_info);
  }
};

class MonitorExitStub: public MonitorAccessStub {
 private:
  bool _compute_lock;
  int  _monitor_ix;

 public:
  MonitorExitStub(LIR_Opr lock_reg, bool compute_lock, int monitor_ix)
    : MonitorAccessStub(LIR_OprFact::illegalOpr, lock_reg),
      _compute_lock(compute_lock), _monitor_ix(monitor_ix) { }
  virtual void emit_code(LIR_Assembler* e);
  virtual void visit(LIR_OpVisitState* visitor) {
    assert(_obj_reg->is_illegal(), "unused");
    if (_compute_lock) {
      visitor->do_temp(_lock_reg);
    } else {
      visitor->do_input(_lock_reg);
    }
  }
};

class PatchingStub: public CodeStub {
 public:
  enum PatchID {
    access_field_id,
    load_klass_id,
    load_mirror_id,
    load_appendix_id
  };
  enum constants {
    patch_info_size = 3
  };
 private:
  PatchID       _id;
  address       _pc_start;
  int           _bytes_to_copy;
  Label         _patched_code_entry;
  Label         _patch_site_entry;
  Label         _patch_site_continuation;
  Register      _obj;
  CodeEmitInfo* _info;
  int           _index;  // index of the patchable oop or Klass* in nmethod oop or metadata table if needed
  static int    _patch_info_offset;

  void align_patch_site(MacroAssembler* masm);

 public:
  static int patch_info_offset() { return _patch_info_offset; }

  PatchingStub(MacroAssembler* masm, PatchID id, int index = -1):
      _id(id)
    , _info(NULL)
    , _index(index) {
    if (os::is_MP()) {
      // force alignment of patch sites on MP hardware so we
      // can guarantee atomic writes to the patch site.
      align_patch_site(masm);
    }
    _pc_start = masm->pc();
    masm->bind(_patch_site_entry);
  }

  void install(MacroAssembler* masm, LIR_PatchCode patch_code, Register obj, CodeEmitInfo* info) {
    _info = info;
    _obj = obj;
    masm->bind(_patch_site_continuation);
    _bytes_to_copy = masm->pc() - pc_start();
    if (_id == PatchingStub::access_field_id) {
      // embed a fixed offset to handle long patches which need to be offset by a word.
      // the patching code will just add the field offset field to this offset so
      // that we can refernce either the high or low word of a double word field.
      int field_offset = 0;
      switch (patch_code) {
      case lir_patch_low:         field_offset = lo_word_offset_in_bytes; break;
      case lir_patch_high:        field_offset = hi_word_offset_in_bytes; break;
      case lir_patch_normal:      field_offset = 0;                       break;
      default: ShouldNotReachHere();
      }
      NativeMovRegMem* n_move = nativeMovRegMem_at(pc_start());
      n_move->set_offset(field_offset);
    } else if (_id == load_klass_id || _id == load_mirror_id || _id == load_appendix_id) {
      assert(_obj != noreg, "must have register object for load_klass/load_mirror");
    } else {
      ShouldNotReachHere();
    }
    assert(_bytes_to_copy <= (masm->pc() - pc_start()), "not enough bytes");
  }

  address pc_start() const                       { return _pc_start; }
  PatchID id() const                             { return _id; }

  virtual void emit_code(LIR_Assembler* e);
  virtual CodeEmitInfo* info() const             { return _info; }
  virtual void visit(LIR_OpVisitState* visitor) {
    visitor->do_slow_case(_info);
  }
};

//------------------------------------------------------------------------------
// DeoptimizeStub
//
class DeoptimizeStub : public CodeStub {
private:
  CodeEmitInfo* _info;
  jint _trap_request;

public:
  DeoptimizeStub(CodeEmitInfo* info, Deoptimization::DeoptReason reason, Deoptimization::DeoptAction action) :
    _info(new CodeEmitInfo(info)), _trap_request(Deoptimization::make_trap_request(reason, action)) {}

  virtual void emit_code(LIR_Assembler* e);
  virtual CodeEmitInfo* info() const           { return _info; }
  virtual bool is_exception_throw_stub() const { return true; }
  virtual void visit(LIR_OpVisitState* visitor) {
    visitor->do_slow_case(_info);
  }
};

class SimpleExceptionStub: public CodeStub {
 private:
  LIR_Opr          _obj;
  Runtime1::StubID _stub;
  CodeEmitInfo*    _info;

 public:
  SimpleExceptionStub(Runtime1::StubID stub, LIR_Opr obj, CodeEmitInfo* info):
    _obj(obj), _info(info), _stub(stub) {
  }

  void set_obj(LIR_Opr obj) {
    _obj = obj;
  }

  virtual void emit_code(LIR_Assembler* e);
  virtual CodeEmitInfo* info() const             { return _info; }
  virtual bool is_exception_throw_stub() const   { return true; }
  virtual bool is_simple_exception_stub() const  { return true; }
  virtual void visit(LIR_OpVisitState* visitor) {
    if (_obj->is_valid()) visitor->do_input(_obj);
    visitor->do_slow_case(_info);
  }
};

class ArrayStoreExceptionStub: public SimpleExceptionStub {
 private:
  CodeEmitInfo* _info;

 public:
  ArrayStoreExceptionStub(LIR_Opr obj, CodeEmitInfo* info): SimpleExceptionStub(Runtime1::throw_array_store_exception_id, obj, info) {}
};

class ArrayCopyStub: public CodeStub {
 private:
  LIR_OpArrayCopy* _op;

 public:
  ArrayCopyStub(LIR_OpArrayCopy* op): _op(op) { }

  LIR_Opr src() const                         { return _op->src(); }
  LIR_Opr src_pos() const                     { return _op->src_pos(); }
  LIR_Opr dst() const                         { return _op->dst(); }
  LIR_Opr dst_pos() const                     { return _op->dst_pos(); }
  LIR_Opr length() const                      { return _op->length(); }
  LIR_Opr tmp() const                         { return _op->tmp(); }

  virtual void emit_code(LIR_Assembler* e);
  virtual CodeEmitInfo* info() const          { return _op->info(); }
  virtual void visit(LIR_OpVisitState* visitor) {
    // don't pass in the code emit info since it's processed in the fast path
    visitor->do_slow_case();
  }
};

#endif
