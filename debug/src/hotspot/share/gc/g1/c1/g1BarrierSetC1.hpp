#ifndef SHARE_GC_G1_C1_G1BARRIERSETC1_HPP
#define SHARE_GC_G1_C1_G1BARRIERSETC1_HPP

#include "c1/c1_CodeStubs.hpp"
#include "gc/shared/c1/modRefBarrierSetC1.hpp"

class G1PreBarrierStub: public CodeStub {
  friend class G1BarrierSetC1;
 private:
  bool _do_load;
  LIR_Opr _addr;
  LIR_Opr _pre_val;
  LIR_PatchCode _patch_code;
  CodeEmitInfo* _info;

 public:
  // Version that _does_ generate a load of the previous value from addr.
  // addr (the address of the field to be read) must be a LIR_Address
  // pre_val (a temporary register) must be a register;
  G1PreBarrierStub(LIR_Opr addr, LIR_Opr pre_val, LIR_PatchCode patch_code, CodeEmitInfo* info) :
    _addr(addr), _pre_val(pre_val), _do_load(true),
    _patch_code(patch_code), _info(info)
  { }

  // Version that _does not_ generate load of the previous value; the
  // previous value is assumed to have already been loaded into pre_val.
  G1PreBarrierStub(LIR_Opr pre_val) :
    _addr(LIR_OprFact::illegalOpr), _pre_val(pre_val), _do_load(false),
    _patch_code(lir_patch_none), _info(NULL)
  { }

  LIR_Opr addr()             const { return _addr; }
  LIR_Opr pre_val()          const { return _pre_val; }
  LIR_PatchCode patch_code() const { return _patch_code; }
  CodeEmitInfo* info()       const { return _info; }
  bool do_load()             const { return _do_load; }

  virtual void emit_code(LIR_Assembler* e);
  virtual void visit(LIR_OpVisitState* visitor) {
    if (_do_load) {
      // don't pass in the code emit info since it's processed in the fast
      // path
      if (_info != NULL)
        visitor->do_slow_case(_info);
      else
        visitor->do_slow_case();

      visitor->do_input(_addr);
      visitor->do_temp(_pre_val);
    } else {
      visitor->do_slow_case();
      visitor->do_input(_pre_val);
    }
  }
};

class G1PostBarrierStub: public CodeStub {
  friend class G1BarrierSetC1;
 private:
  LIR_Opr _addr;
  LIR_Opr _new_val;

 public:
  // addr (the address of the object head) and new_val must be registers.
  G1PostBarrierStub(LIR_Opr addr, LIR_Opr new_val): _addr(addr), _new_val(new_val) { }

  LIR_Opr addr() const { return _addr; }
  LIR_Opr new_val() const { return _new_val; }

  virtual void emit_code(LIR_Assembler* e);
  virtual void visit(LIR_OpVisitState* visitor) {
    // don't pass in the code emit info since it's processed in the fast path
    visitor->do_slow_case();
    visitor->do_input(_addr);
    visitor->do_input(_new_val);
  }
};

class CodeBlob;

class G1BarrierSetC1 : public ModRefBarrierSetC1 {
 protected:
  CodeBlob* _pre_barrier_c1_runtime_code_blob;
  CodeBlob* _post_barrier_c1_runtime_code_blob;

  virtual void pre_barrier(LIRAccess& access, LIR_Opr addr_opr, LIR_Opr pre_val, CodeEmitInfo* info);
  virtual void post_barrier(LIRAccess& access, LIR_OprDesc* addr, LIR_OprDesc* new_val);

  virtual void load_at_resolved(LIRAccess& access, LIR_Opr result);

 public:
  G1BarrierSetC1()
    : _pre_barrier_c1_runtime_code_blob(NULL),
      _post_barrier_c1_runtime_code_blob(NULL) { }

  CodeBlob* pre_barrier_c1_runtime_code_blob() { return _pre_barrier_c1_runtime_code_blob; }
  CodeBlob* post_barrier_c1_runtime_code_blob() { return _post_barrier_c1_runtime_code_blob; }

  virtual void generate_c1_runtime_stubs(BufferBlob* buffer_blob);
};

#endif
