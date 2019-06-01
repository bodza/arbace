#include "precompiled.hpp"

#include "code/debugInfoRec.hpp"
#include "code/nmethod.hpp"
#include "code/pcDesc.hpp"
#include "code/scopeDesc.hpp"
#include "memory/resourceArea.hpp"

PcDesc::PcDesc(int pc_offset, int scope_decode_offset, int obj_decode_offset) {
  _pc_offset           = pc_offset;
  _scope_decode_offset = scope_decode_offset;
  _obj_decode_offset   = obj_decode_offset;
  _flags               = 0;
}

address PcDesc::real_pc(const CompiledMethod* code) const {
  return code->code_begin() + pc_offset();
}

void PcDesc::print(CompiledMethod* code) { }

bool PcDesc::verify(CompiledMethod* code) {
  //Unimplemented();
  return true;
}
