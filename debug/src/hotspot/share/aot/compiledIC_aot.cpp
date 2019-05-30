#include "precompiled.hpp"

#include "aot/compiledIC_aot.hpp"

bool CompiledPltStaticCall::is_call_to_interpreted() const {
  // It is a call to interpreted, if it calls to a stub. Hence, the destination
  // must be in the stub part of the nmethod that contains the call
  return destination() == _call->plt_c2i_stub();
}

address CompiledPltStaticCall::find_stub() {
  // It is static NativePltCall. Return c2i stub address.
  return _call->plt_c2i_stub();
}
