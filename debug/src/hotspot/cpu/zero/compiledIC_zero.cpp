#include "precompiled.hpp"
#include "classfile/systemDictionary.hpp"
#include "code/codeCache.hpp"
#include "code/compiledIC.hpp"
#include "code/icBuffer.hpp"
#include "code/nmethod.hpp"
#include "code/vtableStubs.hpp"
#include "interpreter/interpreter.hpp"
#include "interpreter/linkResolver.hpp"
#include "memory/metadataFactory.hpp"
#include "memory/oopFactory.hpp"
#include "oops/method.hpp"
#include "oops/oop.inline.hpp"
#include "oops/symbol.hpp"
#include "runtime/icache.hpp"
#include "runtime/sharedRuntime.hpp"
#include "runtime/stubRoutines.hpp"
#include "utilities/events.hpp"

// ----------------------------------------------------------------------------

address CompiledStaticCall::emit_to_interp_stub(CodeBuffer &cbuf, address mark) {
  ShouldNotReachHere(); // Only needed for COMPILER2.
  return NULL;
}

int CompiledStaticCall::to_interp_stub_size() {
  ShouldNotReachHere(); // Only needed for COMPILER2.
  return 0;
}

// Relocation entries for call stub, compiled java to interpreter.
int CompiledStaticCall::reloc_to_interp_stub() {
  ShouldNotReachHere(); // Only needed for COMPILER2.
  return 0;
}

void CompiledDirectStaticCall::set_to_interpreted(const methodHandle& callee, address entry) {
  ShouldNotReachHere(); // Only needed for COMPILER2.
}

void CompiledDirectStaticCall::set_stub_to_clean(static_stub_Relocation* static_stub) {
  ShouldNotReachHere(); // Only needed for COMPILER2.
}
