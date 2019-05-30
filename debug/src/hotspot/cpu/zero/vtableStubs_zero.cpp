#include "precompiled.hpp"
#include "code/vtableStubs.hpp"
#include "utilities/debug.hpp"

VtableStub* VtableStubs::create_vtable_stub(int vtable_index) {
  ShouldNotCallThis();
  return NULL;
}

VtableStub* VtableStubs::create_itable_stub(int vtable_index) {
  ShouldNotCallThis();
  return NULL;
}

int VtableStub::pd_code_alignment() {
  ShouldNotCallThis();
  return 0;
}
