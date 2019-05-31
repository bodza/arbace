#include "precompiled.hpp"
#include "assembler_zero.inline.hpp"
#include "entry_zero.hpp"
#include "interpreter/cppInterpreter.hpp"
#include "memory/resourceArea.hpp"
#include "nativeInst_zero.hpp"
#include "oops/oop.inline.hpp"
#include "runtime/handles.hpp"
#include "runtime/sharedRuntime.hpp"
#include "runtime/stubRoutines.hpp"
#include "utilities/ostream.hpp"
#include "c1/c1_Runtime1.hpp"

// This method is called by nmethod::make_not_entrant_or_zombie to
// insert a jump to SharedRuntime::get_handle_wrong_method_stub()
// (dest) at the start of a compiled method (verified_entry) to avoid
// a race where a method is invoked while being made non-entrant.

void NativeJump::patch_verified_entry(address entry, address verified_entry, address dest) {

#ifdef CC_INTERP
  ((ZeroEntry*) verified_entry)->set_entry_point((address) CppInterpreter::normal_entry);
#else
  Unimplemented();
#endif
}
