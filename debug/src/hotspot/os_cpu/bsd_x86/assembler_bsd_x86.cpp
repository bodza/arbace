#include "precompiled.hpp"
#include "asm/macroAssembler.hpp"
#include "asm/macroAssembler.inline.hpp"
#include "runtime/os.hpp"

void MacroAssembler::int3() {
  call(RuntimeAddress(CAST_FROM_FN_PTR(address, os::breakpoint)));
}
