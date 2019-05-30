#include <stdlib.h>

#include "precompiled.hpp"
#include "code/codeBlob.hpp"
#include "asm/macroAssembler.hpp"

// hook routine called during JVM bootstrap to test AArch64 assembler

extern "C" void entry(CodeBuffer*);

void aarch64TestHook()
{
  BufferBlob* b = BufferBlob::create("aarch64Test", 500000);
  CodeBuffer code(b);
  MacroAssembler _masm(&code);
  entry(&code);
}
