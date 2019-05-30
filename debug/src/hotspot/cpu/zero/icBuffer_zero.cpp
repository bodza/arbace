#include "precompiled.hpp"
#include "asm/assembler.hpp"
#include "assembler_zero.inline.hpp"
#include "code/icBuffer.hpp"
#include "gc/shared/collectedHeap.inline.hpp"
#include "interpreter/bytecodes.hpp"
#include "memory/resourceArea.hpp"
#include "nativeInst_zero.hpp"
#include "oops/oop.inline.hpp"

int InlineCacheBuffer::ic_stub_code_size() {
  // NB set this once the functions below are implemented
  return 4;
}

void InlineCacheBuffer::assemble_ic_buffer_code(address code_begin, void* cached_oop, address entry_point) {
  // NB ic_stub_code_size() must return the size of the code we generate
  ShouldNotCallThis();
}

address InlineCacheBuffer::ic_buffer_entry_point(address code_begin) {
  // NB ic_stub_code_size() must return the size of the code we generate
  ShouldNotCallThis();
  return NULL;
}

void* InlineCacheBuffer::ic_buffer_cached_value(address code_begin) {
  ShouldNotCallThis();
  return NULL;
}
