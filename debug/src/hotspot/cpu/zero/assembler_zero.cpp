#include "precompiled.hpp"
#include "assembler_zero.inline.hpp"
#include "gc/shared/cardTableBarrierSet.hpp"
#include "gc/shared/collectedHeap.inline.hpp"
#include "interpreter/interpreter.hpp"
#include "memory/resourceArea.hpp"
#include "prims/methodHandles.hpp"
#include "runtime/biasedLocking.hpp"
#include "runtime/interfaceSupport.inline.hpp"
#include "runtime/objectMonitor.hpp"
#include "runtime/os.hpp"
#include "runtime/sharedRuntime.hpp"
#include "runtime/stubRoutines.hpp"
#include "utilities/macros.hpp"

int AbstractAssembler::code_fill_byte() {
  return 0;
}

void Assembler::pd_patch_instruction(address branch, address target) {
  ShouldNotCallThis();
}

void MacroAssembler::align(int modulus) {
  while (offset() % modulus != 0)
    emit_int8(AbstractAssembler::code_fill_byte());
}

void MacroAssembler::bang_stack_with_offset(int offset) {
  ShouldNotCallThis();
}

void MacroAssembler::advance(int bytes) {
  code_section()->set_end(code_section()->end() + bytes);
}

RegisterOrConstant MacroAssembler::delayed_value_impl(intptr_t* delayed_value_addr, Register tmpl, int offset) {
  ShouldNotCallThis();
  return RegisterOrConstant();
}

void MacroAssembler::store_oop(jobject obj) {
  code_section()->relocate(pc(), oop_Relocation::spec_for_immediate());
  emit_address((address) obj);
}

void MacroAssembler::store_Metadata(Metadata* md) {
  code_section()->relocate(pc(), metadata_Relocation::spec_for_immediate());
  emit_address((address) md);
}

static void should_not_call() {
  report_should_not_call(__FILE__, __LINE__);
}

address ShouldNotCallThisStub() {
  return (address) should_not_call;
}

address ShouldNotCallThisEntry() {
  return (address) should_not_call;
}
