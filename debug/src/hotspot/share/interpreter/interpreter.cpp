#include "precompiled.hpp"

#include "asm/macroAssembler.hpp"
#include "asm/macroAssembler.inline.hpp"
#include "compiler/disassembler.hpp"
#include "interpreter/bytecodeHistogram.hpp"
#include "interpreter/bytecodeInterpreter.hpp"
#include "interpreter/interpreter.hpp"
#include "interpreter/interpreterRuntime.hpp"
#include "interpreter/interp_masm.hpp"
#include "interpreter/templateTable.hpp"
#include "memory/allocation.inline.hpp"
#include "memory/resourceArea.hpp"
#include "oops/arrayOop.hpp"
#include "oops/methodData.hpp"
#include "oops/method.hpp"
#include "oops/oop.inline.hpp"
#include "prims/forte.hpp"
#include "prims/methodHandles.hpp"
#include "runtime/handles.inline.hpp"
#include "runtime/sharedRuntime.hpp"
#include "runtime/stubRoutines.hpp"
#include "runtime/timer.hpp"

# define __ _masm->

//------------------------------------------------------------------------------------------------------------------------
// Implementation of InterpreterCodelet

void InterpreterCodelet::initialize(const char* description, Bytecodes::Code bytecode) {
  _description       = description;
  _bytecode          = bytecode;
}

void InterpreterCodelet::verify() { }

void InterpreterCodelet::print_on(outputStream* st) const {
  ttyLocker ttyl;

  if (PrintInterpreter) {
    st->cr();
    st->print_cr("----------------------------------------------------------------------");
  }

  if (description() != NULL) st->print("%s  ", description());
  if (bytecode()    >= 0   ) st->print("%d %s  ", bytecode(), Bytecodes::name(bytecode()));
  st->print_cr("[" INTPTR_FORMAT ", " INTPTR_FORMAT "]  %d bytes", p2i(code_begin()), p2i(code_end()), code_size());

  if (PrintInterpreter) {
    st->cr();
    Disassembler::decode(code_begin(), code_end(), st, CodeStrings());
  }
}

CodeletMark::CodeletMark(InterpreterMacroAssembler*& masm, const char* description, Bytecodes::Code bytecode) :
  _clet((InterpreterCodelet*)AbstractInterpreter::code()->request(codelet_size())),
  _cb(_clet->code_begin(), _clet->code_size()) {

  // Initialize Codelet attributes.
  _clet->initialize(description, bytecode);
  // Create assembler for code generation.
  masm = new InterpreterMacroAssembler(&_cb);
  _masm = &masm;
}

CodeletMark::~CodeletMark() {
  // Align so printing shows nop's instead of random code at the end (Codelets are aligned).
  (*_masm)->align(wordSize);
  // Make sure all code is in code buffer.
  (*_masm)->flush();

  // Commit Codelet.
  int committed_code_size = (*_masm)->code()->pure_insts_size();
  if (committed_code_size) {
    AbstractInterpreter::code()->commit(committed_code_size, (*_masm)->code()->strings());
  }
  // Make sure nobody can use _masm outside a CodeletMark lifespan.
  *_masm = NULL;
}

void interpreter_init() {
  Interpreter::initialize();
  // need to hit every safepoint in order to call zapping routine
  // register the interpreter
  Forte::register_stub("Interpreter", AbstractInterpreter::code()->code_start(), AbstractInterpreter::code()->code_end());
}
