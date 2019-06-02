#include "precompiled.hpp"

#include "asm/macroAssembler.hpp"
#include "asm/macroAssembler.inline.hpp"
#include "code/codeCache.hpp"
#include "compiler/disassembler.hpp"
#include "oops/oop.inline.hpp"
#include "prims/forte.hpp"
#include "runtime/stubCodeGenerator.hpp"

// Implementation of StubCodeDesc

StubCodeDesc* StubCodeDesc::_list = NULL;
bool          StubCodeDesc::_frozen = false;

StubCodeDesc* StubCodeDesc::desc_for(address pc) {
  StubCodeDesc* p = _list;
  while (p != NULL && !p->contains(pc)) {
    p = p->_next;
  }
  return p;
}

const char* StubCodeDesc::name_for(address pc) {
  StubCodeDesc* p = desc_for(pc);
  return p == NULL ? NULL : p->name();
}

void StubCodeDesc::freeze() {
  _frozen = true;
}

void StubCodeDesc::print_on(outputStream* st) const {
  st->print("%s", group());
  st->print("::");
  st->print("%s", name());
  st->print(" [" INTPTR_FORMAT ", " INTPTR_FORMAT "[ (%d bytes)", p2i(begin()), p2i(end()), size_in_bytes());
}

// Implementation of StubCodeGenerator

StubCodeGenerator::StubCodeGenerator(CodeBuffer* code, bool print_code) {
  _masm = new MacroAssembler(code );
  _print_code = print_code;
}

StubCodeGenerator::~StubCodeGenerator() {
  if (_print_code) {
    CodeBuffer* cbuf = _masm->code();
    CodeBlob*   blob = CodeCache::find_blob_unsafe(cbuf->insts()->start());
    if (blob != NULL) {
      blob->set_strings(cbuf->strings());
    }
  }
}

void StubCodeGenerator::stub_prolog(StubCodeDesc* cdesc) {
  // default implementation - do nothing
}

void StubCodeGenerator::stub_epilog(StubCodeDesc* cdesc) {
  if (_print_code) {
    CodeStrings cs;
    ptrdiff_t offset = 0;
    cdesc->print();
    tty->cr();
    Disassembler::decode(cdesc->begin(), cdesc->end(), NULL, cs, offset);
    tty->cr();
  }
}

// Implementation of CodeMark

StubCodeMark::StubCodeMark(StubCodeGenerator* cgen, const char* group, const char* name) {
  _cgen  = cgen;
  _cdesc = new StubCodeDesc(group, name, _cgen->assembler()->pc());
  _cgen->stub_prolog(_cdesc);
  // define the stub's beginning (= entry point) to be after the prolog:
  _cdesc->set_begin(_cgen->assembler()->pc());
}

StubCodeMark::~StubCodeMark() {
  _cgen->assembler()->flush();
  _cdesc->set_end(_cgen->assembler()->pc());
  _cgen->stub_epilog(_cdesc);
  Forte::register_stub(_cdesc->name(), _cdesc->begin(), _cdesc->end());
}
