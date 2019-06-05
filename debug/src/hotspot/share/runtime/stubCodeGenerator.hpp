#ifndef SHARE_VM_RUNTIME_STUBCODEGENERATOR_HPP
#define SHARE_VM_RUNTIME_STUBCODEGENERATOR_HPP

#include "asm/assembler.hpp"
#include "memory/allocation.hpp"

// All the basic framework for stub code generation/debugging/printing.

// A StubCodeDesc describes a piece of generated code (usually stubs).
// This information is mainly useful for debugging and printing.
// Currently, code descriptors are simply chained in a linked list,
// this may have to change if searching becomes too slow.

class StubCodeDesc: public CHeapObj<mtCode> {
 private:
  static StubCodeDesc* _list;                  // the list of all descriptors
  static bool          _frozen;                // determines whether _list modifications are allowed

  StubCodeDesc*        _next;                  // the next element in the linked list
  const char*          _group;                 // the group to which the stub code belongs
  const char*          _name;                  // the name assigned to the stub code
  address              _begin;                 // points to the first byte of the stub code    (included)
  address              _end;                   // points to the first byte after the stub code (excluded)

  void set_end(address end) {
    _end = end;
  }

  void set_begin(address begin) {
    _begin = begin;
  }

  friend class StubCodeMark;
  friend class StubCodeGenerator;

 public:
  static StubCodeDesc* first() { return _list; }
  static StubCodeDesc* next(StubCodeDesc* desc)  { return desc->_next; }

  static StubCodeDesc* desc_for(address pc);     // returns the code descriptor for the code containing pc or NULL
  static const char*   name_for(address pc);     // returns the name of the code containing pc or NULL

  StubCodeDesc(const char* group, const char* name, address begin, address end = NULL) {
    _next           = _list;
    _group          = group;
    _name           = name;
    _begin          = begin;
    _end            = end;
    _list           = this;
  };

  static void freeze();

  const char* group() const                      { return _group; }
  const char* name() const                       { return _name; }
  address     begin() const                      { return _begin; }
  address     end() const                        { return _end; }
  int         size_in_bytes() const              { return _end - _begin; }
  bool        contains(address pc) const         { return _begin <= pc && pc < _end; }
  void        print_on(outputStream* st) const;
  void        print() const                      { print_on(tty); }
};

// The base class for all stub-generating code generators.
// Provides utility functions.

class StubCodeGenerator: public StackObj {
 protected:
  MacroAssembler*  _masm;

 public:
  StubCodeGenerator(CodeBuffer* code);
  ~StubCodeGenerator();

  MacroAssembler* assembler() const              { return _masm; }

  virtual void stub_prolog(StubCodeDesc* cdesc); // called by StubCodeMark constructor
  virtual void stub_epilog(StubCodeDesc* cdesc); // called by StubCodeMark destructor
};

// Stack-allocated helper class used to associate a stub code with a name.
// All stub code generating functions that use a StubCodeMark will be registered
// in the global StubCodeDesc list and the generated stub code can be identified
// later via an address pointing into it.

class StubCodeMark: public StackObj {
 private:
  StubCodeGenerator* _cgen;
  StubCodeDesc*      _cdesc;

 public:
  StubCodeMark(StubCodeGenerator* cgen, const char* group, const char* name);
  ~StubCodeMark();
};

#endif
