#ifndef SHARE_VM_INTERPRETER_CPPINTERPRETERGENERATOR_HPP
#define SHARE_VM_INTERPRETER_CPPINTERPRETERGENERATOR_HPP

// This file contains the platform-independent parts
// of the template interpreter generator.

#ifdef CC_INTERP
#ifdef ZERO
# include "entry_zero.hpp"
# include "interpreter/interp_masm.hpp"
#endif

class CppInterpreterGenerator: public AbstractInterpreterGenerator {

 private:
  void generate_all();

  address generate_slow_signature_handler();

  address generate_method_entry(AbstractInterpreter::MethodKind kind);
  address generate_normal_entry(bool synchronized);
  address generate_native_entry(bool synchronized);
  address generate_abstract_entry();
  address generate_math_entry(AbstractInterpreter::MethodKind kind);
  address generate_empty_entry();
  address generate_accessor_entry();
  address generate_Reference_get_entry();

 public:
  CppInterpreterGenerator(StubQueue* _code);

#ifdef ZERO
 protected:
  MacroAssembler* assembler() const {
    return _masm;
  }

 public:
  static address generate_entry_impl(MacroAssembler* masm, address entry_point) {
    ZeroEntry *entry = (ZeroEntry *) masm->pc();
    masm->advance(sizeof(ZeroEntry));
    entry->set_entry_point(entry_point);
    return (address) entry;
  }

 protected:
  address generate_entry(address entry_point) {
    return generate_entry_impl(assembler(), entry_point);
  }
#endif
};

#endif
#endif
