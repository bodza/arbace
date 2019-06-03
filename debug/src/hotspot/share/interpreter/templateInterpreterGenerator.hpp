#ifndef SHARE_VM_INTERPRETER_TEMPLATEINTERPRETERGENERATOR_HPP
#define SHARE_VM_INTERPRETER_TEMPLATEINTERPRETERGENERATOR_HPP

// This file contains the platform-independent parts
// of the template interpreter generator.

class TemplateInterpreterGenerator: public AbstractInterpreterGenerator {
 protected:
  // entry points for shared code sequence
  address _unimplemented_bytecode;
  address _illegal_bytecode_sequence;

  // shared code sequences
  // Converter for native abi result to tosca result
  address generate_result_handler_for(BasicType type);
  address generate_slow_signature_handler();
  address generate_error_exit(const char* msg);
  address generate_StackOverflowError_handler();
  address generate_exception_handler(const char* name, const char* message) {
    return generate_exception_handler_common(name, message, false);
  }
  address generate_klass_exception_handler(const char* name) {
    return generate_exception_handler_common(name, NULL, true);
  }
  address generate_exception_handler_common(const char* name, const char* message, bool pass_oop);
  address generate_ClassCastException_handler();
  address generate_ArrayIndexOutOfBounds_handler();
  address generate_return_entry_for(TosState state, int step, size_t index_size);
  address generate_deopt_entry_for(TosState state, int step, address continuation = NULL);
  address generate_safept_entry_for(TosState state, address runtime_entry);
  void    generate_throw_exception();

  void lock_method();

  void bang_stack_shadow_pages(bool native_call);

  // Instruction generation
  void generate_and_dispatch (Template* t, TosState tos_out = ilgl);
  void set_vtos_entry_points (Template* t, address& bep, address& cep, address& sep, address& aep, address& iep, address& lep, address& fep, address& dep, address& vep);
  void set_short_entry_points(Template* t, address& bep, address& cep, address& sep, address& aep, address& iep, address& lep, address& fep, address& dep, address& vep);
  void set_wide_entry_point  (Template* t, address& wep);

  void set_entry_points(Bytecodes::Code code);
  void set_unimplemented(int i);
  void set_entry_points_for_all_bytes();
  void set_safepoints_for_all_bytes();

  void generate_all();

  // entry point generator
  address generate_method_entry(AbstractInterpreter::MethodKind kind);

  address generate_normal_entry(bool synchronized);
  address generate_native_entry(bool synchronized);
  address generate_abstract_entry(void);
  address generate_math_entry(AbstractInterpreter::MethodKind kind);
  address generate_Reference_get_entry();
  address generate_CRC32_update_entry();
  address generate_CRC32_updateBytes_entry(AbstractInterpreter::MethodKind kind);
  address generate_CRC32C_updateBytes_entry(AbstractInterpreter::MethodKind kind);
#ifdef IA32
  address generate_Float_intBitsToFloat_entry();
  address generate_Float_floatToRawIntBits_entry();
  address generate_Double_longBitsToDouble_entry();
  address generate_Double_doubleToRawLongBits_entry();
#endif
  // Some platforms don't need registers, other need two. Unused function is
  // left unimplemented.
  void generate_stack_overflow_check(void);
  void generate_stack_overflow_check(Register Rframe_size, Register Rscratch);

  void generate_counter_incr(Label* overflow, Label* profile_method, Label* profile_method_continue);
  void generate_counter_overflow(Label& continue_entry);

  void generate_fixed_frame(bool native_call);

#ifdef AARCH64
  void generate_transcendental_entry(AbstractInterpreter::MethodKind kind, int fpargs);
#endif

 public:
  TemplateInterpreterGenerator(StubQueue* _code);
};

#endif
