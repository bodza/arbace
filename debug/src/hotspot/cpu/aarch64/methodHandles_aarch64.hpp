// Platform-specific definitions for method handles.
// These definitions are inlined into class MethodHandles.

// Adapters
enum /* platform_dependent_constants */ {
  adapter_code_size = 32000
};

public:

  static void load_klass_from_Class(MacroAssembler* _masm, Register klass_reg);

  static void verify_klass(MacroAssembler* _masm,
                           Register obj, SystemDictionary::WKID klass_id,
                           const char* error_message = "wrong klass") { };

  static void verify_method_handle(MacroAssembler* _masm, Register mh_reg) {
    verify_klass(_masm, mh_reg, SystemDictionary::WK_KLASS_ENUM_NAME(java_lang_invoke_MethodHandle),
                 "reference is a MH");
  }

  static void verify_ref_kind(MacroAssembler* _masm, int ref_kind, Register member_reg, Register temp) { };

  // Similar to InterpreterMacroAssembler::jump_from_interpreted.
  // Takes care of special dispatch from single stepping too.
  static void jump_from_method_handle(MacroAssembler* _masm, Register method, Register temp,
                                      bool for_compiler_entry);

  static void jump_to_lambda_form(MacroAssembler* _masm,
                                  Register recv, Register method_temp,
                                  Register temp2,
                                  bool for_compiler_entry);

  static Register saved_last_sp_register() {
    // Should be in sharedRuntime, not here.
    return noreg;
  }
