#ifndef CPU_X86_VM_C1_LIRASSEMBLER_X86_HPP
#define CPU_X86_VM_C1_LIRASSEMBLER_X86_HPP

 private:
  Address::ScaleFactor array_element_size(BasicType type) const;

  void arith_fpu_implementation(LIR_Code code, int left_index, int right_index, int dest_index, bool pop_fpu_stack);

  // helper functions which checks for overflow and sets bailout if it
  // occurs.  Always returns a valid embeddable pointer but in the
  // bailout case the pointer won't be to unique storage.
  address float_constant(float f);
  address double_constant(double d);

  bool is_literal_address(LIR_Address* addr);

  // When we need to use something other than rscratch1 use this
  // method.
  Address as_Address(LIR_Address* addr, Register tmp);

  // Record the type of the receiver in ReceiverTypeData
  void type_profile_helper(Register mdo, ciMethodData *md, ciProfileData *data, Register recv, Label* update_done);

  enum {
    _call_stub_size = 28,
    _call_aot_stub_size = 12,
    _exception_handler_size = 175,
    _deopt_handler_size = 17
  };

public:
  void store_parameter(Register r,  int offset_from_esp_in_words);
  void store_parameter(jint c,      int offset_from_esp_in_words);
  void store_parameter(jobject c,   int offset_from_esp_in_words);
  void store_parameter(Metadata* c, int offset_from_esp_in_words);

#endif
