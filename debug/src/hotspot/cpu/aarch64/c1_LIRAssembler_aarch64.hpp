#ifndef CPU_AARCH64_VM_C1_LIRASSEMBLER_AARCH64_HPP
#define CPU_AARCH64_VM_C1_LIRASSEMBLER_AARCH64_HPP

// ArrayCopyStub needs access to bailout
friend class ArrayCopyStub;

 private:

  int array_element_size(BasicType type) const;

  void arith_fpu_implementation(LIR_Code code, int left_index, int right_index, int dest_index, bool pop_fpu_stack);

  // helper functions which checks for overflow and sets bailout if it
  // occurs.  Always returns a valid embeddable pointer but in the
  // bailout case the pointer won't be to unique storage.
  address float_constant(float f);
  address double_constant(double d);

  address int_constant(jlong n);

  bool is_literal_address(LIR_Address* addr);

  // When we need to use something other than rscratch1 use this
  // method.
  Address as_Address(LIR_Address* addr, Register tmp);

  // Record the type of the receiver in ReceiverTypeData
  void type_profile_helper(Register mdo,
                           ciMethodData *md, ciProfileData *data,
                           Register recv, Label* update_done);
  void add_debug_info_for_branch(address adr, CodeEmitInfo* info);

  void casw(Register addr, Register newval, Register cmpval);
  void casl(Register addr, Register newval, Register cmpval);

  void poll_for_safepoint(relocInfo::relocType rtype, CodeEmitInfo* info = NULL);

  static const int max_tableswitches = 20;
  struct tableswitch switches[max_tableswitches];
  int tableswitch_count;

  void init() { tableswitch_count = 0; }

  void deoptimize_trap(CodeEmitInfo *info);

  enum {
    _call_stub_size = 12 * NativeInstruction::instruction_size,
    _call_aot_stub_size = 0,
    _exception_handler_size = 175,
    _deopt_handler_size = 7 * NativeInstruction::instruction_size
  };

  void arithmetic_idiv(LIR_Op3* op, bool is_irem);

public:

  void store_parameter(Register r, int offset_from_esp_in_words);
  void store_parameter(jint c,     int offset_from_esp_in_words);
  void store_parameter(jobject c,  int offset_from_esp_in_words);

#endif
