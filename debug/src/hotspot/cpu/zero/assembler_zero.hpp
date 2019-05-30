#ifndef CPU_ZERO_VM_ASSEMBLER_ZERO_HPP
#define CPU_ZERO_VM_ASSEMBLER_ZERO_HPP

// In normal, CPU-specific ports of HotSpot these two classes are used
// for generating assembly language.  We don't do any of this in zero,
// of course, but we do sneak entry points around in CodeBuffers so we
// generate those here.

class Assembler : public AbstractAssembler {
 public:
  Assembler(CodeBuffer* code) : AbstractAssembler(code) {}

 public:
  void pd_patch_instruction(address branch, address target);
};

class MacroAssembler : public Assembler {
 public:
  MacroAssembler(CodeBuffer* code) : Assembler(code) {}

 public:
  void align(int modulus);
  void bang_stack_with_offset(int offset);
  bool needs_explicit_null_check(intptr_t offset);
  RegisterOrConstant delayed_value_impl(intptr_t* delayed_value_addr, Register tmp, int offset);
 public:
  void advance(int bytes);
  void store_oop(jobject obj);
  void store_Metadata(Metadata* obj);
};

address ShouldNotCallThisStub();
address ShouldNotCallThisEntry();

#endif
