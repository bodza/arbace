#ifndef CPU_AARCH64_VM_INTERPRETERRT_AARCH64_HPP
#define CPU_AARCH64_VM_INTERPRETERRT_AARCH64_HPP

// This is included in the middle of class Interpreter.
// Do not include files here.

// native method calls

class SignatureHandlerGenerator: public NativeSignatureIterator {
 private:
  MacroAssembler* _masm;
  unsigned int _call_format;
  unsigned int _num_fp_args;
  unsigned int _num_int_args;
  int _stack_offset;

  void pass_int();
  void pass_long();
  void pass_float();
  void pass_double();
  void pass_object();

 public:
  // Creation
  SignatureHandlerGenerator(const methodHandle& method, CodeBuffer* buffer);

  // Code generation
  void generate(uint64_t fingerprint);

  // Code generation support
  static Register from();
  static Register to();
  static Register temp();
};

#endif
