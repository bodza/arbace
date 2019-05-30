#ifndef CPU_X86_VM_INTERPRETERRT_X86_HPP
#define CPU_X86_VM_INTERPRETERRT_X86_HPP

// This is included in the middle of class Interpreter.
// Do not include files here.

// native method calls

class SignatureHandlerGenerator: public NativeSignatureIterator {
 private:
  MacroAssembler* _masm;
#ifdef AMD64
  unsigned int _num_fp_args;
  unsigned int _num_int_args;
  int _stack_offset;
#else
  void move(int from_offset, int to_offset);
  void box(int from_offset, int to_offset);
#endif

  void pass_int();
  void pass_long();
  void pass_float();
#ifdef AMD64
  void pass_double();
#endif
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
