#ifndef SHARE_VM_C1_C1_COMPILER_HPP
#define SHARE_VM_C1_C1_COMPILER_HPP

#include "compiler/abstractCompiler.hpp"
#include "compiler/compilerDirectives.hpp"

// There is one instance of the Compiler per CompilerThread.

class Compiler: public AbstractCompiler {
 private:
  static void init_c1_runtime();
  BufferBlob* init_buffer_blob();

 public:
  // Creation
  Compiler();
  ~Compiler();

  // Name of this compiler
  virtual const char* name()     { return "C1"; }

  // Missing feature tests
  virtual bool supports_native() { return true; }

  // Initialization
  virtual void initialize();

  // Compilation entry point for methods
  virtual void compile_method(ciEnv* env, ciMethod* target, int entry_bci, DirectiveSet* directive);

  // Check if the C1 compiler supports an intrinsic for 'method'.
  virtual bool is_intrinsic_supported(const methodHandle& method);

  // Size of the code buffer
  static int code_buffer_size();
};

#endif
