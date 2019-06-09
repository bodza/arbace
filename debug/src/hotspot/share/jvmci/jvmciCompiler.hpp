#ifndef SHARE_VM_JVMCI_JVMCI_COMPILER_HPP
#define SHARE_VM_JVMCI_JVMCI_COMPILER_HPP

#include "compiler/abstractCompiler.hpp"
#include "jvmci/jvmciEnv.hpp"
#include "utilities/exceptions.hpp"

class JVMCICompiler : public AbstractCompiler {
private:
  bool _bootstrapping;

  /**
   * True if we have seen a bootstrap compilation request.
   */
  volatile bool _bootstrap_compilation_request_handled;

  /**
   * Number of methods successfully compiled by a call to
   * JVMCICompiler::compile_method().
   */
  volatile int _methods_compiled;

  static JVMCICompiler* _instance;

  static elapsedTimer _codeInstallTimer;

  /**
   * Exits the VM due to an unexpected exception.
   */
  static void exit_on_pending_exception(oop exception, const char* message);

public:
  JVMCICompiler();

  static JVMCICompiler* instance(bool require_non_null, TRAPS) {
    if (!EnableJVMCI) {
      THROW_MSG_NULL(vmSymbols::java_lang_InternalError(), "JVMCI is not enabled")
    }
    if (_instance == NULL && require_non_null) {
      THROW_MSG_NULL(vmSymbols::java_lang_InternalError(), "The JVMCI compiler instance has not been created");
    }
    return _instance;
  }

  virtual const char* name()     { return "JVMCI"; }

  virtual bool supports_native() { return true; }

  bool is_jvmci()                { return true; }
  bool is_c1()                   { return false; }
  bool is_c2()                   { return false; }

  bool needs_stubs()             { return false; }

  // Initialization
  virtual void initialize();

  /**
   * Initialize the compile queue with the methods in java.lang.Object and
   * then wait until the queue is empty.
   */
  void bootstrap(TRAPS);

  bool is_bootstrapping() const { return _bootstrapping; }

  // Compilation entry point for methods
  virtual void compile_method(ciEnv* env, ciMethod* target, int entry_bci, DirectiveSet* directive);

  void compile_method(const methodHandle& target, int entry_bci, JVMCIEnv* env);

  /**
   * Gets the number of methods that have been successfully compiled by
   * a call to JVMCICompiler::compile_method().
   */
  int methods_compiled() { return _methods_compiled; }

  static elapsedTimer* codeInstallTimer() { return &_codeInstallTimer; }
};

#endif
