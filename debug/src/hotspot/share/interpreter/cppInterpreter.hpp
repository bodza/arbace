#ifndef SHARE_VM_INTERPRETER_CPPINTERPRETER_HPP
#define SHARE_VM_INTERPRETER_CPPINTERPRETER_HPP

#include "interpreter/abstractInterpreter.hpp"
#include "utilities/macros.hpp"

#ifdef CC_INTERP

class InterpreterCodelet;

// This file contains the platform-independent parts
// of the c++ interpreter

class CppInterpreter: public AbstractInterpreter {
  friend class VMStructs;
 public:
  // Initialization/debugging
  static void       initialize();
  // this only returns whether a pc is within generated code for the interpreter.

  // These are moderately dubious interfaces for the c++ interpreter. Only
  // frame code and debug.cpp should be using it.
  static bool       contains(address pc);
  static InterpreterCodelet* codelet_containing(address pc);

 public:

  // No displatch table to switch so no need for these to do anything special
  static void notice_safepoints() {}
  static void ignore_safepoints() {}

  static address    return_entry  (TosState state, int length, Bytecodes::Code code);
  static address    deopt_entry   (TosState state, int length);

  static void invoke_method(Method* method, address entry_point, TRAPS);
  static void invoke_osr(Method* method,
                         address   entry_point,
                         address   osr_buf,
                         TRAPS);
#ifdef ZERO
# include "cppInterpreter_zero.hpp"
#endif
};

#endif

#endif
