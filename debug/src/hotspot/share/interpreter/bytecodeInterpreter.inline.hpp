#ifndef SHARE_VM_INTERPRETER_BYTECODEINTERPRETER_INLINE_HPP
#define SHARE_VM_INTERPRETER_BYTECODEINTERPRETER_INLINE_HPP

#include "interpreter/bytecodeInterpreter.hpp"
#include "runtime/stubRoutines.hpp"
#include "utilities/macros.hpp"

// This file holds platform-independent bodies of inline functions for the C++ based interpreter

#ifdef CC_INTERP

#define VERIFY_OOP(o)

#ifdef ZERO
# include "bytecodeInterpreter_zero.inline.hpp"
#else
#error "Only Zero Bytecode Interpreter is supported"
#endif

#endif

#endif
