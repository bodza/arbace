#ifndef SHARE_VM_INTERPRETER_BYTECODETRACER_HPP
#define SHARE_VM_INTERPRETER_BYTECODETRACER_HPP

#include "memory/allocation.hpp"
#include "utilities/ostream.hpp"

// The BytecodeTracer is a helper class used by the interpreter for run-time
// bytecode tracing. If bytecode tracing is turned on, trace() will be called
// for each bytecode.
//
// By specialising the BytecodeClosure, all kinds of bytecode traces can
// be done.

// class BytecodeTracer is used by false option and false

class BytecodeClosure;
class BytecodeTracer: AllStatic {
 private:
  static BytecodeClosure* _closure;

 public:
  static BytecodeClosure* std_closure();                        // a printing closure
  static BytecodeClosure* closure()                                                   { return _closure; }
  static void             set_closure(BytecodeClosure* closure) { _closure = closure; }

  static void             trace(const methodHandle& method, address bcp, uintptr_t tos, uintptr_t tos2, outputStream* st = tty);
  static void             trace(const methodHandle& method, address bcp, outputStream* st = tty);
};

// For each bytecode, a BytecodeClosure's trace() routine will be called.

class BytecodeClosure {
 public:
  virtual void trace(const methodHandle& method, address bcp, uintptr_t tos, uintptr_t tos2, outputStream* st) = 0;
  virtual void trace(const methodHandle& method, address bcp, outputStream* st) = 0;
};

#endif
