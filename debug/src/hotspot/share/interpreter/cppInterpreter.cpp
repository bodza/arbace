#include "precompiled.hpp"
#include "interpreter/bytecodeInterpreter.hpp"
#include "interpreter/cppInterpreterGenerator.hpp"
#include "interpreter/interpreter.hpp"
#include "interpreter/interpreterRuntime.hpp"
#include "memory/resourceArea.hpp"
#include "runtime/timerTrace.hpp"

#ifdef CC_INTERP

#ifdef ZERO
# include "entry_zero.hpp"
#else
#error "Only Zero CppInterpreter is supported"
#endif

void CppInterpreter::initialize() {
  if (_code != NULL) return;
  AbstractInterpreter::initialize();

  // generate interpreter
  { ResourceMark rm;
    TraceTime timer("Interpreter generation", TRACETIME_LOG(Info, startuptime));
    int code_size = InterpreterCodeSize;
    _code = new StubQueue(new InterpreterCodeletInterface, code_size, NULL,
                           "Interpreter");
    CppInterpreterGenerator g(_code);
    if (PrintInterpreter) print();
  }

  // Allow c++ interpreter to do one initialization now that switches are set, etc.
  BytecodeInterpreter start_msg(BytecodeInterpreter::initialize);
  BytecodeInterpreter::run(&start_msg);
}

void CppInterpreter::invoke_method(Method* method, address entry_point, TRAPS) {
  ((ZeroEntry *) entry_point)->invoke(method, THREAD);
}

void CppInterpreter::invoke_osr(Method* method, address entry_point, address osr_buf, TRAPS) {
  ((ZeroEntry *) entry_point)->invoke_osr(method, osr_buf, THREAD);
}

InterpreterCodelet* CppInterpreter::codelet_containing(address pc) {
  // FIXME: I'm pretty sure _code is null and this is never called, which is why it's copied.
  return (InterpreterCodelet*)_code->stub_containing(pc);
}

#endif
