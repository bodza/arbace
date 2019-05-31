#include "precompiled.hpp"
#include "asm/assembler.hpp"
#include "interpreter/bytecodeHistogram.hpp"
#include "interpreter/cppInterpreterGenerator.hpp"
#include "interpreter/interpreterRuntime.hpp"
#include "oops/method.hpp"
#include "runtime/arguments.hpp"
#include "interpreter/cppInterpreter.hpp"

address CppInterpreterGenerator::generate_slow_signature_handler() {
  _masm->advance(1);
  return (address) InterpreterRuntime::slow_signature_handler;
}

address CppInterpreterGenerator::generate_math_entry(AbstractInterpreter::MethodKind kind) {
  if (!InlineIntrinsics)
    return NULL;

  Unimplemented();
  return NULL;
}

address CppInterpreterGenerator::generate_abstract_entry() {
  return generate_entry((address) ShouldNotCallThisEntry());
}

address CppInterpreterGenerator::generate_empty_entry() {
  if (!UseFastEmptyMethods)
    return NULL;

  return generate_entry((address) CppInterpreter::empty_entry);
}

address CppInterpreterGenerator::generate_accessor_entry() {
  if (!UseFastAccessorMethods)
    return NULL;

  return generate_entry((address) CppInterpreter::accessor_entry);
}

address CppInterpreterGenerator::generate_Reference_get_entry(void) {
  if (UseG1GC) {
    // We need to generate have a routine that generates code to:
    //   * load the value in the referent field
    //   * passes that value to the pre-barrier.
    //
    // In the case of G1 this will record the value of the
    // referent in an SATB buffer if marking is active.
    // This will cause concurrent marking to mark the referent
    // field as live.
    Unimplemented();
  }

  // If G1 is not enabled then attempt to go through the normal entry point
  // Reference.get could be instrumented by jvmti
  return NULL;
}

address CppInterpreterGenerator::generate_native_entry(bool synchronized) {
  return generate_entry((address) CppInterpreter::native_entry);
}

address CppInterpreterGenerator::generate_normal_entry(bool synchronized) {
  return generate_entry((address) CppInterpreter::normal_entry);
}
