#include "precompiled.hpp"
#include "interpreter/bytecodeInterpreter.hpp"
#include "interpreter/cppInterpreterGenerator.hpp"
#include "interpreter/interpreter.hpp"
#include "interpreter/interpreterRuntime.hpp"

#ifdef CC_INTERP

CppInterpreterGenerator::CppInterpreterGenerator(StubQueue* _code): AbstractInterpreterGenerator(_code) {
  generate_all();
}

void CppInterpreterGenerator::generate_all() {
  { CodeletMark cm(_masm, "slow signature handler");
    AbstractInterpreter::_slow_signature_handler = generate_slow_signature_handler();
  }

#define method_entry(kind) Interpreter::_entry_table[Interpreter::kind] = generate_method_entry(Interpreter::kind)

  { CodeletMark cm(_masm, "(kind = frame_manager)");
    // all non-native method kinds
    method_entry(zerolocals);
    method_entry(zerolocals_synchronized);
    method_entry(empty);
    method_entry(accessor);
    method_entry(abstract);
    method_entry(java_lang_math_sin   );
    method_entry(java_lang_math_cos   );
    method_entry(java_lang_math_tan   );
    method_entry(java_lang_math_abs   );
    method_entry(java_lang_math_sqrt  );
    method_entry(java_lang_math_log   );
    method_entry(java_lang_math_log10 );
    method_entry(java_lang_math_pow );
    method_entry(java_lang_math_exp );
    method_entry(java_lang_math_fmaD );
    method_entry(java_lang_math_fmaF );
    method_entry(java_lang_ref_reference_get);

    AbstractInterpreter::initialize_method_handle_entries();

    Interpreter::_native_entry_begin = Interpreter::code()->code_end();
    method_entry(native);
    method_entry(native_synchronized);
    Interpreter::_native_entry_end = Interpreter::code()->code_end();
  }

#undef method_entry
}

// Generate method entries
address CppInterpreterGenerator::generate_method_entry(
                                        AbstractInterpreter::MethodKind kind) {
  // determine code generation flags
  bool native = false;
  bool synchronized = false;
  address entry_point = NULL;

  switch (kind) {
  case Interpreter::zerolocals             :                                          break;
  case Interpreter::zerolocals_synchronized:                synchronized = true;      break;
  case Interpreter::native                 : native = true;                           break;
  case Interpreter::native_synchronized    : native = true; synchronized = true;      break;
  case Interpreter::empty                  : entry_point = generate_empty_entry();    break;
  case Interpreter::accessor               : entry_point = generate_accessor_entry(); break;
  case Interpreter::abstract               : entry_point = generate_abstract_entry(); break;

  case Interpreter::java_lang_math_sin     : // fall thru
  case Interpreter::java_lang_math_cos     : // fall thru
  case Interpreter::java_lang_math_tan     : // fall thru
  case Interpreter::java_lang_math_abs     : // fall thru
  case Interpreter::java_lang_math_log     : // fall thru
  case Interpreter::java_lang_math_log10   : // fall thru
  case Interpreter::java_lang_math_sqrt    : // fall thru
  case Interpreter::java_lang_math_pow     : // fall thru
  case Interpreter::java_lang_math_exp     : // fall thru
  case Interpreter::java_lang_math_fmaD    : // fall thru
  case Interpreter::java_lang_math_fmaF    : entry_point = generate_math_entry(kind);      break;
  case Interpreter::java_lang_ref_reference_get
                                           : entry_point = generate_Reference_get_entry(); break;
  default:
    fatal("unexpected method kind: %d", kind);
    break;
  }

  if (entry_point) {
    return entry_point;
  }

  // We expect the normal and native entry points to be generated first so we can reuse them.
  if (native) {
    entry_point = Interpreter::entry_for_kind(synchronized ? Interpreter::native_synchronized : Interpreter::native);
    if (entry_point == NULL) {
      entry_point = generate_native_entry(synchronized);
    }
  } else {
    entry_point = Interpreter::entry_for_kind(synchronized ? Interpreter::zerolocals_synchronized : Interpreter::zerolocals);
    if (entry_point == NULL) {
      entry_point = generate_normal_entry(synchronized);
    }
  }

  return entry_point;
}
#endif
