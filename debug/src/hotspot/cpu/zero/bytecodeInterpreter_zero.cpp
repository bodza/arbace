#include "precompiled.hpp"
#include "asm/assembler.hpp"
#include "interpreter/bytecodeInterpreter.hpp"
#include "interpreter/bytecodeInterpreter.inline.hpp"
#include "interpreter/interpreter.hpp"
#include "interpreter/interpreterRuntime.hpp"
#include "oops/methodData.hpp"
#include "oops/method.hpp"
#include "oops/oop.inline.hpp"
#include "runtime/deoptimization.hpp"
#include "runtime/frame.inline.hpp"
#include "runtime/sharedRuntime.hpp"
#include "runtime/stubRoutines.hpp"
#include "runtime/synchronizer.hpp"
#include "runtime/vframeArray.hpp"
#include "utilities/debug.hpp"

#ifdef CC_INTERP

const char *BytecodeInterpreter::name_of_field_at_address(address addr) {
#define DO(member) {if (addr == (address) &(member)) return XSTR(member);}
  DO(_thread);
  DO(_bcp);
  DO(_locals);
  DO(_constants);
  DO(_method);
  DO(_mirror);
  DO(_mdx);
  DO(_stack);
  DO(_msg);
  DO(_result);
  DO(_prev_link);
  DO(_oop_temp);
  DO(_stack_base);
  DO(_stack_limit);
  DO(_monitor_base);
  DO(_self_link);
#undef DO
  if (addr > (address) &_result && addr < (address) (&_result + 1))
    return "_result)";
  return NULL;
}

void BytecodeInterpreter::layout_interpreterState(interpreterState istate,
                                                  frame*    caller,
                                                  frame*    current,
                                                  Method* method,
                                                  intptr_t* locals,
                                                  intptr_t* stack,
                                                  intptr_t* stack_base,
                                                  intptr_t* monitor_base,
                                                  intptr_t* frame_bottom,
                                                  bool      is_top_frame) {
  istate->set_locals(locals);
  istate->set_method(method);
  istate->set_mirror(method->method_holder()->java_mirror());
  istate->set_self_link(istate);
  istate->set_prev_link(NULL);
  // thread will be set by a hacky repurposing of frame::patch_pc()
  // bcp will be set by vframeArrayElement::unpack_on_stack()
  istate->set_constants(method->constants()->cache());
  istate->set_msg(BytecodeInterpreter::method_resume);
  istate->set_bcp_advance(0);
  istate->set_oop_temp(NULL);
  istate->set_mdx(NULL);
  if (caller->is_interpreted_frame()) {
    interpreterState prev = caller->get_interpreterState();
    prev->set_callee(method);
    if (*prev->bcp() == Bytecodes::_invokeinterface)
      prev->set_bcp_advance(5);
    else
      prev->set_bcp_advance(3);
  }
  istate->set_callee(NULL);
  istate->set_monitor_base((BasicObjectLock *) monitor_base);
  istate->set_stack_base(stack_base);
  istate->set_stack(stack);
  istate->set_stack_limit(stack_base - method->max_stack() - 1);
}

#endif
