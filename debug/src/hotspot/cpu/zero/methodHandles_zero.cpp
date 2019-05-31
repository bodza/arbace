#include "precompiled.hpp"
#include "interpreter/cppInterpreterGenerator.hpp"
#include "interpreter/interpreter.hpp"
#include "interpreter/interpreterRuntime.hpp"
#include "memory/allocation.inline.hpp"
#include "memory/resourceArea.hpp"
#include "oops/method.inline.hpp"
#include "oops/oop.inline.hpp"
#include "runtime/frame.inline.hpp"
#include "prims/methodHandles.hpp"

void MethodHandles::invoke_target(Method* method, TRAPS) {

  JavaThread *thread = (JavaThread *) THREAD;
  ZeroStack *stack = thread->zero_stack();
  InterpreterFrame *frame = thread->top_zero_frame()->as_interpreter_frame();
  interpreterState istate = frame->interpreter_state();

  // Trim back the stack to put the parameters at the top
  stack->set_sp(istate->stack() + 1);

  Interpreter::invoke_method(method, method->from_interpreted_entry(), THREAD);

  // Convert the result
  istate->set_stack(stack->sp() - 1);
}

oop MethodHandles::popFromStack(TRAPS) {

  JavaThread *thread = (JavaThread *) THREAD;
  InterpreterFrame *frame = thread->top_zero_frame()->as_interpreter_frame();
  interpreterState istate = frame->interpreter_state();
  intptr_t* topOfStack = istate->stack();

  oop top = STACK_OBJECT(-1);
  MORE_STACK(-1);
  istate->set_stack(topOfStack);

  return top;
}

void MethodHandles::throw_AME(Klass* rcvr, Method* interface_method, TRAPS) {

  JavaThread *thread = (JavaThread *) THREAD;
  // Set up the frame anchor if it isn't already
  bool has_last_Java_frame = thread->has_last_Java_frame();
  if (!has_last_Java_frame) {
    intptr_t *sp = thread->zero_stack()->sp();
    ZeroFrame *frame = thread->top_zero_frame();
    while (frame) {
      if (frame->is_interpreter_frame()) {
        interpreterState istate = frame->as_interpreter_frame()->interpreter_state();
        if (istate->self_link() == istate)
          break;
      }

      sp = ((intptr_t *) frame) + 1;
      frame = frame->next();
    }

    thread->set_last_Java_frame(frame, sp);
  }
  InterpreterRuntime::throw_AbstractMethodErrorVerbose(thread, rcvr, interface_method);
  // Reset the frame anchor if necessary
  if (!has_last_Java_frame) {
    thread->reset_last_Java_frame();
  }
}

int MethodHandles::method_handle_entry_invokeBasic(Method* method, intptr_t UNUSED, TRAPS) {

  JavaThread *thread = (JavaThread *) THREAD;
  InterpreterFrame *frame = thread->top_zero_frame()->as_interpreter_frame();
  interpreterState istate = frame->interpreter_state();
  intptr_t* topOfStack = istate->stack();

  // 'this' is a MethodHandle. We resolve the target method by accessing this.form.vmentry.vmtarget.
  int numArgs = method->size_of_parameters();
  oop lform1 = java_lang_invoke_MethodHandle::form(STACK_OBJECT(-numArgs)); // this.form
  oop vmEntry1 = java_lang_invoke_LambdaForm::vmentry(lform1);
  Method* vmtarget = (Method*) java_lang_invoke_MemberName::vmtarget(vmEntry1);

  invoke_target(vmtarget, THREAD);

  // No deoptimized frames on the stack
  return 0;
}

int MethodHandles::method_handle_entry_linkToStaticOrSpecial(Method* method, intptr_t UNUSED, TRAPS) {

  // Pop appendix argument from stack. This is a MemberName which we resolve to the
  // target method.
  oop vmentry = popFromStack(THREAD);

  Method* vmtarget = (Method*) java_lang_invoke_MemberName::vmtarget(vmentry);

  invoke_target(vmtarget, THREAD);

  return 0;
}

int MethodHandles::method_handle_entry_linkToInterface(Method* method, intptr_t UNUSED, TRAPS) {
  JavaThread *thread = (JavaThread *) THREAD;
  InterpreterFrame *frame = thread->top_zero_frame()->as_interpreter_frame();
  interpreterState istate = frame->interpreter_state();

  // Pop appendix argument from stack. This is a MemberName which we resolve to the
  // target method.
  oop vmentry = popFromStack(THREAD);
  intptr_t* topOfStack = istate->stack();

  // Resolve target method by looking up in the receiver object's itable.
  Klass* clazz = java_lang_Class::as_Klass(java_lang_invoke_MemberName::clazz(vmentry));
  intptr_t vmindex = java_lang_invoke_MemberName::vmindex(vmentry);
  Method* target = (Method*) java_lang_invoke_MemberName::vmtarget(vmentry);

  int numArgs = target->size_of_parameters();
  oop recv = STACK_OBJECT(-numArgs);

  InstanceKlass* klass_part = InstanceKlass::cast(recv->klass());
  itableOffsetEntry* ki = (itableOffsetEntry*) klass_part->start_of_itable();
  int i;
  for ( i = 0; i < klass_part->itable_length(); i++, ki++ ) {
    if (ki->interface_klass() == clazz) break;
  }

  itableMethodEntry* im = ki->first_method_entry(recv->klass());
  Method* vmtarget = im[vmindex].method();
  // Check that the vmtarget entry is non-null.  A null entry means
  // that the method no longer exists (got deleted) or is private.
  // Private class methods can never be an implementation of an
  // interface method. In those cases, throw AME.
  if (vmtarget != NULL) {
    invoke_target(vmtarget, THREAD);
  } else {
    throw_AME(recv->klass(), target, THREAD);
  }

  return 0;
}

int MethodHandles::method_handle_entry_linkToVirtual(Method* method, intptr_t UNUSED, TRAPS) {
  JavaThread *thread = (JavaThread *) THREAD;

  InterpreterFrame *frame = thread->top_zero_frame()->as_interpreter_frame();
  interpreterState istate = frame->interpreter_state();

  // Pop appendix argument from stack. This is a MemberName which we resolve to the
  // target method.
  oop vmentry = popFromStack(THREAD);
  intptr_t* topOfStack = istate->stack();

  // Resolve target method by looking up in the receiver object's vtable.
  intptr_t vmindex = java_lang_invoke_MemberName::vmindex(vmentry);
  Method* target = (Method*) java_lang_invoke_MemberName::vmtarget(vmentry);
  int numArgs = target->size_of_parameters();
  oop recv = STACK_OBJECT(-numArgs);
  Klass* clazz = recv->klass();
  Klass* klass_part = InstanceKlass::cast(clazz);
  ResourceMark rm(THREAD);
  klassVtable vtable = klass_part->vtable();
  Method* vmtarget = vtable.method_at(vmindex);

  invoke_target(vmtarget, THREAD);

  return 0;
}

int MethodHandles::method_handle_entry_invalid(Method* method, intptr_t UNUSED, TRAPS) {
  ShouldNotReachHere();
  return 0;
}

address MethodHandles::generate_method_handle_interpreter_entry(MacroAssembler* masm, vmIntrinsics::ID iid) {
  switch (iid) {
  case vmIntrinsics::_invokeGeneric:
  case vmIntrinsics::_compiledLambdaForm:
    // Perhaps surprisingly, the symbolic references visible to Java are not directly used.
    // They are linked to Java-generated adapters via MethodHandleNatives.linkMethod.
    // They all allow an appendix argument.
    return CppInterpreterGenerator::generate_entry_impl(masm, (address) MethodHandles::method_handle_entry_invalid);
  case vmIntrinsics::_invokeBasic:
    return CppInterpreterGenerator::generate_entry_impl(masm, (address) MethodHandles::method_handle_entry_invokeBasic);
  case vmIntrinsics::_linkToStatic:
  case vmIntrinsics::_linkToSpecial:
    return CppInterpreterGenerator::generate_entry_impl(masm, (address) MethodHandles::method_handle_entry_linkToStaticOrSpecial);
  case vmIntrinsics::_linkToInterface:
    return CppInterpreterGenerator::generate_entry_impl(masm, (address) MethodHandles::method_handle_entry_linkToInterface);
  case vmIntrinsics::_linkToVirtual:
    return CppInterpreterGenerator::generate_entry_impl(masm, (address) MethodHandles::method_handle_entry_linkToVirtual);
  default:
    ShouldNotReachHere();
    return NULL;
  }
}
