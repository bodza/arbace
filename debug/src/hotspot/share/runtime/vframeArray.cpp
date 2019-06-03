#include "precompiled.hpp"

#include "classfile/vmSymbols.hpp"
#include "code/vmreg.inline.hpp"
#include "interpreter/bytecode.hpp"
#include "interpreter/interpreter.hpp"
#include "memory/allocation.inline.hpp"
#include "memory/resourceArea.hpp"
#include "memory/universe.hpp"
#include "oops/methodData.hpp"
#include "oops/oop.inline.hpp"
#include "runtime/frame.inline.hpp"
#include "runtime/handles.inline.hpp"
#include "runtime/monitorChunk.hpp"
#include "runtime/sharedRuntime.hpp"
#include "runtime/vframe.hpp"
#include "runtime/vframeArray.hpp"
#include "runtime/vframe_hp.hpp"
#include "utilities/copy.hpp"
#include "utilities/events.hpp"

int vframeArrayElement::bci(void) const { return (_bci == SynchronizationEntryBCI ? 0 : _bci); }

void vframeArrayElement::free_monitors(JavaThread* jt) {
  if (_monitors != NULL) {
     MonitorChunk* chunk = _monitors;
     _monitors = NULL;
     jt->remove_monitor_chunk(chunk);
     delete chunk;
  }
}

void vframeArrayElement::fill_in(compiledVFrame* vf, bool realloc_failures) {
// Copy the information from the compiled vframe to the
// interpreter frame we will be creating to replace vf

  _method = vf->method();
  _bci    = vf->raw_bci();
  _reexecute = vf->should_reexecute();

  int index;

  // Get the monitors off-stack

  GrowableArray<MonitorInfo*>* list = vf->monitors();
  if (list->is_empty()) {
    _monitors = NULL;
  } else {
    // Allocate monitor chunk
    _monitors = new MonitorChunk(list->length());
    vf->thread()->add_monitor_chunk(_monitors);

    // Migrate the BasicLocks from the stack to the monitor chunk
    for (index = 0; index < list->length(); index++) {
      MonitorInfo* monitor = list->at(index);
      BasicObjectLock* dest = _monitors->at(index);
      if (monitor->owner_is_scalar_replaced()) {
        dest->set_obj(NULL);
      } else {
        dest->set_obj(monitor->owner());
        monitor->lock()->move_to(monitor->owner(), dest->lock());
      }
    }
  }

  // Convert the vframe locals and expressions to off stack
  // values. Because we will not gc all oops can be converted to
  // intptr_t (i.e. a stack slot) and we are fine. This is
  // good since we are inside a HandleMark and the oops in our
  // collection would go away between packing them here and
  // unpacking them in unpack_on_stack.

  // First the locals go off-stack

  // FIXME this seems silly it creates a StackValueCollection
  // in order to get the size to then copy them and
  // convert the types to intptr_t size slots. Seems like it
  // could do it in place... Still uses less memory than the
  // old way though

  StackValueCollection *locs = vf->locals();
  _locals = new StackValueCollection(locs->size());
  for (index = 0; index < locs->size(); index++) {
    StackValue* value = locs->at(index);
    switch(value->type()) {
      case T_OBJECT:
        // preserve object type
        _locals->add( new StackValue(cast_from_oop<intptr_t>((value->get_obj()())), T_OBJECT ));
        break;
      case T_CONFLICT:
        // A dead local.  Will be initialized to null/zero.
        _locals->add( new StackValue());
        break;
      case T_INT:
        _locals->add( new StackValue(value->get_int()));
        break;
      default:
        ShouldNotReachHere();
    }
  }

  // Now the expressions off-stack
  // Same silliness as above

  StackValueCollection *exprs = vf->expressions();
  _expressions = new StackValueCollection(exprs->size());
  for (index = 0; index < exprs->size(); index++) {
    StackValue* value = exprs->at(index);
    switch(value->type()) {
      case T_OBJECT:
        // preserve object type
        _expressions->add( new StackValue(cast_from_oop<intptr_t>((value->get_obj()())), T_OBJECT ));
        break;
      case T_CONFLICT:
        // A dead stack element.  Will be initialized to null/zero.
        // This can occur when the compiler emits a state in which stack
        // elements are known to be dead (because of an imminent exception).
        _expressions->add( new StackValue());
        break;
      case T_INT:
        _expressions->add( new StackValue(value->get_int()));
        break;
      default:
        ShouldNotReachHere();
    }
  }
}

int unpack_counter = 0;

void vframeArrayElement::unpack_on_stack(int caller_actual_parameters, int callee_parameters, int callee_locals, frame* caller, bool is_top_frame, bool is_bottom_frame, int exec_mode) {
  JavaThread* thread = (JavaThread*) Thread::current();

  bool realloc_failure_exception = thread->frames_to_pop_failed_realloc() > 0;

  // Look at bci and decide on bcp and continuation pc
  address bcp;
  // C++ interpreter doesn't need a pc since it will figure out what to do when it
  // begins execution
  address pc;
  bool use_next_mdp = false; // true if we should use the mdp associated with the next bci
                             // rather than the one associated with bcp
  if (raw_bci() == SynchronizationEntryBCI) {
    // We are deoptimizing while hanging in prologue code for synchronized method
    bcp = method()->bcp_from(0); // first byte code
    pc  = Interpreter::deopt_entry(vtos, 0); // step = 0 since we don't skip current bytecode
  } else if (should_reexecute()) { //reexecute this bytecode
    bcp = method()->bcp_from(bci());
    pc  = Interpreter::deopt_reexecute_entry(method(), bcp);
  } else {
    bcp = method()->bcp_from(bci());
    pc  = Interpreter::deopt_continue_after_entry(method(), bcp, callee_parameters, is_top_frame);
    use_next_mdp = true;
  }

  // Monitorenter and pending exceptions:
  //
  // For Compiler2, there should be no pending exception when deoptimizing at monitorenter
  // because there is no safepoint at the null pointer check (it is either handled explicitly
  // or prior to the monitorenter) and asynchronous exceptions are not made "pending" by the
  // runtime interface for the slow case (see JRT_ENTRY_FOR_MONITORENTER).  If an asynchronous
  // exception was processed, the bytecode pointer would have to be extended one bytecode beyond
  // the monitorenter to place it in the proper exception range.
  //
  // For Compiler1, deoptimization can occur while throwing a NullPointerException at monitorenter,
  // in which case bcp should point to the monitorenter since it is within the exception's range.
  //
  // For realloc failure exception we just pop frames, skip the guarantee.

  guarantee(realloc_failure_exception || !(thread->deopt_compiled_method()->is_compiled_by_c2() && *bcp == Bytecodes::_monitorenter             && exec_mode == Deoptimization::Unpack_exception), "shouldn't get exception during monitorenter");

  int popframe_preserved_args_size_in_bytes = 0;
  int popframe_preserved_args_size_in_words = 0;
  if (is_top_frame) {
    {
      // Possibly override the previous pc computation of the top (youngest) frame
      switch (exec_mode) {
      case Deoptimization::Unpack_deopt:
        // use what we've got
        break;
      case Deoptimization::Unpack_exception:
        // exception is pending
        pc = SharedRuntime::raw_exception_handler_for_return_address(thread, pc);
        // [phh] We're going to end up in some handler or other, so it doesn't
        // matter what mdp we point to.  See exception_handler_for_exception()
        // in interpreterRuntime.cpp.
        break;
      case Deoptimization::Unpack_uncommon_trap:
      case Deoptimization::Unpack_reexecute:
        // redo last byte code
        pc  = Interpreter::deopt_entry(vtos, 0);
        use_next_mdp = false;
        break;
      default:
        ShouldNotReachHere();
      }
    }
  }

  // Setup the interpreter frame

  int temps = expressions()->size();

  int locks = monitors() == NULL ? 0 : monitors()->number_of_monitors();

  Interpreter::layout_activation(method(),
                                 temps + callee_parameters,
                                 popframe_preserved_args_size_in_words,
                                 locks,
                                 caller_actual_parameters,
                                 callee_parameters,
                                 callee_locals,
                                 caller,
                                 iframe(),
                                 is_top_frame,
                                 is_bottom_frame);

  // Update the pc in the frame object and overwrite the temporary pc
  // we placed in the skeletal frame now that we finally know the
  // exact interpreter address we should use.

  _frame.patch_pc(thread, pc);

  BasicObjectLock* top = iframe()->interpreter_frame_monitor_begin();
  for (int index = 0; index < locks; index++) {
    top = iframe()->previous_monitor_in_interpreter_frame(top);
    BasicObjectLock* src = _monitors->at(index);
    top->set_obj(src->obj());
    src->lock()->move_to(src->obj(), top->lock());
  }
  if (ProfileInterpreter) {
    iframe()->interpreter_frame_set_mdp(0); // clear out the mdp.
  }
  iframe()->interpreter_frame_set_bcp(bcp);
  if (ProfileInterpreter) {
    MethodData* mdo = method()->method_data();
    if (mdo != NULL) {
      int bci = iframe()->interpreter_frame_bci();
      if (use_next_mdp) ++bci;
      address mdp = mdo->bci_to_dp(bci);
      iframe()->interpreter_frame_set_mdp(mdp);
    }
  }

  // Unpack expression stack
  // If this is an intermediate frame (i.e. not top frame) then this
  // only unpacks the part of the expression stack not used by callee
  // as parameters. The callee parameters are unpacked as part of the
  // callee locals.
  int i;
  for (i = 0; i < expressions()->size(); i++) {
    StackValue *value = expressions()->at(i);
    intptr_t*   addr  = iframe()->interpreter_frame_expression_stack_at(i);
    switch(value->type()) {
      case T_INT:
        *addr = value->get_int();
        break;
      case T_OBJECT:
        *addr = value->get_int(T_OBJECT);
        break;
      case T_CONFLICT:
        // A dead stack slot.  Initialize to null in case it is an oop.
        *addr = NULL_WORD;
        break;
      default:
        ShouldNotReachHere();
    }
  }

  // Unpack the locals
  for (i = 0; i < locals()->size(); i++) {
    StackValue *value = locals()->at(i);
    intptr_t* addr  = iframe()->interpreter_frame_local_at(i);
    switch(value->type()) {
      case T_INT:
        *addr = value->get_int();
        break;
      case T_OBJECT:
        *addr = value->get_int(T_OBJECT);
        break;
      case T_CONFLICT:
        // A dead location. If it is an oop then we need a NULL to prevent GC from following it
        *addr = NULL_WORD;
        break;
      default:
        ShouldNotReachHere();
    }
  }

  // The expression stack and locals are in the resource area don't leave
  // a dangling pointer in the vframeArray we leave around for debug
  // purposes

  _locals = _expressions = NULL;
}

int vframeArrayElement::on_stack_size(int callee_parameters, int callee_locals, bool is_top_frame, int popframe_extra_stack_expression_els) const {
  int locks = monitors() == NULL ? 0 : monitors()->number_of_monitors();
  int temps = expressions()->size();
  return Interpreter::size_activation(method()->max_stack(),
                                      temps + callee_parameters,
                                      popframe_extra_stack_expression_els,
                                      locks,
                                      callee_parameters,
                                      callee_locals,
                                      is_top_frame);
}

intptr_t* vframeArray::unextended_sp() const {
  return _original.unextended_sp();
}

vframeArray* vframeArray::allocate(JavaThread* thread, int frame_size, GrowableArray<compiledVFrame*>* chunk,
                                   RegisterMap *reg_map, frame sender, frame caller, frame self,
                                   bool realloc_failures) {
  // Allocate the vframeArray
  vframeArray * result = (vframeArray*) AllocateHeap(sizeof(vframeArray) + // fixed part
                                                     sizeof(vframeArrayElement) * (chunk->length() - 1), // variable part
                                                     mtCompiler);
  result->_frames = chunk->length();
  result->_owner_thread = thread;
  result->_sender = sender;
  result->_caller = caller;
  result->_original = self;
  result->set_unroll_block(NULL); // initialize it
  result->fill_in(thread, frame_size, chunk, reg_map, realloc_failures);
  return result;
}

void vframeArray::fill_in(JavaThread* thread, int frame_size, GrowableArray<compiledVFrame*>* chunk, const RegisterMap *reg_map, bool realloc_failures) {
  // Set owner first, it is used when adding monitor chunks

  _frame_size = frame_size;
  for (int i = 0; i < chunk->length(); i++) {
    element(i)->fill_in(chunk->at(i), realloc_failures);
  }

  // Copy registers for callee-saved registers
  if (reg_map != NULL) {
    for (int i = 0; i < RegisterMap::reg_count; i++) {
#ifdef AMD64
      // The register map has one entry for every int (32-bit value), so
      // 64-bit physical registers have two entries in the map, one for
      // each half.  Ignore the high halves of 64-bit registers, just like
      // frame::oopmapreg_to_location does.
      //
      // [phh] FIXME: this is a temporary hack!  This code *should* work
      // correctly w/o this hack, possibly by changing RegisterMap::pd_location
      // in frame_amd64.cpp and the values of the phantom high half registers
      // in amd64.ad.
      //      if (VMReg::Name(i) < SharedInfo::stack0 && is_even(i)) {
        intptr_t* src = (intptr_t*) reg_map->location(VMRegImpl::as_VMReg(i));
        _callee_registers[i] = src != NULL ? *src : NULL_WORD;
#else
      jint* src = (jint*) reg_map->location(VMRegImpl::as_VMReg(i));
      _callee_registers[i] = src != NULL ? *src : NULL_WORD;
#endif
      if (src == NULL) {
        set_location_valid(i, false);
      } else {
        set_location_valid(i, true);
        jint* dst = (jint*) register_location(i);
        *dst = *src;
      }
    }
  }
}

void vframeArray::unpack_to_stack(frame &unpack_frame, int exec_mode, int caller_actual_parameters) {
  // stack picture
  //   unpack_frame
  //   [new interpreter frames ] (frames are skeletal but walkable)
  //   caller_frame
  //
  //  This routine fills in the missing data for the skeletal interpreter frames
  //  in the above picture.

  // Find the skeletal interpreter frames to unpack into
  JavaThread* THREAD = JavaThread::current();
  RegisterMap map(THREAD, false);
  // Get the youngest frame we will unpack (last to be unpacked)
  frame me = unpack_frame.sender(&map);
  int index;
  for (index = 0; index < frames(); index++ ) {
    *element(index)->iframe() = me;
    // Get the caller frame (possibly skeletal)
    me = me.sender(&map);
  }

  // Do the unpacking of interpreter frames; the frame at index 0 represents the top activation, so it has no callee
  // Unpack the frames from the oldest (frames() -1) to the youngest (0)
  frame* caller_frame = &me;
  for (index = frames() - 1; index >= 0 ; index--) {
    vframeArrayElement* elem = element(index);  // caller
    int callee_parameters, callee_locals;
    if (index == 0) {
      callee_parameters = callee_locals = 0;
    } else {
      methodHandle caller = elem->method();
      methodHandle callee = element(index - 1)->method();
      Bytecode_invoke inv(caller, elem->bci());
      // invokedynamic instructions don't have a class but obviously don't have a MemberName appendix.
      // NOTE:  Use machinery here that avoids resolving of any kind.
      const bool has_member_arg = !inv.is_invokedynamic() && MethodHandles::has_member_arg(inv.klass(), inv.name());
      callee_parameters = callee->size_of_parameters() + (has_member_arg ? 1 : 0);
      callee_locals     = callee->max_locals();
    }
    elem->unpack_on_stack(caller_actual_parameters,
                          callee_parameters,
                          callee_locals,
                          caller_frame,
                          index == 0,
                          index == frames() - 1,
                          exec_mode);
    if (index == frames() - 1) {
      Deoptimization::unwind_callee_save_values(elem->iframe(), this);
    }
    caller_frame = elem->iframe();
    caller_actual_parameters = callee_parameters;
  }
  deallocate_monitor_chunks();
}

void vframeArray::deallocate_monitor_chunks() {
  JavaThread* jt = JavaThread::current();
  for (int index = 0; index < frames(); index++ ) {
     element(index)->free_monitors(jt);
  }
}

address vframeArray::register_location(int i) const {
  return (address) & _callee_registers[i];
}
