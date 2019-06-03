#include "precompiled.hpp"

#include "code/codeCache.hpp"
#include "code/vmreg.inline.hpp"
#include "compiler/abstractCompiler.hpp"
#include "compiler/disassembler.hpp"
#include "gc/shared/collectedHeap.inline.hpp"
#include "interpreter/interpreter.hpp"
#include "interpreter/oopMapCache.hpp"
#include "memory/resourceArea.hpp"
#include "memory/universe.hpp"
#include "oops/markOop.hpp"
#include "oops/method.hpp"
#include "oops/methodData.hpp"
#include "oops/oop.inline.hpp"
#include "oops/verifyOopClosure.hpp"
#include "prims/methodHandles.hpp"
#include "runtime/frame.inline.hpp"
#include "runtime/handles.inline.hpp"
#include "runtime/javaCalls.hpp"
#include "runtime/monitorChunk.hpp"
#include "runtime/os.hpp"
#include "runtime/sharedRuntime.hpp"
#include "runtime/signature.hpp"
#include "runtime/stubCodeGenerator.hpp"
#include "runtime/stubRoutines.hpp"
#include "runtime/thread.inline.hpp"
#include "utilities/debug.hpp"
#include "utilities/decoder.hpp"
#include "utilities/formatBuffer.hpp"

RegisterMap::RegisterMap(JavaThread *thread, bool update_map) {
  _thread         = thread;
  _update_map     = update_map;
  clear();
}

RegisterMap::RegisterMap(const RegisterMap* map) {
  _thread                = map->thread();
  _update_map            = map->update_map();
  _include_argument_oops = map->include_argument_oops();
  pd_initialize_from(map);
  if (update_map()) {
    for (int i = 0; i < location_valid_size; i++) {
      LocationValidType bits = !update_map() ? 0 : map->_location_valid[i];
      _location_valid[i] = bits;
      // for whichever bits are set, pull in the corresponding map->_location
      int j = i*location_valid_type_size;
      while (bits != 0) {
        if ((bits & 1) != 0) {
          _location[j] = map->_location[j];
        }
        bits >>= 1;
        j += 1;
      }
    }
  }
}

void RegisterMap::clear() {
  set_include_argument_oops(true);
  if (_update_map) {
    for (int i = 0; i < location_valid_size; i++) {
      _location_valid[i] = 0;
    }
    pd_clear();
  } else {
    pd_initialize();
  }
}

// This returns the pc that if you were in the debugger you'd see. Not
// the idealized value in the frame object. This undoes the magic conversion
// that happens for deoptimized frames. In addition it makes the value the
// hardware would want to see in the native frame. The only user (at this point)
// is deoptimization. It likely no one else should ever use it.

address frame::raw_pc() const {
  if (is_deoptimized_frame()) {
    CompiledMethod* cm = cb()->as_compiled_method_or_null();
    if (cm->is_method_handle_return(pc()))
      return cm->deopt_mh_handler_begin() - pc_return_offset;
    else
      return cm->deopt_handler_begin() - pc_return_offset;
  } else {
    return (pc() - pc_return_offset);
  }
}

// Change the pc in a frame object. This does not change the actual pc in
// actual frame. To do that use patch_pc.
//
void frame::set_pc(address newpc ) {
  // Unsafe to use the is_deoptimzed tester after changing pc
  _deopt_state = unknown;
  _pc = newpc;
  _cb = CodeCache::find_blob_unsafe(_pc);
}

// type testers
bool frame::is_ignored_frame() const {
  return false;  // FIXME: some LambdaForm frames should be ignored
}
bool frame::is_deoptimized_frame() const {
  return _deopt_state == is_deoptimized;
}

bool frame::is_native_frame() const {
  return (_cb != NULL && _cb->is_nmethod() && ((nmethod*)_cb)->is_native_method());
}

bool frame::is_java_frame() const {
  if (is_interpreted_frame()) return true;
  if (is_compiled_frame())    return true;
  return false;
}

bool frame::is_compiled_frame() const {
  if (_cb != NULL && _cb->is_compiled() && ((CompiledMethod*)_cb)->is_java_method()) {
    return true;
  }
  return false;
}

bool frame::is_runtime_frame() const {
  return (_cb != NULL && _cb->is_runtime_stub());
}

bool frame::is_safepoint_blob_frame() const {
  return (_cb != NULL && _cb->is_safepoint_stub());
}

// testers

bool frame::is_first_java_frame() const {
  RegisterMap map(JavaThread::current(), false); // No update
  frame s;
  for (s = sender(&map); !(s.is_java_frame() || s.is_first_frame()); s = s.sender(&map));
  return s.is_first_frame();
}

bool frame::entry_frame_is_first() const {
  return entry_frame_call_wrapper()->is_first_frame();
}

JavaCallWrapper* frame::entry_frame_call_wrapper_if_safe(JavaThread* thread) const {
  JavaCallWrapper** jcw = entry_frame_call_wrapper_addr();
  address addr = (address) jcw;

  // addr must be within the usable part of the stack
  if (thread->is_in_usable_stack(addr)) {
    return *jcw;
  }

  return NULL;
}

bool frame::is_entry_frame_valid(JavaThread* thread) const {
  // Validate the JavaCallWrapper an entry frame must have
  address jcw = (address)entry_frame_call_wrapper();
  bool jcw_safe = (jcw < thread->stack_base()) && (jcw > (address)fp()); // less than stack base
  if (!jcw_safe) {
    return false;
  }

  // Validate sp saved in the java frame anchor
  JavaFrameAnchor* jfa = entry_frame_call_wrapper()->anchor();
  return (jfa->last_Java_sp() > sp());
}

bool frame::should_be_deoptimized() const {
  if (_deopt_state == is_deoptimized || !is_compiled_frame()) return false;
  CompiledMethod* nm = (CompiledMethod *)_cb;

  if (!nm->is_marked_for_deoptimization())
    return false;

  // If at the return point, then the frame has already been popped, and
  // only the return needs to be executed. Don't deoptimize here.
  return !nm->is_at_poll_return(pc());
}

bool frame::can_be_deoptimized() const {
  if (!is_compiled_frame()) return false;
  CompiledMethod* nm = (CompiledMethod*)_cb;

  if (!nm->can_be_deoptimized())
    return false;

  return !nm->is_at_poll_return(pc());
}

void frame::deoptimize(JavaThread* thread) {
  // This is a fix for register window patching race
  if (NeedsDeoptSuspend && Thread::current() != thread) {
    // It is possible especially with DeoptimizeALot/DeoptimizeRandom that
    // we could see the frame again and ask for it to be deoptimized since
    // it might move for a long time. That is harmless and we just ignore it.
    if (id() == thread->must_deopt_id()) {
      return;
    }

    // We are at a safepoint so the target thread can only be
    // in 4 states:
    //     blocked - no problem
    //     blocked_trans - no problem (i.e. could have woken up from blocked
    //                                 during a safepoint).
    //     native - register window pc patching race
    //     native_trans - momentary state
    //
    // We could just wait out a thread in native_trans to block.
    // Then we'd have all the issues that the safepoint code has as to
    // whether to spin or block. It isn't worth it. Just treat it like
    // native and be done with it.
    //
    // Examine the state of the thread at the start of safepoint since
    // threads that were in native at the start of the safepoint could
    // come to a halt during the safepoint, changing the current value
    // of the safepoint_state.
    JavaThreadState state = thread->safepoint_state()->orig_thread_state();
    if (state == _thread_in_native || state == _thread_in_native_trans) {
      // Since we are at a safepoint the target thread will stop itself
      // before it can return to java as long as we remain at the safepoint.
      // Therefore we can put an additional request for the thread to stop
      // no matter what no (like a suspend). This will cause the thread
      // to notice it needs to do the deopt on its own once it leaves native.
      //
      // The only reason we must do this is because on machine with register
      // windows we have a race with patching the return address and the
      // window coming live as the thread returns to the Java code (but still
      // in native mode) and then blocks. It is only this top most frame
      // that is at risk. So in truth we could add an additional check to
      // see if this frame is one that is at risk.
      RegisterMap map(thread, false);
      frame at_risk =  thread->last_frame().sender(&map);
      if (id() == at_risk.id()) {
        thread->set_must_deopt_id(id());
        thread->set_deopt_suspend();
        return;
      }
    }
  }

  // If the call site is a MethodHandle call site use the MH deopt
  // handler.
  CompiledMethod* cm = (CompiledMethod*) _cb;
  address deopt = cm->is_method_handle_return(pc()) ?
                        cm->deopt_mh_handler_begin() :
                        cm->deopt_handler_begin();

  // Save the original pc before we patch in the new one
  cm->set_original_pc(this, pc());
  patch_pc(thread, deopt);
}

frame frame::java_sender() const {
  RegisterMap map(JavaThread::current(), false);
  frame s;
  for (s = sender(&map); !(s.is_java_frame() || s.is_first_frame()); s = s.sender(&map));
  guarantee(s.is_java_frame(), "tried to get caller of first java frame");
  return s;
}

frame frame::real_sender(RegisterMap* map) const {
  frame result = sender(map);
  while (result.is_runtime_frame() || result.is_ignored_frame()) {
    result = result.sender(map);
  }
  return result;
}

// Note: called by profiler - NOT for current thread
frame frame::profile_find_Java_sender_frame(JavaThread *thread) {
// If we don't recognize this frame, walk back up the stack until we do
  RegisterMap map(thread, false);
  frame first_java_frame = frame();

  // Find the first Java frame on the stack starting with input frame
  if (is_java_frame()) {
    // top frame is compiled frame or deoptimized frame
    first_java_frame = *this;
  } else if (safe_for_sender(thread)) {
    for (frame sender_frame = sender(&map); sender_frame.safe_for_sender(thread) && !sender_frame.is_first_frame(); sender_frame = sender_frame.sender(&map)) {
      if (sender_frame.is_java_frame()) {
        first_java_frame = sender_frame;
        break;
      }
    }
  }
  return first_java_frame;
}

// Interpreter frames

void frame::interpreter_frame_set_locals(intptr_t* locs) {
  *interpreter_frame_locals_addr() = locs;
}

Method* frame::interpreter_frame_method() const {
  Method* m = *interpreter_frame_method_addr();
  return m;
}

void frame::interpreter_frame_set_method(Method* method) {
  *interpreter_frame_method_addr() = method;
}

void frame::interpreter_frame_set_mirror(oop mirror) {
  *interpreter_frame_mirror_addr() = mirror;
}

jint frame::interpreter_frame_bci() const {
  address bcp = interpreter_frame_bcp();
  return interpreter_frame_method()->bci_from(bcp);
}

address frame::interpreter_frame_bcp() const {
  address bcp = (address)*interpreter_frame_bcp_addr();
  return interpreter_frame_method()->bcp_from(bcp);
}

void frame::interpreter_frame_set_bcp(address bcp) {
  *interpreter_frame_bcp_addr() = (intptr_t)bcp;
}

address frame::interpreter_frame_mdp() const {
  return (address)*interpreter_frame_mdp_addr();
}

void frame::interpreter_frame_set_mdp(address mdp) {
  *interpreter_frame_mdp_addr() = (intptr_t)mdp;
}

BasicObjectLock* frame::next_monitor_in_interpreter_frame(BasicObjectLock* current) const {
  BasicObjectLock* next = (BasicObjectLock*) (((intptr_t*) current) + interpreter_frame_monitor_size());
  return next;
}

BasicObjectLock* frame::previous_monitor_in_interpreter_frame(BasicObjectLock* current) const {
  BasicObjectLock* previous = (BasicObjectLock*) (((intptr_t*) current) - interpreter_frame_monitor_size());
  return previous;
}

// Interpreter locals and expression stack locations.

intptr_t* frame::interpreter_frame_local_at(int index) const {
  const int n = Interpreter::local_offset_in_bytes(index)/wordSize;
  return &((*interpreter_frame_locals_addr())[n]);
}

intptr_t* frame::interpreter_frame_expression_stack_at(jint offset) const {
  const int i = offset * interpreter_frame_expression_stack_direction();
  const int n = i * Interpreter::stackElementWords;
  return &(interpreter_frame_expression_stack()[n]);
}

jint frame::interpreter_frame_expression_stack_size() const {
  // Number of elements on the interpreter expression stack
  // Callers should span by stackElementWords
  int element_size = Interpreter::stackElementWords;
  size_t stack_size = 0;
  if (frame::interpreter_frame_expression_stack_direction() < 0) {
    stack_size = (interpreter_frame_expression_stack() - interpreter_frame_tos_address() + 1)/element_size;
  } else {
    stack_size = (interpreter_frame_tos_address() - interpreter_frame_expression_stack() + 1)/element_size;
  }
  return ((jint)stack_size);
}

// (frame::interpreter_frame_sender_sp accessor is in frame_<arch>.cpp)

const char* frame::print_name() const {
  if (is_native_frame())      return "Native";
  if (is_interpreted_frame()) return "Interpreted";
  if (is_compiled_frame()) {
    if (is_deoptimized_frame()) return "Deoptimized";
    return "Compiled";
  }
  if (sp() == NULL)            return "Empty";
  return "C";
}

void frame::print_value_on(outputStream* st, JavaThread *thread) const {
  st->print("%s frame (sp=" INTPTR_FORMAT " unextended sp=" INTPTR_FORMAT, print_name(), p2i(sp()), p2i(unextended_sp()));
  if (sp() != NULL)
    st->print(", fp=" INTPTR_FORMAT ", real_fp=" INTPTR_FORMAT ", pc=" INTPTR_FORMAT,
              p2i(fp()), p2i(real_fp()), p2i(pc()));

  if (StubRoutines::contains(pc())) {
    st->print_cr(")");
    st->print("(");
    StubCodeDesc* desc = StubCodeDesc::desc_for(pc());
    st->print("~Stub::%s", desc->name());
  } else if (Interpreter::contains(pc())) {
    st->print_cr(")");
    st->print("(");
    InterpreterCodelet* desc = Interpreter::codelet_containing(pc());
    if (desc != NULL) {
      st->print("~");
      desc->print_on(st);
    } else {
      st->print("~interpreter");
    }
  }
  st->print_cr(")");

  if (_cb != NULL) {
    st->print("     ");
    _cb->print_value_on(st);
    st->cr();
  }
}

void frame::print_on(outputStream* st) const {
  print_value_on(st,NULL);
  if (is_interpreted_frame()) {
    interpreter_frame_print_on(st);
  }
}

void frame::interpreter_frame_print_on(outputStream* st) const { }

// Print whether the frame is in the VM or OS indicating a HotSpot problem.
// Otherwise, it's likely a bug in the native library that the Java code calls,
// hopefully indicating where to submit bugs.
void frame::print_C_frame(outputStream* st, char* buf, int buflen, address pc) {
  // C/C++ frame
  bool in_vm = os::address_is_in_vm(pc);
  st->print(in_vm ? "V" : "C");

  int offset;
  bool found;

  // libname
  found = os::dll_address_to_library_name(pc, buf, buflen, &offset);
  if (found) {
    // skip directory names
    const char *p1, *p2;
    p1 = buf;
    int len = (int)strlen(os::file_separator());
    while ((p2 = strstr(p1, os::file_separator())) != NULL) p1 = p2 + len;
    st->print("  [%s+0x%x]", p1, offset);
  } else {
    st->print("  " PTR_FORMAT, p2i(pc));
  }

  found = os::dll_address_to_function_name(pc, buf, buflen, &offset);
  if (found) {
    st->print("  %s+0x%x", buf, offset);
  }
}

// frame::print_on_error() is called by fatal error handler. Notice that we may
// crash inside this function if stack frame is corrupted. The fatal error
// handler can catch and handle the crash. Here we assume the frame is valid.
//
// First letter indicates type of the frame:
//    J: Java frame (compiled)
//    A: Java frame (aot compiled)
//    j: Java frame (interpreted)
//    V: VM frame (C/C++)
//    v: Other frames running VM generated code (e.g. stubs, adapters, etc.)
//    C: C/C++ frame
//
// We don't need detailed frame type as that in frame::print_name(). "C"
// suggests the problem is in user lib; everything else is likely a VM bug.

void frame::print_on_error(outputStream* st, char* buf, int buflen, bool verbose) const {
  if (_cb != NULL) {
    if (Interpreter::contains(pc())) {
      Method* m = this->interpreter_frame_method();
      if (m != NULL) {
        m->name_and_sig_as_C_string(buf, buflen);
        st->print("j  %s", buf);
        st->print("+%d", this->interpreter_frame_bci());
        ModuleEntry* module = m->method_holder()->module();
        if (module->is_named()) {
          module->name()->as_C_string(buf, buflen);
          st->print(" %s", buf);
          if (module->version() != NULL) {
            module->version()->as_C_string(buf, buflen);
            st->print("@%s", buf);
          }
        }
      } else {
        st->print("j  " PTR_FORMAT, p2i(pc()));
      }
    } else if (StubRoutines::contains(pc())) {
      StubCodeDesc* desc = StubCodeDesc::desc_for(pc());
      if (desc != NULL) {
        st->print("v  ~StubRoutines::%s", desc->name());
      } else {
        st->print("v  ~StubRoutines::" PTR_FORMAT, p2i(pc()));
      }
    } else if (_cb->is_buffer_blob()) {
      st->print("v  ~BufferBlob::%s", ((BufferBlob *)_cb)->name());
    } else if (_cb->is_compiled()) {
      CompiledMethod* cm = (CompiledMethod*)_cb;
      Method* m = cm->method();
      if (m != NULL) {
        if (cm->is_aot()) {
          st->print("A %d ", cm->compile_id());
        } else if (cm->is_nmethod()) {
          nmethod* nm = cm->as_nmethod();
          st->print("J %d%s", nm->compile_id(), (nm->is_osr_method() ? "%" : ""));
          st->print(" %s", nm->compiler_name());
        }
        m->name_and_sig_as_C_string(buf, buflen);
        st->print(" %s", buf);
        ModuleEntry* module = m->method_holder()->module();
        if (module->is_named()) {
          module->name()->as_C_string(buf, buflen);
          st->print(" %s", buf);
          if (module->version() != NULL) {
            module->version()->as_C_string(buf, buflen);
            st->print("@%s", buf);
          }
        }
        st->print(" (%d bytes) @ " PTR_FORMAT " [" PTR_FORMAT "+" INTPTR_FORMAT "]", m->code_size(), p2i(_pc), p2i(_cb->code_begin()), _pc - _cb->code_begin());
        if (cm->is_nmethod()) {
          nmethod* nm = cm->as_nmethod();
          char* jvmciName = nm->jvmci_installed_code_name(buf, buflen);
          if (jvmciName != NULL) {
            st->print(" (%s)", jvmciName);
          }
        }
      } else {
        st->print("J  " PTR_FORMAT, p2i(pc()));
      }
    } else if (_cb->is_runtime_stub()) {
      st->print("v  ~RuntimeStub::%s", ((RuntimeStub *)_cb)->name());
    } else if (_cb->is_deoptimization_stub()) {
      st->print("v  ~DeoptimizationBlob");
    } else if (_cb->is_exception_stub()) {
      st->print("v  ~ExceptionBlob");
    } else if (_cb->is_safepoint_stub()) {
      st->print("v  ~SafepointBlob");
    } else {
      st->print("v  blob " PTR_FORMAT, p2i(pc()));
    }
  } else {
    print_C_frame(st, buf, buflen, pc());
  }
}

/*
  The interpreter_frame_expression_stack_at method in the case of SPARC needs the
  max_stack value of the method in order to compute the expression stack address.
  It uses the Method* in order to get the max_stack value but during GC this
  Method* value saved on the frame is changed by reverse_and_push and hence cannot
  be used. So we save the max_stack value in the FrameClosure object and pass it
  down to the interpreter_frame_expression_stack_at method
*/
class InterpreterFrameClosure : public OffsetClosure {
 private:
  frame* _fr;
  OopClosure* _f;
  int    _max_locals;
  int    _max_stack;

 public:
  InterpreterFrameClosure(frame* fr, int max_locals, int max_stack,
                          OopClosure* f) {
    _fr         = fr;
    _max_locals = max_locals;
    _max_stack  = max_stack;
    _f          = f;
  }

  void offset_do(int offset) {
    oop* addr;
    if (offset < _max_locals) {
      addr = (oop*) _fr->interpreter_frame_local_at(offset);
      _f->do_oop(addr);
    } else {
      addr = (oop*) _fr->interpreter_frame_expression_stack_at((offset - _max_locals));
      // In case of exceptions, the expression stack is invalid and the esp will be reset to express
      // this condition. Therefore, we call f only if addr is 'inside' the stack (i.e., addr >= esp for Intel).
      bool in_stack;
      if (frame::interpreter_frame_expression_stack_direction() > 0) {
        in_stack = (intptr_t*)addr <= _fr->interpreter_frame_tos_address();
      } else {
        in_stack = (intptr_t*)addr >= _fr->interpreter_frame_tos_address();
      }
      if (in_stack) {
        _f->do_oop(addr);
      }
    }
  }

  int max_locals()  { return _max_locals; }
  frame* fr()       { return _fr; }
};

class InterpretedArgumentOopFinder: public SignatureInfo {
 private:
  OopClosure* _f;        // Closure to invoke
  int    _offset;        // TOS-relative offset, decremented with each argument
  bool   _has_receiver;  // true if the callee has a receiver
  frame* _fr;

  void set(int size, BasicType type) {
    _offset -= size;
    if (type == T_OBJECT || type == T_ARRAY) oop_offset_do();
  }

  void oop_offset_do() {
    oop* addr;
    addr = (oop*)_fr->interpreter_frame_tos_at(_offset);
    _f->do_oop(addr);
  }

 public:
  InterpretedArgumentOopFinder(Symbol* signature, bool has_receiver, frame* fr, OopClosure* f) : SignatureInfo(signature), _has_receiver(has_receiver) {
    // compute size of arguments
    int args_size = ArgumentSizeComputer(signature).size() + (has_receiver ? 1 : 0);
    // initialize InterpretedArgumentOopFinder
    _f         = f;
    _fr        = fr;
    _offset    = args_size;
  }

  void oops_do() {
    if (_has_receiver) {
      --_offset;
      oop_offset_do();
    }
    iterate_parameters();
  }
};

// Entry frame has following form (n arguments)
//         +-----------+
//   sp -> |  last arg |
//         +-----------+
//         :    :::    :
//         +-----------+
// (sp+n)->|  first arg|
//         +-----------+

// visits and GC's all the arguments in entry frame
class EntryFrameOopFinder: public SignatureInfo {
 private:
  bool   _is_static;
  int    _offset;
  frame* _fr;
  OopClosure* _f;

  void set(int size, BasicType type) {
    if (type == T_OBJECT || type == T_ARRAY) oop_at_offset_do(_offset);
    _offset -= size;
  }

  void oop_at_offset_do(int offset) {
    oop* addr = (oop*) _fr->entry_frame_argument_at(offset);
    _f->do_oop(addr);
  }

 public:
   EntryFrameOopFinder(frame* frame, Symbol* signature, bool is_static) : SignatureInfo(signature) {
     _f = NULL; // will be set later
     _fr = frame;
     _is_static = is_static;
     _offset = ArgumentSizeComputer(signature).size() - 1; // last parameter is at index 0
   }

  void arguments_do(OopClosure* f) {
    _f = f;
    if (!_is_static) oop_at_offset_do(_offset+1); // do the receiver
    iterate_parameters();
  }
};

oop* frame::interpreter_callee_receiver_addr(Symbol* signature) {
  ArgumentSizeComputer asc(signature);
  int size = asc.size();
  return (oop *)interpreter_frame_tos_at(size);
}

void frame::oops_interpreted_do(OopClosure* f, const RegisterMap* map, bool query_oop_map_cache) {
  Thread *thread = Thread::current();
  methodHandle m (thread, interpreter_frame_method());
  jint      bci = interpreter_frame_bci();

  // Handle the monitor elements in the activation
  for (BasicObjectLock* current = interpreter_frame_monitor_end(); current < interpreter_frame_monitor_begin(); current = next_monitor_in_interpreter_frame(current)) {
    current->oops_do(f);
  }

  if (m->is_native()) {
    f->do_oop(interpreter_frame_temp_oop_addr());
  }

  // The method pointer in the frame might be the only path to the method's
  // klass, and the klass needs to be kept alive while executing. The GCs
  // don't trace through method pointers, so the mirror of the method's klass
  // is installed as a GC root.
  f->do_oop(interpreter_frame_mirror_addr());

  int max_locals = m->is_native() ? m->size_of_parameters() : m->max_locals();

  Symbol* signature = NULL;
  bool has_receiver = false;

  // Process a callee's arguments if we are at a call site
  // (i.e., if we are at an invoke bytecode)
  // This is used sometimes for calling into the VM, not for another
  // interpreted or compiled frame.
  if (!m->is_native()) {
    Bytecode_invoke call = Bytecode_invoke_check(m, bci);
    if (call.is_valid()) {
      signature = call.signature();
      has_receiver = call.has_receiver();
      if (map->include_argument_oops() && interpreter_frame_expression_stack_size() > 0) {
        ResourceMark rm(thread);  // is this right ???
        // we are at a call site & the expression stack is not empty
        // => process callee's arguments
        //
        // Note: The expression stack can be empty if an exception
        //       occurred during method resolution/execution. In all
        //       cases we empty the expression stack completely be-
        //       fore handling the exception (the exception handling
        //       code in the interpreter calls a blocking runtime
        //       routine which can cause this code to be executed).
        //       (was bug gri 7/27/98)
        oops_interpreted_arguments_do(signature, has_receiver, f);
      }
    }
  }

  InterpreterFrameClosure blk(this, max_locals, m->max_stack(), f);

  // process locals & expression stack
  InterpreterOopMap mask;
  if (query_oop_map_cache) {
    m->mask_for(bci, &mask);
  } else {
    OopMapCache::compute_one_oop_map(m, bci, &mask);
  }
  mask.iterate_oop(&blk);
}

void frame::oops_interpreted_arguments_do(Symbol* signature, bool has_receiver, OopClosure* f) {
  InterpretedArgumentOopFinder finder(signature, has_receiver, this, f);
  finder.oops_do();
}

void frame::oops_code_blob_do(OopClosure* f, CodeBlobClosure* cf, const RegisterMap* reg_map) {
  if (_cb->oop_maps() != NULL) {
    OopMapSet::oops_do(this, reg_map, f);

    // Preserve potential arguments for a callee. We handle this by dispatching
    // on the codeblob. For c2i, we do
    if (reg_map->include_argument_oops()) {
      _cb->preserve_callee_argument_oops(*this, reg_map, f);
    }
  }
  // In cases where perm gen is collected, GC will want to mark
  // oops referenced from nmethods active on thread stacks so as to
  // prevent them from being collected. However, this visit should be
  // restricted to certain phases of the collection only. The
  // closure decides how it wants nmethods to be traced.
  if (cf != NULL)
    cf->do_code_blob(_cb);
}

class CompiledArgumentOopFinder: public SignatureInfo {
 protected:
  OopClosure*     _f;
  int             _offset;        // the current offset, incremented with each argument
  bool            _has_receiver;  // true if the callee has a receiver
  bool            _has_appendix;  // true if the call has an appendix
  frame           _fr;
  RegisterMap*    _reg_map;
  int             _arg_size;
  VMRegPair*      _regs;        // VMReg list of arguments

  void set(int size, BasicType type) {
    if (type == T_OBJECT || type == T_ARRAY) handle_oop_offset();
    _offset += size;
  }

  virtual void handle_oop_offset() {
    // Extract low order register number from register array.
    // In LP64-land, the high-order bits are valid but unhelpful.
    VMReg reg = _regs[_offset].first();
    oop *loc = _fr.oopmapreg_to_location(reg, _reg_map);
    _f->do_oop(loc);
  }

 public:
  CompiledArgumentOopFinder(Symbol* signature, bool has_receiver, bool has_appendix, OopClosure* f, frame fr,  const RegisterMap* reg_map)
    : SignatureInfo(signature) {
    // initialize CompiledArgumentOopFinder
    _f         = f;
    _offset    = 0;
    _has_receiver = has_receiver;
    _has_appendix = has_appendix;
    _fr        = fr;
    _reg_map   = (RegisterMap*)reg_map;
    _arg_size  = ArgumentSizeComputer(signature).size() + (has_receiver ? 1 : 0) + (has_appendix ? 1 : 0);

    int arg_size;
    _regs = SharedRuntime::find_callee_arguments(signature, has_receiver, has_appendix, &arg_size);
  }

  void oops_do() {
    if (_has_receiver) {
      handle_oop_offset();
      _offset++;
    }
    iterate_parameters();
    if (_has_appendix) {
      handle_oop_offset();
      _offset++;
    }
  }
};

void frame::oops_compiled_arguments_do(Symbol* signature, bool has_receiver, bool has_appendix, const RegisterMap* reg_map, OopClosure* f) {
  ResourceMark rm;
  CompiledArgumentOopFinder finder(signature, has_receiver, has_appendix, f, *this, reg_map);
  finder.oops_do();
}

// Get receiver out of callers frame, i.e. find parameter 0 in callers
// frame.  Consult ADLC for where parameter 0 is to be found.  Then
// check local reg_map for it being a callee-save register or argument
// register, both of which are saved in the local frame.  If not found
// there, it must be an in-stack argument of the caller.
// Note: caller.sp() points to callee-arguments
oop frame::retrieve_receiver(RegisterMap* reg_map) {
  frame caller = *this;

  // First consult the ADLC on where it puts parameter 0 for this signature.
  VMReg reg = SharedRuntime::name_for_receiver();
  oop* oop_adr = caller.oopmapreg_to_location(reg, reg_map);
  if (oop_adr == NULL) {
    guarantee(oop_adr != NULL, "bad register save location");
    return NULL;
  }
  oop r = *oop_adr;
  return r;
}

BasicLock* frame::get_native_monitor() {
  nmethod* nm = (nmethod*)_cb;
  int byte_offset = in_bytes(nm->native_basic_lock_sp_offset());
  return (BasicLock*) &sp()[byte_offset / wordSize];
}

oop frame::get_native_receiver() {
  nmethod* nm = (nmethod*)_cb;
  int byte_offset = in_bytes(nm->native_receiver_sp_offset());
  oop owner = ((oop*) sp())[byte_offset / wordSize];
  return owner;
}

void frame::oops_entry_do(OopClosure* f, const RegisterMap* map) {
  if (map->include_argument_oops()) {
    // must collect argument oops, as nobody else is doing it
    Thread *thread = Thread::current();
    methodHandle m (thread, entry_frame_call_wrapper()->callee_method());
    EntryFrameOopFinder finder(this, m->signature(), m->is_static());
    finder.arguments_do(f);
  }
  // Traverse the Handle Block saved in the entry frame
  entry_frame_call_wrapper()->oops_do(f);
}

void frame::oops_do_internal(OopClosure* f, CodeBlobClosure* cf, RegisterMap* map, bool use_interpreter_oop_map_cache) {
  if (is_interpreted_frame()) {
    oops_interpreted_do(f, map, use_interpreter_oop_map_cache);
  } else if (is_entry_frame()) {
    oops_entry_do(f, map);
  } else if (CodeCache::contains(pc())) {
    oops_code_blob_do(f, cf, map);
  } else {
    ShouldNotReachHere();
  }
}

void frame::nmethods_do(CodeBlobClosure* cf) {
  if (_cb != NULL && _cb->is_nmethod()) {
    cf->do_code_blob(_cb);
  }
}

// call f() on the interpreted Method*s in the stack.
// Have to walk the entire code cache for the compiled frames Yuck.
void frame::metadata_do(void f(Metadata*)) {
  if (is_interpreted_frame()) {
    Method* m = this->interpreter_frame_method();
    f(m);
  }
}

void frame::verify(const RegisterMap* map) {
  // for now make sure receiver type is correct
  if (is_interpreted_frame()) {
    Method* method = interpreter_frame_method();
    guarantee(method->is_method(), "method is wrong in frame::verify");
    if (!method->is_static()) {
      // fetch the receiver
      oop* p = (oop*) interpreter_frame_local_at(0);
      // make sure we have the right receiver type
    }
  }
  oops_do_internal(&VerifyOopClosure::verify_oop, NULL, (RegisterMap*)map, false);
}

//-----------------------------------------------------------------------------------
// StackFrameStream implementation

StackFrameStream::StackFrameStream(JavaThread *thread, bool update) : _reg_map(thread, update) {
  _fr = thread->last_frame();
  _is_done = false;
}
