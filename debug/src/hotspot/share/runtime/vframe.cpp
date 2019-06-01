#include "precompiled.hpp"

#include "classfile/javaClasses.hpp"
#include "classfile/systemDictionary.hpp"
#include "classfile/vmSymbols.hpp"
#include "code/codeCache.hpp"
#include "code/debugInfoRec.hpp"
#include "code/nmethod.hpp"
#include "code/pcDesc.hpp"
#include "code/scopeDesc.hpp"
#include "interpreter/interpreter.hpp"
#include "interpreter/oopMapCache.hpp"
#include "memory/resourceArea.hpp"
#include "oops/instanceKlass.hpp"
#include "oops/oop.inline.hpp"
#include "runtime/frame.inline.hpp"
#include "runtime/handles.inline.hpp"
#include "runtime/objectMonitor.hpp"
#include "runtime/objectMonitor.inline.hpp"
#include "runtime/signature.hpp"
#include "runtime/stubRoutines.hpp"
#include "runtime/synchronizer.hpp"
#include "runtime/vframe.inline.hpp"
#include "runtime/vframeArray.hpp"
#include "runtime/vframe_hp.hpp"

vframe::vframe(const frame* fr, const RegisterMap* reg_map, JavaThread* thread)
: _reg_map(reg_map), _thread(thread) {
  _fr = *fr;
}

vframe::vframe(const frame* fr, JavaThread* thread)
: _reg_map(thread), _thread(thread) {
  _fr = *fr;
}

vframe* vframe::new_vframe(const frame* f, const RegisterMap* reg_map, JavaThread* thread) {
  // Interpreter frame
  if (f->is_interpreted_frame()) {
    return new interpretedVFrame(f, reg_map, thread);
  }

  // Compiled frame
  CodeBlob* cb = f->cb();
  if (cb != NULL) {
    if (cb->is_compiled()) {
      CompiledMethod* nm = (CompiledMethod*)cb;
      return new compiledVFrame(f, reg_map, thread, nm);
    }

    if (f->is_runtime_frame()) {
      // Skip this frame and try again.
      RegisterMap temp_map = *reg_map;
      frame s = f->sender(&temp_map);
      return new_vframe(&s, &temp_map, thread);
    }
  }

  // External frame
  return new externalVFrame(f, reg_map, thread);
}

vframe* vframe::sender() const {
  RegisterMap temp_map = *register_map();
  if (_fr.is_entry_frame() && _fr.is_first_frame()) return NULL;
  frame s = _fr.real_sender(&temp_map);
  if (s.is_first_frame()) return NULL;
  return vframe::new_vframe(&s, &temp_map, thread());
}

vframe* vframe::top() const {
  vframe* vf = (vframe*) this;
  while (!vf->is_top()) vf = vf->sender();
  return vf;
}

javaVFrame* vframe::java_sender() const {
  vframe* f = sender();
  while (f != NULL) {
    if (f->is_java_frame()) return javaVFrame::cast(f);
    f = f->sender();
  }
  return NULL;
}

// ------------- javaVFrame --------------

GrowableArray<MonitorInfo*>* javaVFrame::locked_monitors() {

  GrowableArray<MonitorInfo*>* mons = monitors();
  GrowableArray<MonitorInfo*>* result = new GrowableArray<MonitorInfo*>(mons->length());
  if (mons->is_empty()) return result;

  bool found_first_monitor = false;
  ObjectMonitor *pending_monitor = thread()->current_pending_monitor();
  ObjectMonitor *waiting_monitor = thread()->current_waiting_monitor();
  oop pending_obj = (pending_monitor != NULL ? (oop) pending_monitor->object() : (oop) NULL);
  oop waiting_obj = (waiting_monitor != NULL ? (oop) waiting_monitor->object() : (oop) NULL);

  for (int index = (mons->length()-1); index >= 0; index--) {
    MonitorInfo* monitor = mons->at(index);
    if (monitor->eliminated() && is_compiled_frame()) continue; // skip eliminated monitor
    oop obj = monitor->owner();
    if (obj == NULL) continue; // skip unowned monitor
    //
    // Skip the monitor that the thread is blocked to enter or waiting on
    //
    if (!found_first_monitor && (obj == pending_obj || obj == waiting_obj)) {
      continue;
    }
    found_first_monitor = true;
    result->append(monitor);
  }
  return result;
}

void javaVFrame::print_locked_object_class_name(outputStream* st, Handle obj, const char* lock_state) {
  if (obj.not_null()) {
    st->print("\t- %s <" INTPTR_FORMAT "> ", lock_state, p2i(obj()));
    if (obj->klass() == SystemDictionary::Class_klass()) {
      st->print_cr("(a java.lang.Class for %s)", java_lang_Class::as_external_name(obj()));
    } else {
      Klass* k = obj->klass();
      st->print_cr("(a %s)", k->external_name());
    }
  }
}

void javaVFrame::print_lock_info_on(outputStream* st, int frame_count) {
  Thread* THREAD = Thread::current();
  ResourceMark rm(THREAD);

  // If this is the first frame and it is java.lang.Object.wait(...)
  // then print out the receiver. Locals are not always available,
  // e.g., compiled native frames have no scope so there are no locals.
  if (frame_count == 0) {
    if (method()->name() == vmSymbols::wait_name() && method()->method_holder()->name() == vmSymbols::java_lang_Object()) {
      const char *wait_state = "waiting on"; // assume we are waiting
      // If earlier in the output we reported java.lang.Thread.State ==
      // "WAITING (on object monitor)" and now we report "waiting on", then
      // we are still waiting for notification or timeout. Otherwise if
      // we earlier reported java.lang.Thread.State == "BLOCKED (on object
      // monitor)", then we are actually waiting to re-lock the monitor.
      // At this level we can't distinguish the two cases to report
      // "waited on" rather than "waiting on" for the second case.
      StackValueCollection* locs = locals();
      if (!locs->is_empty()) {
        StackValue* sv = locs->at(0);
        if (sv->type() == T_OBJECT) {
          Handle o = locs->at(0)->get_obj();
          print_locked_object_class_name(st, o, wait_state);
        }
      } else {
        st->print_cr("\t- %s <no object reference available>", wait_state);
      }
    } else if (thread()->current_park_blocker() != NULL) {
      oop obj = thread()->current_park_blocker();
      Klass* k = obj->klass();
      st->print_cr("\t- %s <" INTPTR_FORMAT "> (a %s)", "parking to wait for ", p2i(obj), k->external_name());
    }
  }

  // Print out all monitors that we have locked, or are trying to lock,
  // including re-locking after being notified or timing out in a wait().
  GrowableArray<MonitorInfo*>* mons = monitors();
  if (!mons->is_empty()) {
    bool found_first_monitor = false;
    for (int index = (mons->length()-1); index >= 0; index--) {
      MonitorInfo* monitor = mons->at(index);
      if (monitor->eliminated() && is_compiled_frame()) { // Eliminated in compiled code
        if (monitor->owner_is_scalar_replaced()) {
          Klass* k = java_lang_Class::as_Klass(monitor->owner_klass());
          // format below for lockbits matches this one.
          st->print("\t- eliminated <owner is scalar replaced> (a %s)", k->external_name());
        } else {
          Handle obj(THREAD, monitor->owner());
          if (obj() != NULL) {
            print_locked_object_class_name(st, obj, "eliminated");
          }
        }
        continue;
      }
      if (monitor->owner() != NULL) {
        // the monitor is associated with an object, i.e., it is locked

        markOop mark = NULL;
        const char *lock_state = "locked"; // assume we have the monitor locked
        if (!found_first_monitor && frame_count == 0) {
          // If this is the first frame and we haven't found an owned
          // monitor before, then we need to see if we have completed
          // the lock or if we are blocked trying to acquire it. Only
          // an inflated monitor that is first on the monitor list in
          // the first frame can block us on a monitor enter.
          mark = monitor->owner()->mark();
          if (mark->has_monitor() &&
              ( // we have marked ourself as pending on this monitor
                mark->monitor() == thread()->current_pending_monitor() ||
                // we are not the owner of this monitor
                !mark->monitor()->is_entered(thread())
              )) {
            lock_state = "waiting to lock";
          } else {
            // We own the monitor which is not as interesting so
            // disable the extra printing below.
            mark = NULL;
          }
        } else if (frame_count != 0) {
          // This is not the first frame so we either own this monitor
          // or we owned the monitor before and called wait(). Because
          // wait() could have been called on any monitor in a lower
          // numbered frame on the stack, we have to check all the
          // monitors on the list for this frame.
          mark = monitor->owner()->mark();
          if (mark->has_monitor() &&
              ( // we have marked ourself as pending on this monitor
                mark->monitor() == thread()->current_pending_monitor() ||
                // we are not the owner of this monitor
                !mark->monitor()->is_entered(thread())
              )) {
            lock_state = "waiting to re-lock in wait()";
          } else {
            // We own the monitor which is not as interesting so
            // disable the extra printing below.
            mark = NULL;
          }
        }
        print_locked_object_class_name(st, Handle(THREAD, monitor->owner()), lock_state);
        if (ObjectMonitor::Knob_Verbose && mark != NULL) {
          st->print("\t- lockbits=");
          mark->print_on(st);
          st->cr();
        }

        found_first_monitor = true;
      }
    }
  }
}

// ------------- interpretedVFrame --------------

u_char* interpretedVFrame::bcp() const {
  return fr().interpreter_frame_bcp();
}

void interpretedVFrame::set_bcp(u_char* bcp) {
  fr().interpreter_frame_set_bcp(bcp);
}

intptr_t* interpretedVFrame::locals_addr_at(int offset) const {
  return fr().interpreter_frame_local_at(offset);
}

GrowableArray<MonitorInfo*>* interpretedVFrame::monitors() const {
  GrowableArray<MonitorInfo*>* result = new GrowableArray<MonitorInfo*>(5);
  for (BasicObjectLock* current = (fr().previous_monitor_in_interpreter_frame(fr().interpreter_frame_monitor_begin()));
       current >= fr().interpreter_frame_monitor_end();
       current = fr().previous_monitor_in_interpreter_frame(current)) {
    result->push(new MonitorInfo(current->obj(), current->lock(), false, false));
  }
  return result;
}

int interpretedVFrame::bci() const {
  return method()->bci_from(bcp());
}

Method* interpretedVFrame::method() const {
  return fr().interpreter_frame_method();
}

static StackValue* create_stack_value_from_oop_map(const InterpreterOopMap& oop_mask,
                                                   int index,
                                                   const intptr_t* const addr) {

  // categorize using oop_mask
  if (oop_mask.is_oop(index)) {
    // reference (oop) "r"
    Handle h(Thread::current(), addr != NULL ? (*(oop*)addr) : (oop)NULL);
    return new StackValue(h);
  }
  // value (integer) "v"
  return new StackValue(addr != NULL ? *addr : 0);
}

static bool is_in_expression_stack(const frame& fr, const intptr_t* const addr) {

  // Ensure to be 'inside' the expresion stack (i.e., addr >= sp for Intel).
  // In case of exceptions, the expression stack is invalid and the sp
  // will be reset to express this condition.
  if (frame::interpreter_frame_expression_stack_direction() > 0) {
    return addr <= fr.interpreter_frame_tos_address();
  }

  return addr >= fr.interpreter_frame_tos_address();
}

static void stack_locals(StackValueCollection* result,
                         int length,
                         const InterpreterOopMap& oop_mask,
                         const frame& fr) {

  for (int i = 0; i < length; ++i) {
    const intptr_t* const addr = fr.interpreter_frame_local_at(i);

    StackValue* const sv = create_stack_value_from_oop_map(oop_mask, i, addr);

    result->add(sv);
  }
}

static void stack_expressions(StackValueCollection* result,
                              int length,
                              int max_locals,
                              const InterpreterOopMap& oop_mask,
                              const frame& fr) {

  for (int i = 0; i < length; ++i) {
    const intptr_t* addr = fr.interpreter_frame_expression_stack_at(i);
    if (!is_in_expression_stack(fr, addr)) {
      // Need to ensure no bogus escapes.
      addr = NULL;
    }

    StackValue* const sv = create_stack_value_from_oop_map(oop_mask,
                                                           i + max_locals,
                                                           addr);

    result->add(sv);
  }
}

StackValueCollection* interpretedVFrame::locals() const {
  return stack_data(false);
}

StackValueCollection* interpretedVFrame::expressions() const {
  return stack_data(true);
}

/*
 * Worker routine for fetching references and/or values
 * for a particular bci in the interpretedVFrame.
 *
 * Returns data for either "locals" or "expressions",
 * using bci relative oop_map (oop_mask) information.
 *
 * @param expressions  bool switch controlling what data to return
                       (false == locals / true == expression)
 *
 */
StackValueCollection* interpretedVFrame::stack_data(bool expressions) const {

  InterpreterOopMap oop_mask;
  method()->mask_for(bci(), &oop_mask);
  const int mask_len = oop_mask.number_of_entries();

  // If the method is native, method()->max_locals() is not telling the truth.
  // For our purposes, max locals instead equals the size of parameters.
  const int max_locals = method()->is_native() ?
    method()->size_of_parameters() : method()->max_locals();

  const int length = expressions ? mask_len - max_locals : max_locals;

  StackValueCollection* const result = new StackValueCollection(length);

  if (0 == length) {
    return result;
  }

  if (expressions) {
    stack_expressions(result, length, max_locals, oop_mask, fr());
  } else {
    stack_locals(result, length, oop_mask, fr());
  }

  return result;
}

void interpretedVFrame::set_locals(StackValueCollection* values) const {
  if (values == NULL || values->size() == 0) return;

  // If the method is native, max_locals is not telling the truth.
  // maxlocals then equals the size of parameters
  const int max_locals = method()->is_native() ?
    method()->size_of_parameters() : method()->max_locals();

  // handle locals
  for (int i = 0; i < max_locals; i++) {
    // Find stack location
    intptr_t *addr = locals_addr_at(i);

    // Depending on oop/int put it in the right package
    const StackValue* const sv = values->at(i);
    if (sv->type() == T_OBJECT) {
      *(oop *) addr = (sv->get_obj())();
    } else {                   // integer
      *addr = sv->get_int();
    }
  }
}

// ------------- cChunk --------------

entryVFrame::entryVFrame(const frame* fr, const RegisterMap* reg_map, JavaThread* thread)
: externalVFrame(fr, reg_map, thread) { }

// top-frame will be skipped
vframeStream::vframeStream(JavaThread* thread, frame top_frame,
  bool stop_at_java_call_stub) : vframeStreamCommon(thread) {
  _stop_at_java_call_stub = stop_at_java_call_stub;

  // skip top frame, as it may not be at safepoint
  _frame  = top_frame.sender(&_reg_map);
  while (!fill_from_frame()) {
    _frame = _frame.sender(&_reg_map);
  }
}

// Step back n frames, skip any pseudo frames in between.
// This function is used in Class.forName, Class.newInstance, Method.Invoke,
// AccessController.doPrivileged.
void vframeStreamCommon::security_get_caller_frame(int depth) {
  for (int n = 0; !at_end(); security_next()) {
    if (!method()->is_ignored_by_security_stack_walk()) {
      if (n == depth) {
        // We have reached the desired depth; return.
        return;
      }
      n++;  // this is a non-skipped frame; count it against the depth
    }
  }
  // NOTE: At this point there were not enough frames on the stack
  // to walk to depth.  Callers of this method have to check for at_end.
}

void vframeStreamCommon::security_next() {
  if (method()->is_prefixed_native()) {
    skip_prefixed_method_and_wrappers();  // calls next()
  } else {
    next();
  }
}

void vframeStreamCommon::skip_prefixed_method_and_wrappers() {
  while (!at_end()) {
    next();
    break; // didn't find the prefix, can't be a wrapper
  }
}

void vframeStreamCommon::skip_reflection_related_frames() {
  while (!at_end() &&
          (method()->method_holder()->is_subclass_of(SystemDictionary::reflect_MethodAccessorImpl_klass()) ||
           method()->method_holder()->is_subclass_of(SystemDictionary::reflect_ConstructorAccessorImpl_klass()))) {
    next();
  }
}
