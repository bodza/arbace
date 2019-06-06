#include "precompiled.hpp"

#include "classfile/javaClasses.hpp"
#include "classfile/systemDictionary.hpp"
#include "classfile/vmSymbols.hpp"
#include "code/codeCache.hpp"
#include "code/debugInfoRec.hpp"
#include "code/nmethod.hpp"
#include "code/pcDesc.hpp"
#include "code/scopeDesc.hpp"
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
  if (_fr.is_entry_frame() && _fr.is_first_frame())
    return NULL;
  frame s = _fr.real_sender(&temp_map);
  if (s.is_first_frame())
    return NULL;
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

// ------------- cChunk --------------

entryVFrame::entryVFrame(const frame* fr, const RegisterMap* reg_map, JavaThread* thread)
: externalVFrame(fr, reg_map, thread) { }

// top-frame will be skipped
vframeStream::vframeStream(JavaThread* thread, frame top_frame, bool stop_at_java_call_stub) : vframeStreamCommon(thread) {
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
