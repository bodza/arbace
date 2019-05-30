#include "precompiled.hpp"
#include "classfile/javaClasses.inline.hpp"
#include "code/codeCache.hpp"
#include "code/debugInfoRec.hpp"
#include "code/nmethod.hpp"
#include "code/pcDesc.hpp"
#include "code/scopeDesc.hpp"
#include "interpreter/interpreter.hpp"
#include "interpreter/oopMapCache.hpp"
#include "oops/instanceKlass.hpp"
#include "oops/oop.inline.hpp"
#include "runtime/basicLock.hpp"
#include "runtime/frame.inline.hpp"
#include "runtime/handles.inline.hpp"
#include "runtime/monitorChunk.hpp"
#include "runtime/signature.hpp"
#include "runtime/stubRoutines.hpp"
#include "runtime/vframeArray.hpp"
#include "runtime/vframe_hp.hpp"

// ------------- compiledVFrame --------------

StackValueCollection* compiledVFrame::locals() const {
  // Natives has no scope
  if (scope() == NULL) return new StackValueCollection(0);
  GrowableArray<ScopeValue*>*  scv_list = scope()->locals();
  if (scv_list == NULL) return new StackValueCollection(0);

  // scv_list is the list of ScopeValues describing the JVM stack state.
  // There is one scv_list entry for every JVM stack state in use.
  int length = scv_list->length();
  StackValueCollection* result = new StackValueCollection(length);
  for (int i = 0; i < length; i++) {
    result->add(create_stack_value(scv_list->at(i)));
  }

  return result;
}

void compiledVFrame::set_locals(StackValueCollection* values) const {
  fatal("Should use update_local for each local update");
}

void compiledVFrame::update_local(BasicType type, int index, jvalue value) {
  assert(index >= 0 && index < method()->max_locals(), "out of bounds");
}

void compiledVFrame::update_stack(BasicType type, int index, jvalue value) {
  assert(index >= 0 && index < method()->max_stack(), "out of bounds");
}

void compiledVFrame::update_monitor(int index, MonitorInfo* val) {
  assert(index >= 0, "out of bounds");
}

StackValueCollection* compiledVFrame::expressions() const {
  // Natives has no scope
  if (scope() == NULL) return new StackValueCollection(0);
  GrowableArray<ScopeValue*>*  scv_list = scope()->expressions();
  if (scv_list == NULL) return new StackValueCollection(0);

  // scv_list is the list of ScopeValues describing the JVM stack state.
  // There is one scv_list entry for every JVM stack state in use.
  int length = scv_list->length();
  StackValueCollection* result = new StackValueCollection(length);
  for (int i = 0; i < length; i++) {
    result->add(create_stack_value(scv_list->at(i)));
  }

  return result;
}

// The implementation of the following two methods was factorized into the
// class StackValue because it is also used from within deoptimization.cpp for
// rematerialization and relocking of non-escaping objects.

StackValue *compiledVFrame::create_stack_value(ScopeValue *sv) const {
  return StackValue::create_stack_value(&_fr, register_map(), sv);
}

BasicLock* compiledVFrame::resolve_monitor_lock(Location location) const {
  return StackValue::resolve_monitor_lock(&_fr, location);
}

GrowableArray<MonitorInfo*>* compiledVFrame::monitors() const {
  // Natives has no scope
  if (scope() == NULL) {
    CompiledMethod* nm = code();
    Method* method = nm->method();
    assert(method->is_native() || nm->is_aot(), "Expect a native method or precompiled method");
    if (!method->is_synchronized()) {
      return new GrowableArray<MonitorInfo*>(0);
    }
    // This monitor is really only needed for UseBiasedLocking, but
    // return it in all cases for now as it might be useful for stack
    // traces and tools as well
    GrowableArray<MonitorInfo*> *monitors = new GrowableArray<MonitorInfo*>(1);
    // Casting away const
    frame& fr = (frame&) _fr;
    MonitorInfo* info = new MonitorInfo(
        fr.get_native_receiver(), fr.get_native_monitor(), false, false);
    monitors->push(info);
    return monitors;
  }
  GrowableArray<MonitorValue*>* monitors = scope()->monitors();
  if (monitors == NULL) {
    return new GrowableArray<MonitorInfo*>(0);
  }
  GrowableArray<MonitorInfo*>* result = new GrowableArray<MonitorInfo*>(monitors->length());
  for (int index = 0; index < monitors->length(); index++) {
    MonitorValue* mv = monitors->at(index);
    ScopeValue*   ov = mv->owner();
    StackValue *owner_sv = create_stack_value(ov); // it is an oop
    if (ov->is_object() && owner_sv->obj_is_scalar_replaced()) { // The owner object was scalar replaced
      assert(mv->eliminated(), "monitor should be eliminated for scalar replaced object");
      // Put klass for scalar replaced object.
      ScopeValue* kv = ((ObjectValue *)ov)->klass();
      assert(kv->is_constant_oop(), "klass should be oop constant for scalar replaced object");
      Handle k(Thread::current(), ((ConstantOopReadValue*)kv)->value()());
      assert(java_lang_Class::is_instance(k()), "must be");
      result->push(new MonitorInfo(k(), resolve_monitor_lock(mv->basic_lock()), mv->eliminated(), true));
    } else {
      result->push(new MonitorInfo(owner_sv->get_obj()(), resolve_monitor_lock(mv->basic_lock()), mv->eliminated(), false));
    }
  }

  return result;
}

compiledVFrame::compiledVFrame(const frame* fr, const RegisterMap* reg_map, JavaThread* thread, CompiledMethod* nm)
: javaVFrame(fr, reg_map, thread) {
  _scope  = NULL;
  _vframe_id = 0;
  // Compiled method (native stub or Java code)
  // native wrappers have no scope data, it is implied
  if (!nm->is_compiled() || !nm->as_compiled_method()->is_native_method()) {
      _scope  = nm->scope_desc_at(_fr.pc());
  }
}

compiledVFrame::compiledVFrame(const frame* fr, const RegisterMap* reg_map, JavaThread* thread, ScopeDesc* scope, int vframe_id)
: javaVFrame(fr, reg_map, thread) {
  _scope  = scope;
  _vframe_id = vframe_id;
  guarantee(_scope != NULL, "scope must be present");
}

bool compiledVFrame::is_top() const {
  // FIX IT: Remove this when new native stubs are in place
  if (scope() == NULL) return true;
  return scope()->is_top();
}

CompiledMethod* compiledVFrame::code() const {
  return CodeCache::find_compiled(_fr.pc());
}

Method* compiledVFrame::method() const {
  if (scope() == NULL) {
    // native nmethods have no scope the method is implied
    nmethod* nm = code()->as_nmethod();
    assert(nm->is_native_method(), "must be native");
    return nm->method();
  }
  return scope()->method();
}

int compiledVFrame::bci() const {
  int raw = raw_bci();
  return raw == SynchronizationEntryBCI ? 0 : raw;
}

int compiledVFrame::raw_bci() const {
  if (scope() == NULL) {
    // native nmethods have no scope the method/bci is implied
    nmethod* nm = code()->as_nmethod();
    assert(nm->is_native_method(), "must be native");
    return 0;
  }
  return scope()->bci();
}

bool compiledVFrame::should_reexecute() const {
  if (scope() == NULL) {
    // native nmethods have no scope the method/bci is implied
    nmethod* nm = code()->as_nmethod();
    assert(nm->is_native_method(), "must be native");
    return false;
  }
  return scope()->should_reexecute();
}

vframe* compiledVFrame::sender() const {
  const frame f = fr();
  if (scope() == NULL) {
    // native nmethods have no scope the method/bci is implied
    nmethod* nm = code()->as_nmethod();
    assert(nm->is_native_method(), "must be native");
    return vframe::sender();
  } else {
    return scope()->is_top()
      ? vframe::sender()
      : new compiledVFrame(&f, register_map(), thread(), scope()->sender(), vframe_id() + 1);
  }
}
