#include "precompiled.hpp"

#include "code/codeCache.hpp"
#include "interpreter/interpreter.hpp"
#include "oops/method.inline.hpp"
#include "oops/oop.inline.hpp"
#include "oops/symbol.hpp"
#include "runtime/frame.inline.hpp"
#include "runtime/rframe.hpp"
#include "runtime/vframe.hpp"
#include "runtime/vframe_hp.hpp"

static RFrame*const  noCaller    = (RFrame*) 0x1;               // no caller (i.e., initial frame)
static RFrame*const  noCallerYet = (RFrame*) 0x0;               // caller not yet computed

RFrame::RFrame(frame fr, JavaThread* thread, RFrame*const callee) :
  _fr(fr), _thread(thread), _callee(callee), _num(callee ? callee->num() + 1 : 0) {
  _caller = (RFrame*)noCallerYet;
  _invocations = 0;
  _distance = 0;
}

void RFrame::set_distance(int d) {
  _distance = d;
}

InterpretedRFrame::InterpretedRFrame(frame fr, JavaThread* thread, RFrame*const callee)
: RFrame(fr, thread, callee) {
  RegisterMap map(thread, false);
  _vf     = javaVFrame::cast(vframe::new_vframe(&_fr, &map, thread));
  _method = _vf->method();
  init();
}

InterpretedRFrame::InterpretedRFrame(frame fr, JavaThread* thread, Method* m)
: RFrame(fr, thread, NULL) {
  RegisterMap map(thread, false);
  _vf     = javaVFrame::cast(vframe::new_vframe(&_fr, &map, thread));
  _method = m;

  init();
}

CompiledRFrame::CompiledRFrame(frame fr, JavaThread* thread, RFrame*const  callee)
: RFrame(fr, thread, callee) {
  init();
}

CompiledRFrame::CompiledRFrame(frame fr, JavaThread* thread)
: RFrame(fr, thread, NULL) {
  init();
}

DeoptimizedRFrame::DeoptimizedRFrame(frame fr, JavaThread* thread, RFrame*const  callee)
: InterpretedRFrame(fr, thread, callee) { }

RFrame* RFrame::new_RFrame(frame fr, JavaThread* thread, RFrame*const  callee) {
  RFrame* rf = NULL;
  int dist = callee ? callee->distance() : -1;
  if (fr.is_interpreted_frame()) {
    rf = new InterpretedRFrame(fr, thread, callee);
    dist++;
  } else if (fr.is_compiled_frame()) {
    // Even deopted frames look compiled because the deopt
    // is invisible until it happens.
    rf = new CompiledRFrame(fr, thread, callee);
  } else {
    ShouldNotReachHere();
  }
  if (rf != NULL) {
    rf->set_distance(dist);
    rf->init();
  }
  return rf;
}

RFrame* RFrame::caller() {
  if (_caller != noCallerYet) return (_caller == noCaller) ? NULL : _caller;    // already computed caller

  // caller not yet computed; do it now
  if (_fr.is_first_java_frame()) {
    _caller = (RFrame*)noCaller;
    return NULL;
  }

  RegisterMap map(_thread, false);
  frame sender = _fr.real_sender(&map);
  if (sender.is_java_frame()) {
    _caller = new_RFrame(sender, thread(), this);
    return _caller;
  }

  // Real caller is not java related
  _caller = (RFrame*)noCaller;
  return NULL;
}

int InterpretedRFrame::cost() const {
  return _method->code_size();    // fix this
  //return _method->estimated_inline_cost(_receiverKlass);
}

int CompiledRFrame::cost() const {
  CompiledMethod* nm = top_method()->code();
  if (nm != NULL) {
    return nm->insts_size();
  } else {
    return top_method()->code_size();
  }
}

void CompiledRFrame::init() {
  RegisterMap map(thread(), false);
  vframe* vf = vframe::new_vframe(&_fr, &map, thread());
  _nm = compiledVFrame::cast(vf)->code()->as_nmethod();
  vf = vf->top();
  _vf = javaVFrame::cast(vf);
  _method = CodeCache::find_nmethod(_fr.pc())->method();
}

void InterpretedRFrame::init() {
  _invocations = _method->invocation_count() + _method->backedge_count();
}

void RFrame::print(const char* kind) { }

void CompiledRFrame::print() {
  RFrame::print("comp");
}

void InterpretedRFrame::print() {
  RFrame::print("int.");
}

void DeoptimizedRFrame::print() {
  RFrame::print("deopt.");
}
