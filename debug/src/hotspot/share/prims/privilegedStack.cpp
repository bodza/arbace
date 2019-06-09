#include "precompiled.hpp"

#include "memory/allocation.inline.hpp"
#include "oops/instanceKlass.hpp"
#include "oops/method.hpp"
#include "oops/oop.inline.hpp"
#include "prims/privilegedStack.hpp"
#include "runtime/thread.inline.hpp"
#include "runtime/vframe.inline.hpp"

void PrivilegedElement::initialize(vframeStream* vfst, oop context, PrivilegedElement* next, TRAPS) {
  Method* method      = vfst->method();
  _klass              = method->method_holder();
  _privileged_context = context;
  _frame_id           = vfst->frame_id();
  _next               = next;
}

void PrivilegedElement::oops_do(OopClosure* f) {
  PrivilegedElement *cur = this;
  do {
    f->do_oop((oop*) &cur->_privileged_context);
    cur = cur->_next;
  } while (cur != NULL);
}
