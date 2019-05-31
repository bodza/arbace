#include "precompiled.hpp"
#include "ci/ciExceptionHandler.hpp"
#include "ci/ciUtilities.inline.hpp"
#include "runtime/handles.inline.hpp"

// ciExceptionHandler
//
// This class represents an exception handler for a method.

// ------------------------------------------------------------------
// ciExceptionHandler::catch_klass
//
// Get the exception klass that this handler catches.
ciInstanceKlass* ciExceptionHandler::catch_klass() {
  VM_ENTRY_MARK;
  if (_catch_klass == NULL) {
    bool will_link;
    constantPoolHandle cpool(_loading_klass->get_instanceKlass()->constants());
    ciKlass* k = CURRENT_ENV->get_klass_by_index(cpool, _catch_klass_index, will_link, _loading_klass);
    if (!will_link && k->is_loaded()) {
      GUARDED_VM_ENTRY(
        k = CURRENT_ENV->get_unloaded_klass(_loading_klass, k->name());
      )
    }
    _catch_klass = k->as_instance_klass();
  }
  return _catch_klass;
}

// ------------------------------------------------------------------
// ciExceptionHandler::print()
void ciExceptionHandler::print() {
  tty->print("<ciExceptionHandler start=%d limit=%d"
             " handler_bci=%d ex_klass_index=%d",
             start(), limit(), handler_bci(), catch_klass_index());
  if (_catch_klass != NULL) {
    tty->print(" ex_klass=");
    _catch_klass->print();
  }
  tty->print(">");
}
