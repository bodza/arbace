#include "precompiled.hpp"
#include "classfile/javaClasses.inline.hpp"
#include "ci/ciCallSite.hpp"
#include "ci/ciUtilities.inline.hpp"

// ciCallSite

bool ciCallSite::is_constant_call_site() {
  return klass()->is_subclass_of(CURRENT_ENV->ConstantCallSite_klass());
}
bool ciCallSite::is_mutable_call_site() {
  return klass()->is_subclass_of(CURRENT_ENV->MutableCallSite_klass());
}
bool ciCallSite::is_volatile_call_site() {
  return klass()->is_subclass_of(CURRENT_ENV->VolatileCallSite_klass());
}

// ------------------------------------------------------------------
// ciCallSite::get_target
//
// Return the target MethodHandle of this CallSite.
ciMethodHandle* ciCallSite::get_target() const {
  VM_ENTRY_MARK;
  oop method_handle_oop = java_lang_invoke_CallSite::target(get_oop());
  return CURRENT_ENV->get_object(method_handle_oop)->as_method_handle();
}

// ------------------------------------------------------------------
// ciCallSite::print
//
// Print debugging information about the CallSite.
void ciCallSite::print() {
  Unimplemented();
}
