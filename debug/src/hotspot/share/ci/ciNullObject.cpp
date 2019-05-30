#include "precompiled.hpp"
#include "ci/ciNullObject.hpp"
#include "ci/ciUtilities.hpp"

// ciNullObject
//
// This class represents a null reference.  It can be used
// as a class loader or as the null constant.

// ------------------------------------------------------------------
// ciNullObject::print_impl
//
// Implementation of the print method.
void ciNullObject::print_impl(outputStream* st) {
  ciObject::print_impl(st);
  st->print(" unique");
}

// ------------------------------------------------------------------
// ciNullObject::make
//
// Get the distinguished instance of this class.
ciNullObject* ciNullObject::make() {
  return CURRENT_ENV->_null_object_instance->as_null_object();
}
