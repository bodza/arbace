#include "precompiled.hpp"

#include "ci/ciBaseObject.hpp"
#include "ci/ciUtilities.hpp"
#include "gc/shared/collectedHeap.inline.hpp"

// Set the unique identity number of a ciBaseObject.
void ciBaseObject::set_ident(uint id) {
  _ident = _ident + (id << FLAG_BITS);
}

// Report the unique identity number of a ciBaseObject.
uint ciBaseObject::ident() {
  uint id = _ident >> FLAG_BITS;
  return id;
}
