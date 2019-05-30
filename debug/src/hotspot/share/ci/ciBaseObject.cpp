#include "precompiled.hpp"
#include "ci/ciBaseObject.hpp"
#include "ci/ciUtilities.hpp"
#include "gc/shared/collectedHeap.inline.hpp"

// ------------------------------------------------------------------
// ciBaseObject::set_ident
//
// Set the unique identity number of a ciBaseObject.
void ciBaseObject::set_ident(uint id) {
  assert((_ident >> FLAG_BITS) == 0, "must only initialize once");
  assert( id < ((uint)1 << (BitsPerInt-FLAG_BITS)), "id too big");
  _ident = _ident + (id << FLAG_BITS);
}

// ------------------------------------------------------------------
// ciBaseObject::ident
//
// Report the unique identity number of a ciBaseObject.
uint ciBaseObject::ident() {
  uint id = _ident >> FLAG_BITS;
  assert(id != 0, "must be initialized");
  return id;
}
