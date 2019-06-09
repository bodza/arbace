#include "precompiled.hpp"

#include "ci/ciTypeArray.hpp"
#include "ci/ciUtilities.inline.hpp"
#include "oops/typeArrayOop.inline.hpp"

// ciTypeArray
//
// This class represents an typeArrayOop in the HotSpot virtual
// machine.

// Implementation of the char_at method.
jchar ciTypeArray::char_at(int index) {
  VM_ENTRY_MARK;
  jchar c = get_typeArrayOop()->char_at(index);
  return c;
}

// Implementation of the byte_at method.
jbyte ciTypeArray::byte_at(int index) {
  VM_ENTRY_MARK;
  return get_typeArrayOop()->byte_at(index);
}
