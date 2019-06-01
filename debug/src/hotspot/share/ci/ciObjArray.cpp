#include "precompiled.hpp"

#include "ci/ciNullObject.hpp"
#include "ci/ciObjArray.hpp"
#include "ci/ciUtilities.inline.hpp"
#include "oops/objArrayOop.inline.hpp"

// ciObjArray
//
// This class represents an objArrayOop in the HotSpot virtual
// machine.

ciObject* ciObjArray::obj_at(int index) {
  VM_ENTRY_MARK;
  objArrayOop array = get_objArrayOop();
  if (index < 0 || index >= array->length()) return NULL;
  oop o = array->obj_at(index);
  if (o == NULL) {
    return ciNullObject::make();
  } else {
    return CURRENT_ENV->get_object(o);
  }
}
