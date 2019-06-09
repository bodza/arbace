#include "precompiled.hpp"

#include "ci/ciArray.hpp"
#include "ci/ciArrayKlass.hpp"
#include "ci/ciConstant.hpp"
#include "ci/ciKlass.hpp"
#include "ci/ciUtilities.inline.hpp"
#include "oops/objArrayOop.inline.hpp"
#include "oops/oop.inline.hpp"
#include "oops/typeArrayOop.inline.hpp"

// ciArray
//
// This class represents an arrayOop in the HotSpot virtual
// machine.
static BasicType fixup_element_type(BasicType bt) {
  if (bt == T_ARRAY)    return T_OBJECT;
  if (bt == T_BOOLEAN)  return T_BYTE;
  return bt;
}

ciConstant ciArray::element_value_impl(BasicType elembt, arrayOop ary, int index) {
  if (ary == NULL)
    return ciConstant();
  if (index < 0 || index >= ary->length())
    return ciConstant();
  ArrayKlass* ak = (ArrayKlass*) ary->klass();
  BasicType abt = ak->element_type();
  if (fixup_element_type(elembt) !=
      fixup_element_type(abt))
    return ciConstant();
  switch (elembt) {
  case T_ARRAY:
  case T_OBJECT:
    {
      objArrayOop objary = (objArrayOop) ary;
      oop elem = objary->obj_at(index);
      ciEnv* env = ciEnv::current();
      ciObject* box = env->get_object(elem);
      return ciConstant(T_OBJECT, box);
    }
  default:
    break;
  }
  typeArrayOop tary = (typeArrayOop) ary;
  jint value = 0;
  switch (elembt) {
  case T_LONG:          return ciConstant(tary->long_at(index));
  case T_FLOAT:         return ciConstant(tary->float_at(index));
  case T_DOUBLE:        return ciConstant(tary->double_at(index));
  default:              return ciConstant();
  case T_BYTE:          value = tary->byte_at(index);           break;
  case T_BOOLEAN:       value = tary->byte_at(index) & 1;       break;
  case T_SHORT:         value = tary->short_at(index);          break;
  case T_CHAR:          value = tary->char_at(index);           break;
  case T_INT:           value = tary->int_at(index);            break;
  }
  return ciConstant(elembt, value);
}

// Current value of an element.
// Returns T_ILLEGAL if there is no element at the given index.
ciConstant ciArray::element_value(int index) {
  BasicType elembt = element_basic_type();
  GUARDED_VM_ENTRY(
    return element_value_impl(elembt, get_arrayOop(), index);
  )
}

// Current value of an element at the specified offset.
// Returns T_ILLEGAL if there is no element at the given offset.
ciConstant ciArray::element_value_by_offset(intptr_t element_offset) {
  BasicType elembt = element_basic_type();
  intptr_t shift  = exact_log2(type2aelembytes(elembt));
  intptr_t header = arrayOopDesc::base_offset_in_bytes(elembt);
  intptr_t index = (element_offset - header) >> shift;
  intptr_t offset = header + ((intptr_t)index << shift);
  if (offset != element_offset || index != (jint)index || index < 0 || index >= length()) {
    return ciConstant();
  }
  return element_value((jint) index);
}

// Implementation of the print method.
void ciArray::print_impl(outputStream* st) {
  st->print(" length=%d type=", length());
  klass()->print(st);
}
