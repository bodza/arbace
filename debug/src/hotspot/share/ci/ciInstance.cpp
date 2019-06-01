#include "precompiled.hpp"

#include "ci/ciConstant.hpp"
#include "ci/ciField.hpp"
#include "ci/ciInstance.hpp"
#include "ci/ciInstanceKlass.hpp"
#include "ci/ciUtilities.inline.hpp"
#include "classfile/systemDictionary.hpp"
#include "oops/oop.inline.hpp"

// ciInstance
//
// This class represents an instanceOop in the HotSpot virtual
// machine.

// ------------------------------------------------------------------
// ciObject::java_mirror_type
ciType* ciInstance::java_mirror_type() {
  VM_ENTRY_MARK;
  oop m = get_oop();
  // Return NULL if it is not java.lang.Class.
  if (m == NULL || m->klass() != SystemDictionary::Class_klass()) {
    return NULL;
  }
  // Return either a primitive type or a klass.
  if (java_lang_Class::is_primitive(m)) {
    return ciType::make(java_lang_Class::primitive_type(m));
  } else {
    Klass* k = java_lang_Class::as_Klass(m);
    return CURRENT_THREAD_ENV->get_klass(k);
  }
}

// ------------------------------------------------------------------
// ciInstance::field_value_impl
ciConstant ciInstance::field_value_impl(BasicType field_btype, int offset) {
  oop obj = get_oop();
  switch(field_btype) {
    case T_BYTE:    return ciConstant(field_btype, obj->byte_field(offset));
    case T_CHAR:    return ciConstant(field_btype, obj->char_field(offset));
    case T_SHORT:   return ciConstant(field_btype, obj->short_field(offset));
    case T_BOOLEAN: return ciConstant(field_btype, obj->bool_field(offset));
    case T_INT:     return ciConstant(field_btype, obj->int_field(offset));
    case T_FLOAT:   return ciConstant(obj->float_field(offset));
    case T_DOUBLE:  return ciConstant(obj->double_field(offset));
    case T_LONG:    return ciConstant(obj->long_field(offset));
    case T_OBJECT:  // fall through
    case T_ARRAY: {
      oop o = obj->obj_field(offset);

      // A field will be "constant" if it is known always to be
      // a non-null reference to an instance of a particular class,
      // or to a particular array.  This can happen even if the instance
      // or array is not perm.  In such a case, an "unloaded" ciArray
      // or ciInstance is created.  The compiler may be able to use
      // information about the object's class (which is exact) or length.

      if (o == NULL) {
        return ciConstant(field_btype, ciNullObject::make());
      } else {
        return ciConstant(field_btype, CURRENT_ENV->get_object(o));
      }
    }
    default:
      fatal("no field value: %s", type2name(field_btype));
      return ciConstant();
  }
}

// ------------------------------------------------------------------
// ciInstance::field_value
//
// Constant value of a field.
ciConstant ciInstance::field_value(ciField* field) {

  GUARDED_VM_ENTRY(return field_value_impl(field->type()->basic_type(), field->offset());)
}

// ------------------------------------------------------------------
// ciInstance::field_value_by_offset
//
// Constant value of a field at the specified offset.
ciConstant ciInstance::field_value_by_offset(int field_offset) {
  ciInstanceKlass* ik = klass()->as_instance_klass();
  ciField* field = ik->get_field_by_offset(field_offset, false);
  if (field == NULL)
    return ciConstant();  // T_ILLEGAL
  return field_value(field);
}

// ------------------------------------------------------------------
// ciInstance::print_impl
//
// Implementation of the print method.
void ciInstance::print_impl(outputStream* st) {
  st->print(" type=");
  klass()->print(st);
}

ciKlass* ciInstance::java_lang_Class_klass() {
  VM_ENTRY_MARK;
  return CURRENT_ENV->get_metadata(java_lang_Class::as_Klass(get_oop()))->as_klass();
}
