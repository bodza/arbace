#include "precompiled.hpp"

#include "ci/ciInstance.hpp"
#include "ci/ciMethodType.hpp"
#include "ci/ciUtilities.inline.hpp"
#include "classfile/javaClasses.hpp"

ciType* ciMethodType::class_to_citype(oop klass_oop) const {
  if (java_lang_Class::is_primitive(klass_oop)) {
    BasicType bt = java_lang_Class::primitive_type(klass_oop);
    return ciType::make(bt);
  } else {
    Klass* k = java_lang_Class::as_Klass(klass_oop);
    return CURRENT_ENV->get_klass(k);
  }
}

ciType* ciMethodType::rtype() const {
  GUARDED_VM_ENTRY(
    oop rtype = java_lang_invoke_MethodType::rtype(get_oop());
    return class_to_citype(rtype);
  )
}

int ciMethodType::ptype_count() const {
  GUARDED_VM_ENTRY(return java_lang_invoke_MethodType::ptype_count(get_oop());)
}

int ciMethodType::ptype_slot_count() const {
  GUARDED_VM_ENTRY(return java_lang_invoke_MethodType::ptype_slot_count(get_oop());)
}

ciType* ciMethodType::ptype_at(int index) const {
  GUARDED_VM_ENTRY(
    oop ptype = java_lang_invoke_MethodType::ptype(get_oop(), index);
    return class_to_citype(ptype);
  )
}
