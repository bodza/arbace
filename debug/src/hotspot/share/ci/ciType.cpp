#include "precompiled.hpp"

#include "ci/ciEnv.hpp"
#include "ci/ciType.hpp"
#include "ci/ciUtilities.inline.hpp"
#include "classfile/systemDictionary.hpp"
#include "memory/resourceArea.hpp"
#include "oops/oop.inline.hpp"

ciType* ciType::_basic_types[T_CONFLICT+1];

// ciType
//
// This class represents either a class (T_OBJECT), array (T_ARRAY),
// or one of the primitive types such as T_INT.

ciType::ciType(BasicType basic_type) : ciMetadata() {
  _basic_type = basic_type;
}

ciType::ciType(Klass* k) : ciMetadata(k) {
  _basic_type = k->is_array_klass() ? T_ARRAY : T_OBJECT;
}

bool ciType::is_subtype_of(ciType* type) {
  if (this == type)  return true;
  if (is_klass() && type->is_klass())
    return this->as_klass()->is_subtype_of(type->as_klass());
  return false;
}

// Return the name of this type
const char* ciType::name() {
  if (is_primitive_type()) {
    return type2name(basic_type());
  } else {
    return as_klass()->name()->as_utf8();
  }
}

// Implementation of the print method.
void ciType::print_impl(outputStream* st) {
  st->print(" type=");
  print_name_on(st);
}

// Print the name of this type
void ciType::print_name_on(outputStream* st) {
  ResourceMark rm;
  st->print("%s", name());
}

ciInstance* ciType::java_mirror() {
  VM_ENTRY_MARK;
  return ciEnv::current(thread)->get_instance(Universe::java_mirror(basic_type()));
}

ciKlass* ciType::box_klass() {
  if (!is_primitive_type())  return this->as_klass();  // reference types are "self boxing"

  // Void is "boxed" with a null.
  if (basic_type() == T_VOID)  return NULL;

  VM_ENTRY_MARK;
  return ciEnv::current(thread)->get_instance_klass(SystemDictionary::box_klass(basic_type()));
}

// Produce the ciType for a given primitive BasicType.
// As a bonus, produce the right reference type for T_OBJECT.
// Does not work on T_ARRAY.
ciType* ciType::make(BasicType t) {
  // short, etc.
  // Note: Bare T_ADDRESS means a raw pointer type, not a return_address.
  if (t == T_OBJECT)  return ciEnv::_Object_klass;  // java/lang/Object
  return _basic_types[t];
}

// This class represents the type of a specific return address in the bytecodes.
ciReturnAddress::ciReturnAddress(int bci) : ciType(T_ADDRESS) {
  _bci = bci;
}

// Implementation of the print method.
void ciReturnAddress::print_impl(outputStream* st) {
  st->print(" bci=%d", _bci);
}

ciReturnAddress* ciReturnAddress::make(int bci) {
  GUARDED_VM_ENTRY(return ciEnv::current()->get_return_address(bci);)
}
