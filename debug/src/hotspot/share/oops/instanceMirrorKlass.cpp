#include "precompiled.hpp"

#include "classfile/javaClasses.hpp"
#include "classfile/systemDictionary.hpp"
#include "gc/shared/collectedHeap.inline.hpp"
#include "memory/iterator.inline.hpp"
#include "memory/oopFactory.hpp"
#include "oops/instanceKlass.hpp"
#include "oops/instanceMirrorKlass.hpp"
#include "oops/instanceOop.hpp"
#include "oops/oop.inline.hpp"
#include "oops/symbol.hpp"
#include "runtime/handles.inline.hpp"
#include "utilities/macros.hpp"

int InstanceMirrorKlass::_offset_of_static_fields = 0;

int InstanceMirrorKlass::instance_size(Klass* k) {
  if (k != NULL && k->is_instance_klass()) {
    return align_object_size(size_helper() + InstanceKlass::cast(k)->static_field_size());
  }
  return size_helper();
}

instanceOop InstanceMirrorKlass::allocate_instance(Klass* k, TRAPS) {
  // Query before forming handle.
  int size = instance_size(k);

  // Since mirrors can be variable sized because of the static fields, store
  // the size in the mirror itself.
  return (instanceOop)Universe::heap()->class_allocate(this, size, CHECK_NULL);
}

int InstanceMirrorKlass::oop_size(oop obj) const {
  return java_lang_Class::oop_size(obj);
}

int InstanceMirrorKlass::compute_static_oop_field_count(oop obj) {
  Klass* k = java_lang_Class::as_Klass(obj);
  if (k != NULL && k->is_instance_klass()) {
    return InstanceKlass::cast(k)->static_oop_field_count();
  }
  return 0;
}
