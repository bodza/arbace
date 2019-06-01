#include "precompiled.hpp"

#include "classfile/systemDictionary.hpp"
#include "classfile/vmSymbols.hpp"
#include "memory/resourceArea.hpp"
#include "memory/universe.hpp"
#include "oops/annotations.hpp"
#include "oops/instanceKlass.hpp"
#include "oops/oop.inline.hpp"
#include "oops/fieldStreams.hpp"
#include "runtime/fieldDescriptor.hpp"
#include "runtime/handles.inline.hpp"
#include "runtime/signature.hpp"

oop fieldDescriptor::loader() const {
  return _cp->pool_holder()->class_loader();
}

Symbol* fieldDescriptor::generic_signature() const {
  if (!has_generic_signature()) {
    return NULL;
  }

  int idx = 0;
  InstanceKlass* ik = field_holder();
  for (AllFieldStream fs(ik); !fs.done(); fs.next()) {
    if (idx == _index) {
      return fs.generic_signature();
    } else {
      idx ++;
    }
  }
  ShouldNotReachHere();
  return NULL;
}

AnnotationArray* fieldDescriptor::annotations() const {
  InstanceKlass* ik = field_holder();
  Array<AnnotationArray*>* md = ik->fields_annotations();
  if (md == NULL)
    return NULL;
  return md->at(index());
}

AnnotationArray* fieldDescriptor::type_annotations() const {
  InstanceKlass* ik = field_holder();
  Array<AnnotationArray*>* type_annos = ik->fields_type_annotations();
  if (type_annos == NULL)
    return NULL;
  return type_annos->at(index());
}

constantTag fieldDescriptor::initial_value_tag() const {
  return constants()->tag_at(initial_value_index());
}

jint fieldDescriptor::int_initial_value() const {
  return constants()->int_at(initial_value_index());
}

jlong fieldDescriptor::long_initial_value() const {
  return constants()->long_at(initial_value_index());
}

jfloat fieldDescriptor::float_initial_value() const {
  return constants()->float_at(initial_value_index());
}

jdouble fieldDescriptor::double_initial_value() const {
  return constants()->double_at(initial_value_index());
}

oop fieldDescriptor::string_initial_value(TRAPS) const {
  return constants()->uncached_string_at(initial_value_index(), THREAD);
}

void fieldDescriptor::reinitialize(InstanceKlass* ik, int index) {
  if (_cp.is_null() || field_holder() != ik) {
    _cp = constantPoolHandle(Thread::current(), ik->constants());
  }
  FieldInfo* f = ik->field(index);

  _access_flags = accessFlags_from(f->access_flags());
  guarantee(f->name_index() != 0 && f->signature_index() != 0, "bad constant pool index for fieldDescriptor");
  _index = index;
  verify();
}
