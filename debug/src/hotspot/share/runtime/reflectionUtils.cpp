#include "precompiled.hpp"
#include "classfile/javaClasses.hpp"
#include "memory/universe.hpp"
#include "runtime/reflectionUtils.hpp"

KlassStream::KlassStream(InstanceKlass* klass, bool local_only,
                         bool classes_only, bool walk_defaults) {
  _klass = _base_klass = klass;
  _base_class_search_defaults = false;
  _defaults_checked = false;
  if (classes_only) {
    _interfaces = Universe::the_empty_klass_array();
  } else {
    _interfaces = klass->transitive_interfaces();
  }
  _interface_index = _interfaces->length();
  _local_only = local_only;
  _classes_only = classes_only;
  _walk_defaults = walk_defaults;
}

bool KlassStream::eos() {
  if (index() >= 0) return false;
  if (_local_only) return true;
  if (!_klass->is_interface() && _klass->super() != NULL) {
    // go up superclass chain (not for interfaces)
    _klass = InstanceKlass::cast(_klass->super());
  // Next for method walks, walk default methods
  } else if (_walk_defaults && (_defaults_checked == false)  && (_base_klass->default_methods() != NULL)) {
      _base_class_search_defaults = true;
      _klass = _base_klass;
      _defaults_checked = true;
  } else {
    // Next walk transitive interfaces
    if (_interface_index > 0) {
      _klass = InstanceKlass::cast(_interfaces->at(--_interface_index));
    } else {
      return true;
    }
  }
  _index = length();
  next();
  return eos();
}

GrowableArray<FilteredField*> *FilteredFieldsMap::_filtered_fields = new (ResourceObj::C_HEAP, mtInternal) GrowableArray<FilteredField*>(3,true);

void FilteredFieldsMap::initialize() {
  int offset = reflect_ConstantPool::oop_offset();
  _filtered_fields->append(new FilteredField(SystemDictionary::reflect_ConstantPool_klass(), offset));
  offset = reflect_UnsafeStaticFieldAccessorImpl::base_offset();
  _filtered_fields->append(new FilteredField(SystemDictionary::reflect_UnsafeStaticFieldAccessorImpl_klass(), offset));
}

int FilteredFieldStream::field_count() {
  int numflds = 0;
  for (;!eos(); next()) {
    numflds++;
  }
  return numflds;
}
