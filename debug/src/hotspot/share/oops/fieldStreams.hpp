#ifndef SHARE_VM_OOPS_FIELDSTREAMS_HPP
#define SHARE_VM_OOPS_FIELDSTREAMS_HPP

#include "oops/instanceKlass.hpp"
#include "oops/fieldInfo.hpp"
#include "runtime/fieldDescriptor.hpp"

// The is the base class for iteration over the fields array
// describing the declared fields in the class.  Several subclasses
// are provided depending on the kind of iteration required.  The
// JavaFieldStream is for iterating over regular Java fields and it
// generally the preferred iterator.  InternalFieldStream only
// iterates over fields that have been injected by the JVM.
// AllFieldStream exposes all fields and should only be used in rare
// cases.
class FieldStreamBase : public StackObj {
 protected:
  Array<u2>*          _fields;
  constantPoolHandle  _constants;
  int                 _index;
  int                 _limit;
  int                 _generic_signature_slot;
  fieldDescriptor     _fd_buf;

  FieldInfo* field() const { return FieldInfo::from_field_array(_fields, _index); }
  InstanceKlass* field_holder() const { return _constants->pool_holder(); }

  int init_generic_signature_start_slot() {
    int length = _fields->length();
    int num_fields = _index;
    int skipped_generic_signature_slots = 0;
    FieldInfo* fi;
    AccessFlags flags;
    /* Scan from 0 to the current _index. Count the number of generic
       signature slots for field[0] to field[_index - 1]. */
    for (int i = 0; i < _index; i++) {
      fi = FieldInfo::from_field_array(_fields, i);
      flags.set_flags(fi->access_flags());
      if (flags.field_has_generic_signature()) {
        length --;
        skipped_generic_signature_slots ++;
      }
    }
    /* Scan from the current _index. */
    for (int i = _index; i*FieldInfo::field_slots < length; i++) {
      fi = FieldInfo::from_field_array(_fields, i);
      flags.set_flags(fi->access_flags());
      if (flags.field_has_generic_signature()) {
        length --;
      }
      num_fields ++;
    }
    _generic_signature_slot = length + skipped_generic_signature_slots;
    return num_fields;
  }

  FieldStreamBase(Array<u2>* fields, const constantPoolHandle& constants, int start, int limit) {
    _fields = fields;
    _constants = constants;
    _index = start;
    int num_fields = init_generic_signature_start_slot();
    if (limit < start) {
      _limit = num_fields;
    } else {
      _limit = limit;
    }
  }

  FieldStreamBase(Array<u2>* fields, const constantPoolHandle& constants) {
    _fields = fields;
    _constants = constants;
    _index = 0;
    _limit = init_generic_signature_start_slot();
  }

 public:
  FieldStreamBase(InstanceKlass* klass) {
    _fields = klass->fields();
    _constants = klass->constants();
    _index = 0;
    _limit = klass->java_fields_count();
    init_generic_signature_start_slot();
  }

  // accessors
  int index() const                 { return _index; }

  void next() {
    if (access_flags().field_has_generic_signature()) {
      _generic_signature_slot ++;
    }
    _index += 1;
  }
  bool done() const { return _index >= _limit; }

  // Accessors for current field
  AccessFlags access_flags() const {
    AccessFlags flags;
    flags.set_flags(field()->access_flags());
    return flags;
  }

  void set_access_flags(u2 flags) const {
    field()->set_access_flags(flags);
  }

  void set_access_flags(AccessFlags flags) const {
    set_access_flags(flags.as_short());
  }

  Symbol* name() const {
    return field()->name(_constants);
  }

  Symbol* signature() const {
    return field()->signature(_constants);
  }

  Symbol* generic_signature() const {
    if (access_flags().field_has_generic_signature()) {
      int index = _fields->at(_generic_signature_slot);
      return _constants->symbol_at(index);
    } else {
      return NULL;
    }
  }

  int offset() const {
    return field()->offset();
  }

  int allocation_type() const {
    return field()->allocation_type();
  }

  void set_offset(int offset) {
    field()->set_offset(offset);
  }

  bool is_offset_set() const {
    return field()->is_offset_set();
  }

  bool is_contended() const {
    return field()->is_contended();
  }

  int contended_group() const {
    return field()->contended_group();
  }

  // bridge to a heavier API:
  fieldDescriptor& field_descriptor() const {
    fieldDescriptor& field = const_cast<fieldDescriptor&>(_fd_buf);
    field.reinitialize(field_holder(), _index);
    return field;
  }
};

// Iterate over only the internal fields
class JavaFieldStream : public FieldStreamBase {
 public:
  JavaFieldStream(const InstanceKlass* k): FieldStreamBase(k->fields(), k->constants(), 0, k->java_fields_count()) { }

  int name_index() const {
    return field()->name_index();
  }
  void set_name_index(int index) {
    field()->set_name_index(index);
  }
  int signature_index() const {
    return field()->signature_index();
  }
  void set_signature_index(int index) {
    field()->set_signature_index(index);
  }
  int generic_signature_index() const {
    if (access_flags().field_has_generic_signature()) {
      return _fields->at(_generic_signature_slot);
    } else {
      return 0;
    }
  }
  void set_generic_signature_index(int index) {
    if (access_flags().field_has_generic_signature()) {
      _fields->at_put(_generic_signature_slot, index);
    }
  }
  int initval_index() const {
    return field()->initval_index();
  }
  void set_initval_index(int index) {
    return field()->set_initval_index(index);
  }
};

// Iterate over only the internal fields
class InternalFieldStream : public FieldStreamBase {
 public:
  InternalFieldStream(InstanceKlass* k):      FieldStreamBase(k->fields(), k->constants(), k->java_fields_count(), 0) { }
};

class AllFieldStream : public FieldStreamBase {
 public:
  AllFieldStream(Array<u2>* fields, const constantPoolHandle& constants): FieldStreamBase(fields, constants) { }
  AllFieldStream(InstanceKlass* k):      FieldStreamBase(k->fields(), k->constants()) { }
};

#endif
