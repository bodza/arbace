#ifndef SHARE_VM_CLASSFILE_JAVACLASSES_INLINE_HPP
#define SHARE_VM_CLASSFILE_JAVACLASSES_INLINE_HPP

#include "classfile/javaClasses.hpp"
#include "oops/access.inline.hpp"
#include "oops/oop.inline.hpp"
#include "oops/oopsHierarchy.hpp"

void java_lang_String::set_coder(oop string, jbyte coder) {
  string->byte_field_put(coder_offset, coder);
}

void java_lang_String::set_value_raw(oop string, typeArrayOop buffer) {
  string->obj_field_put_raw(value_offset, buffer);
}
void java_lang_String::set_value(oop string, typeArrayOop buffer) {
  string->obj_field_put(value_offset, (oop)buffer);
}
void java_lang_String::set_hash(oop string, unsigned int hash) {
  string->int_field_put(hash_offset, hash);
}

// Accessors
typeArrayOop java_lang_String::value(oop java_string) {
  return (typeArrayOop) java_string->obj_field(value_offset);
}
typeArrayOop java_lang_String::value_no_keepalive(oop java_string) {
  return (typeArrayOop) java_string->obj_field_access<AS_NO_KEEPALIVE>(value_offset);
}
unsigned int java_lang_String::hash(oop java_string) {
  return java_string->int_field(hash_offset);
}
bool java_lang_String::is_latin1(oop java_string) {
  jbyte coder = java_string->byte_field(coder_offset);
  return coder == CODER_LATIN1;
}
int java_lang_String::length(oop java_string) {
  typeArrayOop value = java_lang_String::value_no_keepalive(java_string);
  if (value == NULL) {
    return 0;
  }
  int arr_length = value->length();
  if (!is_latin1(java_string)) {
    arr_length >>= 1; // convert number of bytes to number of elements
  }
  return arr_length;
}

bool java_lang_String::is_instance_inlined(oop obj) {
  return obj != NULL && obj->klass() == SystemDictionary::String_klass();
}

// Accessors
oop java_lang_ref_Reference::referent(oop ref) {
  return ref->obj_field(referent_offset);
}
void java_lang_ref_Reference::set_referent(oop ref, oop value) {
  ref->obj_field_put(referent_offset, value);
}
void java_lang_ref_Reference::set_referent_raw(oop ref, oop value) {
  ref->obj_field_put_raw(referent_offset, value);
}
HeapWord* java_lang_ref_Reference::referent_addr_raw(oop ref) {
  return ref->obj_field_addr_raw<HeapWord>(referent_offset);
}
oop java_lang_ref_Reference::next(oop ref) {
  return ref->obj_field(next_offset);
}
void java_lang_ref_Reference::set_next(oop ref, oop value) {
  ref->obj_field_put(next_offset, value);
}
void java_lang_ref_Reference::set_next_raw(oop ref, oop value) {
  ref->obj_field_put_raw(next_offset, value);
}
HeapWord* java_lang_ref_Reference::next_addr_raw(oop ref) {
  return ref->obj_field_addr_raw<HeapWord>(next_offset);
}
oop java_lang_ref_Reference::discovered(oop ref) {
  return ref->obj_field(discovered_offset);
}
void java_lang_ref_Reference::set_discovered(oop ref, oop value) {
  ref->obj_field_put(discovered_offset, value);
}
void java_lang_ref_Reference::set_discovered_raw(oop ref, oop value) {
  ref->obj_field_put_raw(discovered_offset, value);
}
HeapWord* java_lang_ref_Reference::discovered_addr_raw(oop ref) {
  return ref->obj_field_addr_raw<HeapWord>(discovered_offset);
}
bool java_lang_ref_Reference::is_phantom(oop ref) {
  return InstanceKlass::cast(ref->klass())->reference_type() == REF_PHANTOM;
}

inline bool java_lang_Class::is_instance(oop obj)  { return obj != NULL && obj->klass() == SystemDictionary::Class_klass(); }
inline bool java_lang_Module::is_instance(oop obj) { return obj != NULL && obj->klass() == SystemDictionary::Module_klass(); }

inline int Backtrace::merge_bci_and_version(int bci, int version) {
  // only store u2 for version, checking for overflow.
  if (version > USHRT_MAX || version < 0) version = USHRT_MAX;
  return build_int_from_shorts(version, bci);
}

inline int Backtrace::merge_mid_and_cpref(int mid, int cpref) {
  // only store u2 for mid and cpref, checking for overflow.
  return build_int_from_shorts(cpref, mid);
}

inline int Backtrace::bci_at(unsigned int merged) {
  return extract_high_short_from_int(merged);
}

inline int Backtrace::version_at(unsigned int merged) {
  return extract_low_short_from_int(merged);
}

inline int Backtrace::mid_at(unsigned int merged) {
  return extract_high_short_from_int(merged);
}

inline int Backtrace::cpref_at(unsigned int merged) {
  return extract_low_short_from_int(merged);
}

inline int Backtrace::get_line_number(const methodHandle& method, int bci) {
  int line_number = 0;
  if (method->is_native()) {
    // Negative value different from -1 below, enabling Java code in
    // class java.lang.StackTraceElement to distinguish "native" from
    // "no LineNumberTable".  JDK tests for -2.
    line_number = -2;
  } else {
    // Returns -1 if no LineNumberTable, and otherwise actual line number
    line_number = method->line_number_from_bci(bci);
    if (line_number == -1 && ShowHiddenFrames) {
      line_number = bci + 1000000;
    }
  }
  return line_number;
}

inline Symbol* Backtrace::get_source_file_name(InstanceKlass* holder, int version) {
  // RedefineClasses() currently permits redefine operations to
  // happen in parallel using a "last one wins" philosophy. That
  // spec laxness allows the constant pool entry associated with
  // the source_file_name_index for any older constant pool version
  // to be unstable so we shouldn't try to use it.
  if (holder->constants()->version() != version) {
    return NULL;
  } else {
    return holder->source_file_name();
  }
}

#endif
