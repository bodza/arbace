#ifndef SHARE_VM_OOPS_TYPEARRAYOOP_INLINE_HPP
#define SHARE_VM_OOPS_TYPEARRAYOOP_INLINE_HPP

#include "oops/access.inline.hpp"
#include "oops/oop.inline.hpp"
#include "oops/arrayOop.inline.hpp"
#include "oops/typeArrayOop.hpp"

int typeArrayOopDesc::object_size() {
  TypeArrayKlass* tk = TypeArrayKlass::cast(klass());
  return object_size(tk->layout_helper(), length());
}

inline jchar* typeArrayOopDesc::char_base() const { return (jchar*) base(T_CHAR); }
inline jboolean* typeArrayOopDesc::bool_base() const { return (jboolean*)base(T_BOOLEAN); }
inline jbyte* typeArrayOopDesc::byte_base() const { return (jbyte*) base(T_BYTE); }
inline jint* typeArrayOopDesc::int_base() const { return (jint*) base(T_INT); }
inline jlong* typeArrayOopDesc::long_base() const { return (jlong*) base(T_LONG); }
inline jshort* typeArrayOopDesc::short_base() const { return (jshort*) base(T_SHORT); }
inline jfloat* typeArrayOopDesc::float_base() const { return (jfloat*) base(T_FLOAT); }
inline jdouble* typeArrayOopDesc::double_base() const { return (jdouble*) base(T_DOUBLE); }

inline jbyte* typeArrayOopDesc::byte_at_addr(int which) const {
  return &byte_base()[which];
}

inline jboolean* typeArrayOopDesc::bool_at_addr(int which) const {
  return &bool_base()[which];
}

inline jchar* typeArrayOopDesc::char_at_addr(int which) const {
  return &char_base()[which];
}

inline jint* typeArrayOopDesc::int_at_addr(int which) const {
  return &int_base()[which];
}

inline jshort* typeArrayOopDesc::short_at_addr(int which) const {
  return &short_base()[which];
}

inline jushort* typeArrayOopDesc::ushort_at_addr(int which) const {  // for field descriptor arrays
  return (jushort*) &short_base()[which];
}

inline jlong* typeArrayOopDesc::long_at_addr(int which) const {
  return &long_base()[which];
}

inline jfloat* typeArrayOopDesc::float_at_addr(int which) const {
  return &float_base()[which];
}

inline jdouble* typeArrayOopDesc::double_at_addr(int which) const {
  return &double_base()[which];
}

inline jbyte typeArrayOopDesc::byte_at(int which) const {
  ptrdiff_t offset = element_offset<jbyte>(which);
  return HeapAccess<IS_ARRAY>::load_at(as_oop(), offset);
}
inline void typeArrayOopDesc::byte_at_put(int which, jbyte contents) {
  ptrdiff_t offset = element_offset<jbyte>(which);
  HeapAccess<IS_ARRAY>::store_at(as_oop(), offset, contents);
}

inline jboolean typeArrayOopDesc::bool_at(int which) const {
  ptrdiff_t offset = element_offset<jboolean>(which);
  return HeapAccess<IS_ARRAY>::load_at(as_oop(), offset);
}
inline void typeArrayOopDesc::bool_at_put(int which, jboolean contents) {
  ptrdiff_t offset = element_offset<jboolean>(which);
  HeapAccess<IS_ARRAY>::store_at(as_oop(), offset, jboolean(contents & 1));
}

inline jchar typeArrayOopDesc::char_at(int which) const {
  ptrdiff_t offset = element_offset<jchar>(which);
  return HeapAccess<IS_ARRAY>::load_at(as_oop(), offset);
}
inline void typeArrayOopDesc::char_at_put(int which, jchar contents) {
  ptrdiff_t offset = element_offset<jchar>(which);
  HeapAccess<IS_ARRAY>::store_at(as_oop(), offset, contents);
}

inline jint typeArrayOopDesc::int_at(int which) const {
  ptrdiff_t offset = element_offset<jint>(which);
  return HeapAccess<IS_ARRAY>::load_at(as_oop(), offset);
}
inline void typeArrayOopDesc::int_at_put(int which, jint contents) {
  ptrdiff_t offset = element_offset<jint>(which);
  HeapAccess<IS_ARRAY>::store_at(as_oop(), offset, contents);
}

inline jshort typeArrayOopDesc::short_at(int which) const {
  ptrdiff_t offset = element_offset<jshort>(which);
  return HeapAccess<IS_ARRAY>::load_at(as_oop(), offset);
}
inline void typeArrayOopDesc::short_at_put(int which, jshort contents) {
  ptrdiff_t offset = element_offset<jshort>(which);
  HeapAccess<IS_ARRAY>::store_at(as_oop(), offset, contents);
}

inline jushort typeArrayOopDesc::ushort_at(int which) const {
  ptrdiff_t offset = element_offset<jushort>(which);
  return HeapAccess<IS_ARRAY>::load_at(as_oop(), offset);
}
inline void typeArrayOopDesc::ushort_at_put(int which, jushort contents) {
  ptrdiff_t offset = element_offset<jushort>(which);
  HeapAccess<IS_ARRAY>::store_at(as_oop(), offset, contents);
}

inline jlong typeArrayOopDesc::long_at(int which) const {
  ptrdiff_t offset = element_offset<jlong>(which);
  return HeapAccess<IS_ARRAY>::load_at(as_oop(), offset);
}
inline void typeArrayOopDesc::long_at_put(int which, jlong contents) {
  ptrdiff_t offset = element_offset<jlong>(which);
  HeapAccess<IS_ARRAY>::store_at(as_oop(), offset, contents);
}

inline jfloat typeArrayOopDesc::float_at(int which) const {
  ptrdiff_t offset = element_offset<jfloat>(which);
  return HeapAccess<IS_ARRAY>::load_at(as_oop(), offset);
}
inline void typeArrayOopDesc::float_at_put(int which, jfloat contents) {
  ptrdiff_t offset = element_offset<jfloat>(which);
  HeapAccess<IS_ARRAY>::store_at(as_oop(), offset, contents);
}

inline jdouble typeArrayOopDesc::double_at(int which) const {
  ptrdiff_t offset = element_offset<jdouble>(which);
  return HeapAccess<IS_ARRAY>::load_at(as_oop(), offset);
}
inline void typeArrayOopDesc::double_at_put(int which, jdouble contents) {
  ptrdiff_t offset = element_offset<jdouble>(which);
  HeapAccess<IS_ARRAY>::store_at(as_oop(), offset, contents);
}

inline jbyte typeArrayOopDesc::byte_at_acquire(int which) const {
  ptrdiff_t offset = element_offset<jbyte>(which);
  return HeapAccess<MO_ACQUIRE | IS_ARRAY>::load_at(as_oop(), offset);
}
inline void typeArrayOopDesc::release_byte_at_put(int which, jbyte contents) {
  ptrdiff_t offset = element_offset<jbyte>(which);
  HeapAccess<MO_RELEASE | IS_ARRAY>::store_at(as_oop(), offset, contents);
}

// Java thinks Symbol arrays are just arrays of either long or int, since
// there doesn't seem to be T_ADDRESS, so this is a bit of unfortunate
// casting
inline Symbol* typeArrayOopDesc::symbol_at(int which) const {
  ptrdiff_t offset = element_offset<jlong>(which);
  return (Symbol*)(jlong) HeapAccess<IS_ARRAY>::load_at(as_oop(), offset);
}
inline void typeArrayOopDesc::symbol_at_put(int which, Symbol* contents) {
  ptrdiff_t offset = element_offset<jlong>(which);
  HeapAccess<IS_ARRAY>::store_at(as_oop(), offset, (jlong)contents);
}

#endif
