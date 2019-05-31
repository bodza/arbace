#ifndef SHARE_VM_OOPS_KLASS_INLINE_HPP
#define SHARE_VM_OOPS_KLASS_INLINE_HPP

#include "memory/universe.hpp"
#include "oops/klass.hpp"
#include "oops/markOop.hpp"

inline void Klass::set_prototype_header(markOop header) {
  _prototype_header = header;
}

inline bool Klass::is_null(Klass* obj) { return obj == NULL; }
inline bool Klass::is_null(narrowKlass obj) { return obj == 0; }

// Encoding and decoding for klass field.

inline bool check_klass_alignment(Klass* obj) {
  return (intptr_t)obj % KlassAlignmentInBytes == 0;
}

inline narrowKlass Klass::encode_klass_not_null(Klass* v) {
  int    shift = Universe::narrow_klass_shift();
  uint64_t pd = (uint64_t)(pointer_delta((void*)v, Universe::narrow_klass_base(), 1));
  uint64_t result = pd >> shift;
  return (narrowKlass)result;
}

inline narrowKlass Klass::encode_klass(Klass* v) {
  return is_null(v) ? (narrowKlass)0 : encode_klass_not_null(v);
}

inline Klass* Klass::decode_klass_not_null(narrowKlass v) {
  int    shift = Universe::narrow_klass_shift();
  Klass* result = (Klass*)(void*)((uintptr_t)Universe::narrow_klass_base() + ((uintptr_t)v << shift));
  return result;
}

inline Klass* Klass::decode_klass(narrowKlass v) {
  return is_null(v) ? (Klass*)NULL : decode_klass_not_null(v);
}

#endif
