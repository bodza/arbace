#include "precompiled.hpp"

#include "oops/access.inline.hpp"
#include "oops/objArrayKlass.hpp"
#include "oops/objArrayOop.inline.hpp"
#include "oops/oop.inline.hpp"

oop objArrayOopDesc::atomic_compare_exchange_oop(int index, oop exchange_value,
                                                 oop compare_value) {
  ptrdiff_t offs;
  if (UseCompressedOops) {
    offs = objArrayOopDesc::obj_at_offset<narrowOop>(index);
  } else {
    offs = objArrayOopDesc::obj_at_offset<oop>(index);
  }
  return HeapAccess<IS_ARRAY>::oop_atomic_cmpxchg_at(exchange_value, as_oop(), offs, compare_value);
}

Klass* objArrayOopDesc::element_klass() {
  return ObjArrayKlass::cast(klass())->element_klass();
}
