#ifndef SHARE_VM_OOPS_TYPEARRAYKLASS_INLINE_HPP
#define SHARE_VM_OOPS_TYPEARRAYKLASS_INLINE_HPP

#include "oops/arrayKlass.hpp"
#include "oops/klass.hpp"
#include "oops/oop.inline.hpp"
#include "oops/typeArrayKlass.hpp"
#include "oops/typeArrayOop.hpp"

class OopIterateClosure;

inline void TypeArrayKlass::oop_oop_iterate_impl(oop obj, OopIterateClosure* closure) {
  // Performance tweak: We skip processing the klass pointer since all
  // TypeArrayKlasses are guaranteed processed via the null class loader.
}

template <typename T, typename OopClosureType>
void TypeArrayKlass::oop_oop_iterate(oop obj, OopClosureType* closure) {
  oop_oop_iterate_impl(obj, closure);
}

template <typename T, typename OopClosureType>
void TypeArrayKlass::oop_oop_iterate_bounded(oop obj, OopClosureType* closure, MemRegion mr) {
  oop_oop_iterate_impl(obj, closure);
}

template <typename T, typename OopClosureType>
void TypeArrayKlass::oop_oop_iterate_reverse(oop obj, OopClosureType* closure) {
  oop_oop_iterate_impl(obj, closure);
}

#endif
