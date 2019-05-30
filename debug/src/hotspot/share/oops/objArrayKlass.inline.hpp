#ifndef SHARE_VM_OOPS_OBJARRAYKLASS_INLINE_HPP
#define SHARE_VM_OOPS_OBJARRAYKLASS_INLINE_HPP

#include "memory/memRegion.hpp"
#include "memory/iterator.hpp"
#include "oops/arrayOop.inline.hpp"
#include "oops/arrayKlass.hpp"
#include "oops/klass.hpp"
#include "oops/objArrayKlass.hpp"
#include "oops/objArrayOop.inline.hpp"
#include "oops/oop.inline.hpp"
#include "utilities/macros.hpp"

template <typename T, class OopClosureType>
void ObjArrayKlass::oop_oop_iterate_elements(objArrayOop a, OopClosureType* closure) {
  T* p         = (T*)a->base_raw();
  T* const end = p + a->length();

  for (;p < end; p++) {
    Devirtualizer::do_oop(closure, p);
  }
}

template <typename T, class OopClosureType>
void ObjArrayKlass::oop_oop_iterate_elements_bounded(
    objArrayOop a, OopClosureType* closure, void* low, void* high) {

  T* const l = (T*)low;
  T* const h = (T*)high;

  T* p   = (T*)a->base_raw();
  T* end = p + a->length();

  if (p < l) {
    p = l;
  }
  if (end > h) {
    end = h;
  }

  for (;p < end; ++p) {
    Devirtualizer::do_oop(closure, p);
  }
}

template <typename T, typename OopClosureType>
void ObjArrayKlass::oop_oop_iterate(oop obj, OopClosureType* closure) {
  assert(obj->is_array(), "obj must be array");
  objArrayOop a = objArrayOop(obj);

  if (Devirtualizer::do_metadata(closure)) {
    Devirtualizer::do_klass(closure, obj->klass());
  }

  oop_oop_iterate_elements<T>(a, closure);
}

template <typename T, typename OopClosureType>
void ObjArrayKlass::oop_oop_iterate_reverse(oop obj, OopClosureType* closure) {
  // No reverse implementation ATM.
  oop_oop_iterate<T>(obj, closure);
}

template <typename T, typename OopClosureType>
void ObjArrayKlass::oop_oop_iterate_bounded(oop obj, OopClosureType* closure, MemRegion mr) {
  assert(obj->is_array(), "obj must be array");
  objArrayOop a  = objArrayOop(obj);

  if (Devirtualizer::do_metadata(closure)) {
    Devirtualizer::do_klass(closure, a->klass());
  }

  oop_oop_iterate_elements_bounded<T>(a, closure, mr.start(), mr.end());
}

// Like oop_oop_iterate but only iterates over a specified range and only used
// for objArrayOops.
template <typename T, class OopClosureType>
void ObjArrayKlass::oop_oop_iterate_range(objArrayOop a, OopClosureType* closure, int start, int end) {
  T* low = start == 0 ? cast_from_oop<T*>(a) : a->obj_at_addr_raw<T>(start);
  T* high = (T*)a->base_raw() + end;

  oop_oop_iterate_elements_bounded<T>(a, closure, low, high);
}

// Placed here to resolve include cycle between objArrayKlass.inline.hpp and objArrayOop.inline.hpp
template <typename OopClosureType>
void objArrayOopDesc::oop_iterate_range(OopClosureType* blk, int start, int end) {
  if (UseCompressedOops) {
    ((ObjArrayKlass*)klass())->oop_oop_iterate_range<narrowOop>(this, blk, start, end);
  } else {
    ((ObjArrayKlass*)klass())->oop_oop_iterate_range<oop>(this, blk, start, end);
  }
}

#endif
