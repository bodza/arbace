#ifndef SHARE_VM_OOPS_INSTANCEKLASS_INLINE_HPP
#define SHARE_VM_OOPS_INSTANCEKLASS_INLINE_HPP

#include "memory/iterator.hpp"
#include "oops/instanceKlass.hpp"
#include "oops/klass.hpp"
#include "oops/oop.inline.hpp"
#include "runtime/orderAccess.hpp"
#include "utilities/debug.hpp"
#include "utilities/globalDefinitions.hpp"
#include "utilities/macros.hpp"

inline Klass* InstanceKlass::array_klasses_acquire() const {
  return OrderAccess::load_acquire(&_array_klasses);
}

inline void InstanceKlass::release_set_array_klasses(Klass* k) {
  OrderAccess::release_store(&_array_klasses, k);
}

inline jmethodID* InstanceKlass::methods_jmethod_ids_acquire() const {
  return OrderAccess::load_acquire(&_methods_jmethod_ids);
}

inline void InstanceKlass::release_set_methods_jmethod_ids(jmethodID* jmeths) {
  OrderAccess::release_store(&_methods_jmethod_ids, jmeths);
}

// The iteration over the oops in objects is a hot path in the GC code.
// By force inlining the following functions, we get similar GC performance
// as the previous macro based implementation.

template <typename T, class OopClosureType>
ALWAYSINLINE void InstanceKlass::oop_oop_iterate_oop_map(OopMapBlock* map, oop obj, OopClosureType* closure) {
  T* p         = (T*)obj->obj_field_addr_raw<T>(map->offset());
  T* const end = p + map->count();

  for ( ; p < end; ++p) {
    Devirtualizer::do_oop(closure, p);
  }
}

template <typename T, class OopClosureType>
ALWAYSINLINE void InstanceKlass::oop_oop_iterate_oop_map_reverse(OopMapBlock* map, oop obj, OopClosureType* closure) {
  T* const start = (T*)obj->obj_field_addr_raw<T>(map->offset());
  T*       p     = start + map->count();

  while (start < p) {
    --p;
    Devirtualizer::do_oop(closure, p);
  }
}

template <typename T, class OopClosureType>
ALWAYSINLINE void InstanceKlass::oop_oop_iterate_oop_map_bounded(OopMapBlock* map, oop obj, OopClosureType* closure, MemRegion mr) {
  T* p   = (T*)obj->obj_field_addr_raw<T>(map->offset());
  T* end = p + map->count();

  T* const l   = (T*)mr.start();
  T* const h   = (T*)mr.end();

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

template <typename T, class OopClosureType>
ALWAYSINLINE void InstanceKlass::oop_oop_iterate_oop_maps(oop obj, OopClosureType* closure) {
  OopMapBlock* map           = start_of_nonstatic_oop_maps();
  OopMapBlock* const end_map = map + nonstatic_oop_map_count();

  for ( ; map < end_map; ++map) {
    oop_oop_iterate_oop_map<T>(map, obj, closure);
  }
}

template <typename T, class OopClosureType>
ALWAYSINLINE void InstanceKlass::oop_oop_iterate_oop_maps_reverse(oop obj, OopClosureType* closure) {
  OopMapBlock* const start_map = start_of_nonstatic_oop_maps();
  OopMapBlock* map             = start_map + nonstatic_oop_map_count();

  while (start_map < map) {
    --map;
    oop_oop_iterate_oop_map_reverse<T>(map, obj, closure);
  }
}

template <typename T, class OopClosureType>
ALWAYSINLINE void InstanceKlass::oop_oop_iterate_oop_maps_bounded(oop obj, OopClosureType* closure, MemRegion mr) {
  OopMapBlock* map           = start_of_nonstatic_oop_maps();
  OopMapBlock* const end_map = map + nonstatic_oop_map_count();

  for (;map < end_map; ++map) {
    oop_oop_iterate_oop_map_bounded<T>(map, obj, closure, mr);
  }
}

template <typename T, class OopClosureType>
ALWAYSINLINE int InstanceKlass::oop_oop_iterate(oop obj, OopClosureType* closure) {
  if (Devirtualizer::do_metadata(closure)) {
    Devirtualizer::do_klass(closure, this);
  }

  oop_oop_iterate_oop_maps<T>(obj, closure);

  return size_helper();
}

template <typename T, class OopClosureType>
ALWAYSINLINE int InstanceKlass::oop_oop_iterate_reverse(oop obj, OopClosureType* closure) {
  oop_oop_iterate_oop_maps_reverse<T>(obj, closure);

  return size_helper();
}

template <typename T, class OopClosureType>
ALWAYSINLINE int InstanceKlass::oop_oop_iterate_bounded(oop obj, OopClosureType* closure, MemRegion mr) {
  if (Devirtualizer::do_metadata(closure)) {
    if (mr.contains(obj)) {
      Devirtualizer::do_klass(closure, this);
    }
  }

  oop_oop_iterate_oop_maps_bounded<T>(obj, closure, mr);

  return size_helper();
}

#endif
