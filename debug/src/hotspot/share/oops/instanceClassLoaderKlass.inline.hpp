#ifndef SHARE_VM_OOPS_INSTANCECLASSLOADERKLASS_INLINE_HPP
#define SHARE_VM_OOPS_INSTANCECLASSLOADERKLASS_INLINE_HPP

#include "classfile/javaClasses.hpp"
#include "memory/iterator.hpp"
#include "oops/instanceClassLoaderKlass.hpp"
#include "oops/instanceKlass.inline.hpp"
#include "oops/oop.inline.hpp"
#include "utilities/debug.hpp"
#include "utilities/globalDefinitions.hpp"
#include "utilities/macros.hpp"

template <typename T, class OopClosureType>
inline void InstanceClassLoaderKlass::oop_oop_iterate(oop obj, OopClosureType* closure) {
  InstanceKlass::oop_oop_iterate<T>(obj, closure);

  if (Devirtualizer::do_metadata(closure)) {
    ClassLoaderData* cld = java_lang_ClassLoader::loader_data(obj);
    // cld can be null if we have a non-registered class loader.
    if (cld != NULL) {
      Devirtualizer::do_cld(closure, cld);
    }
  }
}

template <typename T, class OopClosureType>
inline void InstanceClassLoaderKlass::oop_oop_iterate_reverse(oop obj, OopClosureType* closure) {
  InstanceKlass::oop_oop_iterate_reverse<T>(obj, closure);

}

template <typename T, class OopClosureType>
inline void InstanceClassLoaderKlass::oop_oop_iterate_bounded(oop obj, OopClosureType* closure, MemRegion mr) {
  InstanceKlass::oop_oop_iterate_bounded<T>(obj, closure, mr);

  if (Devirtualizer::do_metadata(closure)) {
    if (mr.contains(obj)) {
      ClassLoaderData* cld = java_lang_ClassLoader::loader_data(obj);
      // cld can be null if we have a non-registered class loader.
      if (cld != NULL) {
        Devirtualizer::do_cld(closure, cld);
      }
    }
  }
}

#endif
