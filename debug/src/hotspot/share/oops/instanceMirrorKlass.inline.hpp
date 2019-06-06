#ifndef SHARE_VM_OOPS_INSTANCEMIRRORKLASS_INLINE_HPP
#define SHARE_VM_OOPS_INSTANCEMIRRORKLASS_INLINE_HPP

#include "classfile/javaClasses.hpp"
#include "oops/instanceKlass.inline.hpp"
#include "oops/instanceMirrorKlass.hpp"
#include "oops/klass.hpp"
#include "oops/oop.inline.hpp"
#include "utilities/debug.hpp"
#include "utilities/globalDefinitions.hpp"
#include "utilities/macros.hpp"

template <typename T, class OopClosureType>
void InstanceMirrorKlass::oop_oop_iterate_statics(oop obj, OopClosureType* closure) {
  T* p         = (T*)start_of_static_fields(obj);
  T* const end = p + java_lang_Class::static_oop_field_count(obj);

  for ( ; p < end; ++p) {
    Devirtualizer::do_oop(closure, p);
  }
}

template <typename T, class OopClosureType>
void InstanceMirrorKlass::oop_oop_iterate(oop obj, OopClosureType* closure) {
  InstanceKlass::oop_oop_iterate<T>(obj, closure);

  if (Devirtualizer::do_metadata(closure)) {
    Klass* klass = java_lang_Class::as_Klass(obj);
    // We'll get NULL for primitive mirrors.
    if (klass != NULL) {
      if (klass->is_instance_klass() && InstanceKlass::cast(klass)->is_anonymous()) {
        // An anonymous class doesn't have its own class loader, so when handling
        // the java mirror for an anonymous class we need to make sure its class
        // loader data is claimed, this is done by calling do_cld explicitly.
        // For non-anonymous classes the call to do_cld is made when the class
        // loader itself is handled.
        Devirtualizer::do_cld(closure, klass->class_loader_data());
      } else {
        Devirtualizer::do_klass(closure, klass);
      }
    }
  }

  oop_oop_iterate_statics<T>(obj, closure);
}

template <typename T, class OopClosureType>
void InstanceMirrorKlass::oop_oop_iterate_reverse(oop obj, OopClosureType* closure) {
  InstanceKlass::oop_oop_iterate_reverse<T>(obj, closure);

  InstanceMirrorKlass::oop_oop_iterate_statics<T>(obj, closure);
}

template <typename T, class OopClosureType>
void InstanceMirrorKlass::oop_oop_iterate_statics_bounded(oop obj, OopClosureType* closure, MemRegion mr) {
  T* p   = (T*)start_of_static_fields(obj);
  T* end = p + java_lang_Class::static_oop_field_count(obj);

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
void InstanceMirrorKlass::oop_oop_iterate_bounded(oop obj, OopClosureType* closure, MemRegion mr) {
  InstanceKlass::oop_oop_iterate_bounded<T>(obj, closure, mr);

  if (Devirtualizer::do_metadata(closure)) {
    if (mr.contains(obj)) {
      Klass* klass = java_lang_Class::as_Klass(obj);
      // We'll get NULL for primitive mirrors.
      if (klass != NULL) {
        Devirtualizer::do_klass(closure, klass);
      }
    }
  }

  oop_oop_iterate_statics_bounded<T>(obj, closure, mr);
}

#endif
