#ifndef SHARE_VM_OOPS_INSTANCEREFKLASS_INLINE_HPP
#define SHARE_VM_OOPS_INSTANCEREFKLASS_INLINE_HPP

#include "classfile/javaClasses.inline.hpp"
#include "gc/shared/referenceProcessor.hpp"
#include "logging/log.hpp"
#include "oops/access.inline.hpp"
#include "oops/compressedOops.inline.hpp"
#include "oops/instanceKlass.inline.hpp"
#include "oops/instanceRefKlass.hpp"
#include "oops/oop.inline.hpp"
#include "utilities/debug.hpp"
#include "utilities/globalDefinitions.hpp"
#include "utilities/macros.hpp"

template <typename T, class OopClosureType, class Contains>
void InstanceRefKlass::do_referent(oop obj, OopClosureType* closure, Contains& contains) {
  T* referent_addr = (T*)java_lang_ref_Reference::referent_addr_raw(obj);
  if (contains(referent_addr)) {
    Devirtualizer::do_oop(closure, referent_addr);
  }
}

template <typename T, class OopClosureType, class Contains>
void InstanceRefKlass::do_discovered(oop obj, OopClosureType* closure, Contains& contains) {
  T* discovered_addr = (T*)java_lang_ref_Reference::discovered_addr_raw(obj);
  if (contains(discovered_addr)) {
    Devirtualizer::do_oop(closure, discovered_addr);
  }
}

static inline oop load_referent(oop obj, ReferenceType type) {
  if (type == REF_PHANTOM) {
    return HeapAccess<ON_PHANTOM_OOP_REF | AS_NO_KEEPALIVE>::oop_load(java_lang_ref_Reference::referent_addr_raw(obj));
  } else {
    return HeapAccess<ON_WEAK_OOP_REF | AS_NO_KEEPALIVE>::oop_load(java_lang_ref_Reference::referent_addr_raw(obj));
  }
}

template <typename T, class OopClosureType>
bool InstanceRefKlass::try_discover(oop obj, ReferenceType type, OopClosureType* closure) {
  ReferenceDiscoverer* rd = closure->ref_discoverer();
  if (rd != NULL) {
    oop referent = load_referent(obj, type);
    if (referent != NULL) {
      if (!referent->is_gc_marked()) {
        // Only try to discover if not yet marked.
        return rd->discover_reference(obj, type);
      }
    }
  }
  return false;
}

template <typename T, class OopClosureType, class Contains>
void InstanceRefKlass::oop_oop_iterate_discovery(oop obj, ReferenceType type, OopClosureType* closure, Contains& contains) {
  // Try to discover reference and return if it succeeds.
  if (try_discover<T>(obj, type, closure)) {
    return;
  }

  // Treat referent and discovered as normal oops.
  do_referent<T>(obj, closure, contains);
  do_discovered<T>(obj, closure, contains);
}

template <typename T, class OopClosureType, class Contains>
void InstanceRefKlass::oop_oop_iterate_discovered_and_discovery(oop obj, ReferenceType type, OopClosureType* closure, Contains& contains) {
  // Explicitly apply closure to the discovered field.
  do_discovered<T>(obj, closure, contains);
  // Then do normal reference processing with discovery.
  oop_oop_iterate_discovery<T>(obj, type, closure, contains);
}

template <typename T, class OopClosureType, class Contains>
void InstanceRefKlass::oop_oop_iterate_fields(oop obj, OopClosureType* closure, Contains& contains) {
  do_referent<T>(obj, closure, contains);
  do_discovered<T>(obj, closure, contains);
}

template <typename T, class OopClosureType, class Contains>
void InstanceRefKlass::oop_oop_iterate_fields_except_referent(oop obj, OopClosureType* closure, Contains& contains) {
  do_discovered<T>(obj, closure, contains);
}

template <typename T, class OopClosureType, class Contains>
void InstanceRefKlass::oop_oop_iterate_ref_processing(oop obj, OopClosureType* closure, Contains& contains) {
  switch (closure->reference_iteration_mode()) {
    case OopIterateClosure::DO_DISCOVERY:
      trace_reference_gc<T>("do_discovery", obj);
      oop_oop_iterate_discovery<T>(obj, reference_type(), closure, contains);
      break;
    case OopIterateClosure::DO_DISCOVERED_AND_DISCOVERY:
      trace_reference_gc<T>("do_discovered_and_discovery", obj);
      oop_oop_iterate_discovered_and_discovery<T>(obj, reference_type(), closure, contains);
      break;
    case OopIterateClosure::DO_FIELDS:
      trace_reference_gc<T>("do_fields", obj);
      oop_oop_iterate_fields<T>(obj, closure, contains);
      break;
    case OopIterateClosure::DO_FIELDS_EXCEPT_REFERENT:
      trace_reference_gc<T>("do_fields_except_referent", obj);
      oop_oop_iterate_fields_except_referent<T>(obj, closure, contains);
      break;
    default:
      ShouldNotReachHere();
  }
}

class AlwaysContains {
 public:
  template <typename T> bool operator()(T* p) const { return true; }
};

template <typename T, class OopClosureType>
void InstanceRefKlass::oop_oop_iterate_ref_processing(oop obj, OopClosureType* closure) {
  AlwaysContains always_contains;
  oop_oop_iterate_ref_processing<T>(obj, closure, always_contains);
}

class MrContains {
  const MemRegion _mr;
 public:
  MrContains(MemRegion mr) : _mr(mr) {}
  template <typename T> bool operator()(T* p) const { return _mr.contains(p); }
};

template <typename T, class OopClosureType>
void InstanceRefKlass::oop_oop_iterate_ref_processing_bounded(oop obj, OopClosureType* closure, MemRegion mr) {
  const MrContains contains(mr);
  oop_oop_iterate_ref_processing<T>(obj, closure, contains);
}

template <typename T, class OopClosureType>
void InstanceRefKlass::oop_oop_iterate(oop obj, OopClosureType* closure) {
  InstanceKlass::oop_oop_iterate<T>(obj, closure);

  oop_oop_iterate_ref_processing<T>(obj, closure);
}

template <typename T, class OopClosureType>
void InstanceRefKlass::oop_oop_iterate_reverse(oop obj, OopClosureType* closure) {
  InstanceKlass::oop_oop_iterate_reverse<T>(obj, closure);

  oop_oop_iterate_ref_processing<T>(obj, closure);
}

template <typename T, class OopClosureType>
void InstanceRefKlass::oop_oop_iterate_bounded(oop obj, OopClosureType* closure, MemRegion mr) {
  InstanceKlass::oop_oop_iterate_bounded<T>(obj, closure, mr);

  oop_oop_iterate_ref_processing_bounded<T>(obj, closure, mr);
}

#endif
