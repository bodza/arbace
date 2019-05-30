#ifndef SHARE_VM_GC_SHARED_ACCESSBARRIERSUPPORT_INLINE_HPP
#define SHARE_VM_GC_SHARED_ACCESSBARRIERSUPPORT_INLINE_HPP

#include "gc/shared/accessBarrierSupport.hpp"

template <DecoratorSet decorators>
DecoratorSet AccessBarrierSupport::resolve_possibly_unknown_oop_ref_strength(oop base, ptrdiff_t offset) {
  if (!HasDecorator<decorators, ON_UNKNOWN_OOP_REF>::value) {
    return decorators;
  } else {
    return resolve_unknown_oop_ref_strength(decorators, base, offset);
  }
}

#endif
