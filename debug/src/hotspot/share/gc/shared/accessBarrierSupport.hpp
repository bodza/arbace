#ifndef SHARE_VM_GC_SHARED_ACCESSBARRIERSUPPORT_HPP
#define SHARE_VM_GC_SHARED_ACCESSBARRIERSUPPORT_HPP

#include "memory/allocation.hpp"
#include "oops/access.hpp"

class AccessBarrierSupport: AllStatic {
private:
  static DecoratorSet resolve_unknown_oop_ref_strength(DecoratorSet decorators, oop base, ptrdiff_t offset);

public:
  // Some collectors, like G1, needs to keep referents alive when loading them.
  // Therefore, for APIs that accept unknown oop ref strength (e.g. unsafe),
  // we need to dynamically find out if a given field is on a java.lang.ref.Reference object.
  // and in that case what strength it has.
  template<DecoratorSet decorators>
  static DecoratorSet resolve_possibly_unknown_oop_ref_strength(oop base, ptrdiff_t offset);
};

#endif
