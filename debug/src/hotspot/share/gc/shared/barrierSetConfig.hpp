#ifndef SHARE_VM_GC_SHARED_BARRIERSETCONFIG_HPP
#define SHARE_VM_GC_SHARED_BARRIERSETCONFIG_HPP

#include "utilities/macros.hpp"

// Do something for each concrete barrier set part of the build.
#define FOR_EACH_CONCRETE_BARRIER_SET_DO(f) \
  f(CardTableBarrierSet) \
  f(EpsilonBarrierSet) \
  f(G1BarrierSet)

#define FOR_EACH_ABSTRACT_BARRIER_SET_DO(f) \
  f(ModRef)

// Do something for each known barrier set.
#define FOR_EACH_BARRIER_SET_DO(f) \
  FOR_EACH_ABSTRACT_BARRIER_SET_DO(f) \
  FOR_EACH_CONCRETE_BARRIER_SET_DO(f)

// To enable runtime-resolution of GC barriers on primitives, please
// define SUPPORT_BARRIER_ON_PRIMITIVES.
#ifdef SUPPORT_BARRIER_ON_PRIMITIVES
#define ACCESS_PRIMITIVE_SUPPORT INTERNAL_BT_BARRIER_ON_PRIMITIVES
#else
#define ACCESS_PRIMITIVE_SUPPORT INTERNAL_EMPTY
#endif

#ifdef SUPPORT_NOT_TO_SPACE_INVARIANT
#define ACCESS_TO_SPACE_INVARIANT_SUPPORT INTERNAL_EMPTY
#else
#define ACCESS_TO_SPACE_INVARIANT_SUPPORT INTERNAL_BT_TO_SPACE_INVARIANT
#endif

#define BT_BUILDTIME_DECORATORS (ACCESS_PRIMITIVE_SUPPORT | ACCESS_TO_SPACE_INVARIANT_SUPPORT)

#endif
