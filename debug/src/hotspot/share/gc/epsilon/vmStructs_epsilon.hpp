#ifndef SHARE_GC_EPSILON_VMSTRUCTS_HPP
#define SHARE_GC_EPSILON_VMSTRUCTS_HPP

#include "gc/epsilon/epsilonHeap.hpp"
#include "gc/shared/space.hpp"
#include "memory/virtualspace.hpp"

#define VM_STRUCTS_EPSILONGC(nonstatic_field, volatile_nonstatic_field, static_field) \
  nonstatic_field(EpsilonHeap, _virtual_space, VirtualSpace) \
  nonstatic_field(EpsilonHeap, _space, ContiguousSpace*)

#define VM_TYPES_EPSILONGC(declare_type, declare_toplevel_type, declare_integer_type) \
  declare_type(EpsilonHeap, CollectedHeap)

#define VM_INT_CONSTANTS_EPSILONGC(declare_constant, declare_constant_with_value)

#endif
