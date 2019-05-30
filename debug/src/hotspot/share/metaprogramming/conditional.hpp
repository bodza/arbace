#ifndef SHARE_VM_METAPROGRAMMING_CONDITIONAL_HPP
#define SHARE_VM_METAPROGRAMMING_CONDITIONAL_HPP

#include "memory/allocation.hpp"

// This trait evaluates its typedef called "type" to TrueType iff the condition
// is true. Otherwise it evaluates to FalseType.

template <bool condition, typename TrueType, typename FalseType>
struct Conditional: AllStatic {
  typedef TrueType type;
};

template <typename TrueType, typename FalseType>
struct Conditional<false, TrueType, FalseType>: AllStatic {
  typedef FalseType type;
};

#endif
