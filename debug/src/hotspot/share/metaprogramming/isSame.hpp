#ifndef SHARE_VM_METAPROGRAMMING_ISSAME_HPP
#define SHARE_VM_METAPROGRAMMING_ISSAME_HPP

#include "metaprogramming/integralConstant.hpp"

// This trait returns true iff the two types X and Y are the same

template <typename X, typename Y>
struct IsSame: public FalseType { };

template <typename X>
struct IsSame<X, X>: public TrueType { };

#endif
