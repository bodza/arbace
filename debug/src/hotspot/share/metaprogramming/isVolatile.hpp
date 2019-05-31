#ifndef SHARE_VM_METAPROGRAMMING_ISVOLATILE_HPP
#define SHARE_VM_METAPROGRAMMING_ISVOLATILE_HPP

#include "metaprogramming/integralConstant.hpp"

template <typename T> struct IsVolatile: public FalseType { };
template <typename T> struct IsVolatile<volatile T>: public TrueType { };

#endif
