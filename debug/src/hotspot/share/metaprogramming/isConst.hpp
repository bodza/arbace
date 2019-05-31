#ifndef SHARE_VM_METAPROGRAMMING_ISCONST_HPP
#define SHARE_VM_METAPROGRAMMING_ISCONST_HPP

#include "metaprogramming/integralConstant.hpp"

template <typename T> struct IsConst: public FalseType { };
template <typename T> struct IsConst<const T>: public TrueType { };

#endif
