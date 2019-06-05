#ifndef SHARE_VM_METAPROGRAMMING_ISINTEGRAL_HPP
#define SHARE_VM_METAPROGRAMMING_ISINTEGRAL_HPP

#include "metaprogramming/integralConstant.hpp"
#include "metaprogramming/isSigned.hpp"
#include "metaprogramming/removeCV.hpp"

#include <limits>

// This metafunction returns true iff the type T (irrespective of CV qualifiers)
// is an integral type. Note that this is false for enums.

template<typename T>
struct IsIntegral
  : public IntegralConstant<bool, std::numeric_limits<typename RemoveCV<T>::type>::is_integer>
{ };

// This metafunction returns true iff the type T (irrespective of CV qualifiers)
// is a signed integral type. Note that this is false for enums.

template<typename T>
struct IsSignedIntegral
  : public IntegralConstant<bool, IsIntegral<T>::value && IsSigned<T>::value>
{ };

// This metafunction returns true iff the type T (irrespective of CV qualifiers)
// is an unsigned integral type. Note that this is false for enums.

template<typename T>
struct IsUnsignedIntegral
  : public IntegralConstant<bool, IsIntegral<T>::value && !IsSigned<T>::value>
{ };

#endif
