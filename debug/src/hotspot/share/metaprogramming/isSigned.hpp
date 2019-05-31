#ifndef SHARE_VM_METAPROGRAMMING_ISSIGNED_HPP
#define SHARE_VM_METAPROGRAMMING_ISSIGNED_HPP

#include "metaprogramming/integralConstant.hpp"
#include "metaprogramming/removeCV.hpp"
#include <limits>

template<typename T>
struct IsSigned
  : public IntegralConstant<bool, std::numeric_limits<typename RemoveCV<T>::type>::is_signed>
{ };

#endif
