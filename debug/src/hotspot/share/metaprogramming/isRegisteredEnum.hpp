#ifndef SHARE_VM_METAPROGRAMMING_ISREGISTEREDENUM_HPP
#define SHARE_VM_METAPROGRAMMING_ISREGISTEREDENUM_HPP

#include "metaprogramming/integralConstant.hpp"

// Recognize registered enum types.
// Registration is by specializing this trait.
//
// This is a manual stand-in for the C++11 std::is_enum<T> type trait.
// It's a lot of work to implement is_enum portably in C++98, so this
// manual approach is being taken for those enum types we need to
// distinguish.
template<typename T>
struct IsRegisteredEnum : public FalseType {};

#endif
