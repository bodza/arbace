#ifndef SHARE_VM_METAPROGRAMMING_ENABLEIF_HPP
#define SHARE_VM_METAPROGRAMMING_ENABLEIF_HPP

#include "memory/allocation.hpp"

// This metaprogramming tool allows explicitly enabling and disabling overloads
// of member functions depending on whether the condition B holds true.
// For example typename EnableIf<IsPointer<T>::value>::type func(T ptr) would
// only become an overload the compiler chooses from if the type T is a pointer.
// If it is not, then the template definition is not expanded and there will be
// no compiler error if there is another overload of func that is selected when
// T is not a pointer. Like for example
// typename EnableIf<!IsPointer<T>::value>::type func(T not_ptr)

template <bool B, typename T = void>
struct EnableIf: AllStatic {};

template <typename T>
struct EnableIf<true, T>: AllStatic {
  typedef T type;
};

#endif
