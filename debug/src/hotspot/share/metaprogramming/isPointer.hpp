#ifndef SHARE_VM_METAPROGRAMMING_ISPOINTER_HPP
#define SHARE_VM_METAPROGRAMMING_ISPOINTER_HPP

#include "metaprogramming/integralConstant.hpp"

// This metafunction returns true iff the type T is (irrespective of CV qualifiers)
// a pointer type.

template <typename T> class IsPointer: public FalseType {};

template <typename T> class IsPointer<T*>: public TrueType {};
template <typename T> class IsPointer<T* const>: public TrueType {};
template <typename T> class IsPointer<T* volatile>: public TrueType {};
template <typename T> class IsPointer<T* const volatile>: public TrueType {};

#endif
