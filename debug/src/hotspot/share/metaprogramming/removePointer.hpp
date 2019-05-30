#ifndef SHARE_VM_METAPROGRAMMING_REMOVEPOINTER_HPP
#define SHARE_VM_METAPROGRAMMING_REMOVEPOINTER_HPP

#include "memory/allocation.hpp"

// This metafunction returns for a type T either the underlying type behind
// the pointer iff T is a pointer type (irrespective of CV qualifiers),
// or the same type T if T is not a pointer type.

template <typename T> struct RemovePointer: AllStatic { typedef T type; };

template <typename T> struct RemovePointer<T*>: AllStatic { typedef T type; };
template <typename T> struct RemovePointer<T* const>: AllStatic { typedef T type; };
template <typename T> struct RemovePointer<T* volatile>: AllStatic { typedef T type; };
template <typename T> struct RemovePointer<T* const volatile>: AllStatic { typedef T type; };

#endif
