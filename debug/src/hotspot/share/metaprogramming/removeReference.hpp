#ifndef SHARE_VM_METAPROGRAMMING_REMOVEREFERENCE_HPP
#define SHARE_VM_METAPROGRAMMING_REMOVEREFERENCE_HPP

#include "memory/allocation.hpp"

// This metafunction returns for a type T either the underlying type behind
// the reference iff T is a reference type, or the same type T if T is not
// a reference type.

template <typename T> struct RemoveReference: AllStatic { typedef T type; };

template <typename T> struct RemoveReference<T&>: AllStatic { typedef T type; };

#endif
