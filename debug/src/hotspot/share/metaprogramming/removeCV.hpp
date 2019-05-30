#ifndef SHARE_VM_METAPROGRAMMING_REMOVECV_HPP
#define SHARE_VM_METAPROGRAMMING_REMOVECV_HPP

#include "memory/allocation.hpp"

template <typename T>
struct RemoveCV: AllStatic {
  typedef T type;
};

template <typename T>
struct RemoveCV<const T>: AllStatic {
  typedef T type;
};

template <typename T>
struct RemoveCV<volatile T>: AllStatic {
  typedef T type;
};

template <typename T>
struct RemoveCV<const volatile T>: AllStatic {
  typedef T type;
};

#endif
