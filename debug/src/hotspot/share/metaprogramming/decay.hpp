#ifndef SHARE_VM_METAPROGRAMMING_DECAY_HPP
#define SHARE_VM_METAPROGRAMMING_DECAY_HPP

#include "memory/allocation.hpp"
#include "metaprogramming/removeCV.hpp"
#include "metaprogramming/removeReference.hpp"

// This trait trims the type from CV qualifiers and references.
// This trait provides a subset of the functionality of std::decay;
// array types and function types are not supported here.

template <typename T>
struct Decay: AllStatic {
  typedef typename RemoveCV<typename RemoveReference<T>::type>::type type;
};

#endif
