#ifndef SHARE_VM_UTILITIES_PAIR_HPP
#define SHARE_VM_UTILITIES_PAIR_HPP

#include "memory/allocation.hpp"

template<typename T, typename V,  typename ALLOC_BASE = ResourceObj>
class Pair : public ALLOC_BASE {
 public:
  T first;
  V second;

  Pair() {}
  Pair(T t, V v) : first(t), second(v) {}
};

#endif
