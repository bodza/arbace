#include "precompiled.hpp"
#include "gc/g1/g1BiasedArray.hpp"
#include "memory/padded.inline.hpp"

// Allocate a new array, generic version.
address G1BiasedMappedArrayBase::create_new_base_array(size_t length, size_t elem_size) {
  assert(length > 0, "just checking");
  assert(elem_size > 0, "just checking");
  return PaddedPrimitiveArray<u_char, mtGC>::create_unfreeable(length * elem_size);
}
