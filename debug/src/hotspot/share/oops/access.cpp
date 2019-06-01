#include "precompiled.hpp"

#include "oops/access.inline.hpp"
#include "oops/accessDecorators.hpp"

// This macro allows instantiating selected accesses to be usable from the
// access.hpp file, to break dependencies to the access.inline.hpp file.
#define INSTANTIATE_HPP_ACCESS(decorators, T, barrier_type) \
  template struct RuntimeDispatch<DecoratorFixup<decorators>::value, T, barrier_type>

namespace AccessInternal {
  INSTANTIATE_HPP_ACCESS(INTERNAL_EMPTY, oop, BARRIER_EQUALS);
}
