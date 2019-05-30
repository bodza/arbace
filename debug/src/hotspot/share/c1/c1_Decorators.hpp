#ifndef SHARE_VM_C1_C1_DECORATORS_HPP
#define SHARE_VM_C1_C1_DECORATORS_HPP

#include "oops/accessDecorators.hpp"
#include "utilities/globalDefinitions.hpp"

// Use the C1_NEEDS_PATCHING decorator for situations when the access is using
// an offset that is not yet known and will require patching
const DecoratorSet C1_NEEDS_PATCHING = DECORATOR_LAST << 1;
// Use the C1_MASK_BOOLEAN decorator for boolean accesses where the value
// needs to be masked.
const DecoratorSet C1_MASK_BOOLEAN   = DECORATOR_LAST << 2;
// The C1_WRITE_ACCESS decorator is used to mark writing accesses.
const DecoratorSet C1_WRITE_ACCESS   = DECORATOR_LAST << 3;
// The C1_READ_ACCESS decorator is used to mark reading accesses.
const DecoratorSet C1_READ_ACCESS    = DECORATOR_LAST << 4;

#endif
