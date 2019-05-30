#ifndef SHARE_VM_GC_SHARED_AGETABLE_INLINE_HPP
#define SHARE_VM_GC_SHARED_AGETABLE_INLINE_HPP

#include "gc/shared/ageTable.hpp"
#include "oops/oop.inline.hpp"

// add entry
void AgeTable::add(oop p, size_t oop_size) {
  add(p->age(), oop_size);
}

#endif
