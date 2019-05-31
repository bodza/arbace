#ifndef SHARE_VM_GC_SHARED_ISGCACTIVEMARK_HPP
#define SHARE_VM_GC_SHARED_ISGCACTIVEMARK_HPP

#include "gc/shared/collectedHeap.hpp"
#include "memory/allocation.hpp"
#include "memory/universe.hpp"
#include "utilities/debug.hpp"

// This class provides a method for block structured setting of the
// _is_gc_active state without requiring accessors in CollectedHeap

class IsGCActiveMark : public StackObj {
 public:
  IsGCActiveMark() {
    CollectedHeap* heap = Universe::heap();
    heap->_is_gc_active = true;
  }

  ~IsGCActiveMark() {
    CollectedHeap* heap = Universe::heap();
    heap->_is_gc_active = false;
  }
};

#endif
