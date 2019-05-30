#ifndef SHARE_VM_GC_SHARED_GCWHEN_HPP
#define SHARE_VM_GC_SHARED_GCWHEN_HPP

#include "memory/allocation.hpp"
#include "utilities/debug.hpp"

class GCWhen : AllStatic {
 public:
  enum Type {
    BeforeGC,
    AfterGC,
    GCWhenEndSentinel
  };

  static const char* to_string(GCWhen::Type when) {
    switch (when) {
    case BeforeGC: return "Before GC";
    case AfterGC:  return "After GC";
    default: ShouldNotReachHere(); return NULL;
    }
  }
};

#endif
