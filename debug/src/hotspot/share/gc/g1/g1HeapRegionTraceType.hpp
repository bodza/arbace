#ifndef SHARE_VM_GC_G1_G1HEAPREGIONTRACETYPE_HPP
#define SHARE_VM_GC_G1_G1HEAPREGIONTRACETYPE_HPP

#include "memory/allocation.hpp"
#include "utilities/debug.hpp"

class G1HeapRegionTraceType : AllStatic {
 public:
  enum Type {
    Free,
    Eden,
    Survivor,
    StartsHumongous,
    ContinuesHumongous,
    Old,
    Pinned,
    OpenArchive,
    ClosedArchive,
    G1HeapRegionTypeEndSentinel
  };

  static const char* to_string(G1HeapRegionTraceType::Type type) {
    switch (type) {
      case Free:               return "Free";
      case Eden:               return "Eden";
      case Survivor:           return "Survivor";
      case StartsHumongous:    return "Starts Humongous";
      case ContinuesHumongous: return "Continues Humongous";
      case Old:                return "Old";
      case Pinned:             return "Pinned";
      case OpenArchive:        return "OpenArchive";
      case ClosedArchive:      return "ClosedArchive";
      default: ShouldNotReachHere(); return NULL;
    }
  }
};

#endif
