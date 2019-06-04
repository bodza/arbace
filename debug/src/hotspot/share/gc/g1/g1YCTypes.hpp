#ifndef SHARE_VM_GC_G1_G1YCTYPES_HPP
#define SHARE_VM_GC_G1_G1YCTYPES_HPP

#include "utilities/debug.hpp"

enum G1YCType {
  Normal,
  InitialMark,
  DuringMarkOrRebuild,
  Mixed,
  G1YCTypeEndSentinel
};

class G1YCTypeHelper {
 public:
  static const char* to_string(G1YCType type) {
    switch (type) {
      case Normal: return "Normal";
      case InitialMark: return "Initial Mark";
      case DuringMarkOrRebuild: return "During Mark";
      case Mixed: return "Mixed";
      default: ShouldNotReachHere(); return NULL;
    }
  }
};

#endif
