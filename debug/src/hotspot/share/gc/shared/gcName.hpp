#ifndef SHARE_VM_GC_SHARED_GCNAME_HPP
#define SHARE_VM_GC_SHARED_GCNAME_HPP

#include "utilities/debug.hpp"

enum GCName {
  ParallelOld,
  SerialOld,
  PSMarkSweep,
  ParallelScavenge,
  DefNew,
  ParNew,
  G1New,
  ConcurrentMarkSweep,
  G1Old,
  G1Full,
  Z,
  NA,
  GCNameEndSentinel
};

class GCNameHelper {
 public:
  static const char* to_string(GCName name) {
    switch (name) {
      case ParallelOld: return "ParallelOld";
      case SerialOld: return "SerialOld";
      case PSMarkSweep: return "PSMarkSweep";
      case ParallelScavenge: return "ParallelScavenge";
      case DefNew: return "DefNew";
      case ParNew: return "ParNew";
      case G1New: return "G1New";
      case ConcurrentMarkSweep: return "ConcurrentMarkSweep";
      case G1Old: return "G1Old";
      case G1Full: return "G1Full";
      case Z: return "Z";
      case NA: return "N/A";
      default: ShouldNotReachHere(); return NULL;
    }
  }
};

#endif
