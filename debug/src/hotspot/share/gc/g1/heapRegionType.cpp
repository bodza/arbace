#include "precompiled.hpp"

#include "gc/g1/g1HeapRegionTraceType.hpp"
#include "gc/g1/heapRegionType.hpp"

bool HeapRegionType::is_valid(Tag tag) {
  switch (tag) {
    case FreeTag:
    case EdenTag:
    case SurvTag:
    case StartsHumongousTag:
    case ContinuesHumongousTag:
    case OldTag:
    case OpenArchiveTag:
    case ClosedArchiveTag:
      return true;
    default:
      return false;
  }
}

const char* HeapRegionType::get_str() const {
  switch (_tag) {
    case FreeTag:               return "FREE";
    case EdenTag:               return "EDEN";
    case SurvTag:               return "SURV";
    case StartsHumongousTag:    return "HUMS";
    case ContinuesHumongousTag: return "HUMC";
    case OldTag:                return "OLD";
    case OpenArchiveTag:        return "OARC";
    case ClosedArchiveTag:      return "CARC";
    default:
      ShouldNotReachHere();
      return NULL; // keep some compilers happy
  }
}

const char* HeapRegionType::get_short_str() const {
  switch (_tag) {
    case FreeTag:               return "F";
    case EdenTag:               return "E";
    case SurvTag:               return "S";
    case StartsHumongousTag:    return "HS";
    case ContinuesHumongousTag: return "HC";
    case OldTag:                return "O";
    case OpenArchiveTag:        return "OA";
    case ClosedArchiveTag:      return "CA";
    default:
      ShouldNotReachHere();
      return NULL; // keep some compilers happy
  }
}

G1HeapRegionTraceType::Type HeapRegionType::get_trace_type() {
  switch (_tag) {
    case FreeTag:               return G1HeapRegionTraceType::Free;
    case EdenTag:               return G1HeapRegionTraceType::Eden;
    case SurvTag:               return G1HeapRegionTraceType::Survivor;
    case StartsHumongousTag:    return G1HeapRegionTraceType::StartsHumongous;
    case ContinuesHumongousTag: return G1HeapRegionTraceType::ContinuesHumongous;
    case OldTag:                return G1HeapRegionTraceType::Old;
    case OpenArchiveTag:        return G1HeapRegionTraceType::OpenArchive;
    case ClosedArchiveTag:      return G1HeapRegionTraceType::ClosedArchive;
    default:
      ShouldNotReachHere();
      return G1HeapRegionTraceType::Free; // keep some compilers happy
  }
}
