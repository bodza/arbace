#include "precompiled.hpp"

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
}
