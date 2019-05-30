#include "precompiled.hpp"
#include "services/nmtCommon.hpp"
#include "utilities/globalDefinitions.hpp"

const char* NMTUtil::_memory_type_names[] = {
  "Java Heap",
  "Class",
  "Thread",
  "Thread Stack",
  "Code",
  "GC",
  "Compiler",
  "Internal",
  "Other",
  "Symbol",
  "Native Memory Tracking",
  "Shared class space",
  "Arena Chunk",
  "Test",
  "Tracing",
  "Logging",
  "Arguments",
  "Module",
  "Unknown"
};

const char* NMTUtil::scale_name(size_t scale) {
  switch(scale) {
    case K: return "KB";
    case M: return "MB";
    case G: return "GB";
  }
  ShouldNotReachHere();
  return NULL;
}

size_t NMTUtil::scale_from_name(const char* scale) {
  assert(scale != NULL, "Null pointer check");
  if (strcasecmp(scale, "1") == 0 || strcasecmp(scale, "b") == 0) {
    return 1;
  } else if (strcasecmp(scale, "kb") == 0 || strcasecmp(scale, "k") == 0) {
    return K;
  } else if (strcasecmp(scale, "mb") == 0 || strcasecmp(scale, "m") == 0) {
    return M;
  } else if (strcasecmp(scale, "gb") == 0 || strcasecmp(scale, "g") == 0) {
    return G;
  } else {
    return 0; // Invalid value
  }
  return K;
}
