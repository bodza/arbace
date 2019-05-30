#ifndef SHARE_GC_SHARED_GCTHREADLOCALDATA_HPP
#define SHARE_GC_SHARED_GCTHREADLOCALDATA_HPP

#include "utilities/globalDefinitions.hpp"

// Thread local data area for GC-specific information. Each GC
// is free to decide the internal structure and contents of this
// area. It is represented as a 64-bit aligned opaque blob to
// avoid circular dependencies between Thread and all GCs. For
// the same reason, the size of the data area is hard coded to
// provide enough space for all current GCs. Adjust the size if
// needed, but avoid making it excessively large as it adds to
// the memory overhead of creating a thread.
//
// Use Thread::gc_data<T>() to access the data, where T is the
// GC-specific type describing the structure of the data. GCs
// should consider placing frequently accessed fields first in
// T, so that field offsets relative to Thread are small, which
// often allows for a more compact instruction encoding.
typedef uint64_t GCThreadLocalData[18]; // 144 bytes

#endif
