#ifndef SHARE_VM_GC_SHARED_HSPACECOUNTERS_HPP
#define SHARE_VM_GC_SHARED_HSPACECOUNTERS_HPP

#include "memory/allocation.hpp"
#include "runtime/perfData.hpp"
#include "utilities/macros.hpp"

// A HSpaceCounter is a holder class for performance counters
// that track a collections (logical spaces) in a heap;

class HSpaceCounters: public CHeapObj<mtGC> {
  friend class VMStructs;

 private:
  PerfVariable* _capacity;
  PerfVariable* _used;

  // Constant PerfData types don't need to retain a reference.
  // However, it's a good idea to document them here.

  char*         _name_space;

 public:

  HSpaceCounters(const char* name_space, const char* name, int ordinal,
                 size_t max_size, size_t initial_capacity);

  ~HSpaceCounters();

  void update_capacity(size_t v);
  void update_used(size_t v);

  void update_all(size_t capacity, size_t used);

  const char* name_space() const        { return _name_space; }
};
#endif
