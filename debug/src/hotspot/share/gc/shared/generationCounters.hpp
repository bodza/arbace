#ifndef SHARE_VM_GC_SHARED_GENERATIONCOUNTERS_HPP
#define SHARE_VM_GC_SHARED_GENERATIONCOUNTERS_HPP

#include "memory/virtualspace.hpp"
#include "runtime/perfData.hpp"

// A GenerationCounter is a holder class for performance counters
// that track a generation

class GenerationCounters: public CHeapObj<mtGC> {
  friend class VMStructs;

private:
  void initialize(const char* name, int ordinal, int spaces,
                  size_t min_capacity, size_t max_capacity,
                  size_t curr_capacity);

 protected:
  PerfVariable*      _current_size;
  VirtualSpace*      _virtual_space;

  // Constant PerfData types don't need to retain a reference.
  // However, it's a good idea to document them here.
  // PerfStringConstant*     _name;
  // PerfConstant*           _min_size;
  // PerfConstant*           _max_size;
  // PerfConstant*           _spaces;

  char*              _name_space;

  // This constructor is only meant for use with the PSGenerationCounters
  // constructor. The need for such an constructor should be eliminated
  // when VirtualSpace and PSVirtualSpace are unified.
  GenerationCounters()
             : _name_space(NULL), _current_size(NULL), _virtual_space(NULL) {}

  // This constructor is used for subclasses that do not have a space
  // associated with them (e.g, in G1).
  GenerationCounters(const char* name, int ordinal, int spaces,
                     size_t min_capacity, size_t max_capacity,
                     size_t curr_capacity);

 public:
  GenerationCounters(const char* name, int ordinal, int spaces,
                     size_t min_capacity, size_t max_capacity, VirtualSpace* v);

  ~GenerationCounters();

  virtual void update_all();

  const char* name_space() const        { return _name_space; }
};
#endif
