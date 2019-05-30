#ifndef SHARE_VM_GC_G1_G1REGIONTOSPACEMAPPER_HPP
#define SHARE_VM_GC_G1_G1REGIONTOSPACEMAPPER_HPP

#include "gc/g1/g1PageBasedVirtualSpace.hpp"
#include "memory/allocation.hpp"
#include "utilities/debug.hpp"

class WorkGang;

class G1MappingChangedListener {
 public:
  // Fired after commit of the memory, i.e. the memory this listener is registered
  // for can be accessed.
  // Zero_filled indicates that the memory can be considered as filled with zero bytes
  // when called.
  virtual void on_commit(uint start_idx, size_t num_regions, bool zero_filled) = 0;
};

// Maps region based commit/uncommit requests to the underlying page sized virtual
// space.
class G1RegionToSpaceMapper : public CHeapObj<mtGC> {
 private:
  G1MappingChangedListener* _listener;
 protected:
  // Backing storage.
  G1PageBasedVirtualSpace _storage;

  size_t _region_granularity;
  // Mapping management
  CHeapBitMap _commit_map;

  G1RegionToSpaceMapper(ReservedSpace rs, size_t used_size, size_t page_size, size_t region_granularity, size_t commit_factor, MemoryType type);

  void fire_on_commit(uint start_idx, size_t num_regions, bool zero_filled);
 public:
  MemRegion reserved() { return _storage.reserved(); }

  size_t reserved_size() { return _storage.reserved_size(); }
  size_t committed_size() { return _storage.committed_size(); }

  void set_mapping_changed_listener(G1MappingChangedListener* listener) { _listener = listener; }

  virtual ~G1RegionToSpaceMapper() {}

  bool is_committed(uintptr_t idx) const {
    return _commit_map.at(idx);
  }

  virtual void commit_regions(uint start_idx, size_t num_regions = 1, WorkGang* pretouch_workers = NULL) = 0;
  virtual void uncommit_regions(uint start_idx, size_t num_regions = 1) = 0;

  // Creates an appropriate G1RegionToSpaceMapper for the given parameters.
  // The actual space to be used within the given reservation is given by actual_size.
  // This is because some OSes need to round up the reservation size to guarantee
  // alignment of page_size.
  // The byte_translation_factor defines how many bytes in a region correspond to
  // a single byte in the data structure this mapper is for.
  // Eg. in the card table, this value corresponds to the size a single card
  // table entry corresponds to in the heap.
  static G1RegionToSpaceMapper* create_mapper(ReservedSpace rs,
                                              size_t actual_size,
                                              size_t page_size,
                                              size_t region_granularity,
                                              size_t byte_translation_factor,
                                              MemoryType type);
};

#endif
