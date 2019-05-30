#ifndef SHARE_GC_G1_G1FULLGCCOMPACTIONPOINT_HPP
#define SHARE_GC_G1_G1FULLGCCOMPACTIONPOINT_HPP

#include "memory/allocation.hpp"
#include "oops/oopsHierarchy.hpp"
#include "utilities/growableArray.hpp"

class HeapRegion;

class G1FullGCCompactionPoint : public CHeapObj<mtGC> {
  HeapRegion* _current_region;
  HeapWord*   _threshold;
  HeapWord*   _compaction_top;
  GrowableArray<HeapRegion*>* _compaction_regions;
  GrowableArrayIterator<HeapRegion*> _compaction_region_iterator;

  bool object_will_fit(size_t size);
  void initialize_values(bool init_threshold);
  void switch_region();
  HeapRegion* next_region();

public:
  G1FullGCCompactionPoint();
  ~G1FullGCCompactionPoint();

  bool has_regions();
  bool is_initialized();
  void initialize(HeapRegion* hr, bool init_threshold);
  void update();
  void forward(oop object, size_t size);
  void add(HeapRegion* hr);
  void merge(G1FullGCCompactionPoint* other);

  HeapRegion* remove_last();
  HeapRegion* current_region();

  GrowableArray<HeapRegion*>* regions();
};

#endif
