#ifndef SHARE_VM_GC_G1_G1SURVIVORREGIONS_HPP
#define SHARE_VM_GC_G1_G1SURVIVORREGIONS_HPP

#include "runtime/globals.hpp"

template <typename T>
class GrowableArray;
class HeapRegion;

class G1SurvivorRegions {
private:
  GrowableArray<HeapRegion*>* _regions;

public:
  G1SurvivorRegions();

  void add(HeapRegion* hr);

  void convert_to_eden();

  void clear();

  uint length() const;

  const GrowableArray<HeapRegion*>* regions() const {
    return _regions;
  }
};

#endif
