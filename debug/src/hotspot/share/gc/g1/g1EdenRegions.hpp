#ifndef SHARE_VM_GC_G1_G1EDENREGIONS_HPP
#define SHARE_VM_GC_G1_G1EDENREGIONS_HPP

#include "gc/g1/heapRegion.hpp"
#include "runtime/globals.hpp"
#include "utilities/debug.hpp"

class G1EdenRegions {
private:
  int _length;

public:
  G1EdenRegions() : _length(0) { }

  void add(HeapRegion* hr) {
    _length++;
  }

  void clear() { _length = 0; }

  uint length() const { return _length; }
};

#endif
