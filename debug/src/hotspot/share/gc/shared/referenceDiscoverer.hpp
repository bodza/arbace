#ifndef SHARE_GC_SHARED_REFERENCEDISCOVERER_HPP
#define SHARE_GC_SHARED_REFERENCEDISCOVERER_HPP

#include "memory/allocation.hpp"
#include "memory/referenceType.hpp"
#include "oops/oopsHierarchy.hpp"

class ReferenceDiscoverer : public CHeapObj<mtGC> {
public:
  virtual bool discover_reference(oop obj, ReferenceType type) = 0;
};

#endif
