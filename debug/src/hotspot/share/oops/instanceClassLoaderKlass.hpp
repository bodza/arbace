#ifndef SHARE_VM_OOPS_INSTANCECLASSLOADERKLASS_HPP
#define SHARE_VM_OOPS_INSTANCECLASSLOADERKLASS_HPP

#include "oops/instanceKlass.hpp"
#include "utilities/macros.hpp"

class ClassFileParser;

// An InstanceClassLoaderKlass is a specialization of the InstanceKlass. It does
// not add any field.  It is added to walk the dependencies for the class loader
// key that this class loader points to.  This is how the loader_data graph is
// walked and dependant class loaders are kept alive.  I thought we walked
// the list later?

class InstanceClassLoaderKlass: public InstanceKlass {
  friend class VMStructs;
  friend class InstanceKlass;
public:
  static const KlassID ID = InstanceClassLoaderKlassID;

private:
  InstanceClassLoaderKlass(const ClassFileParser& parser) : InstanceKlass(parser, InstanceKlass::_misc_kind_class_loader, ID) {}

public:
  InstanceClassLoaderKlass() {
    assert(DumpSharedSpaces || UseSharedSpaces, "only for CDS"); }

  // GC specific object visitors
  //

  // Oop fields (and metadata) iterators
  //
  // The InstanceClassLoaderKlass iterators also visit the CLD pointer (or mirror of anonymous klasses.)

 public:
  // Forward iteration
  // Iterate over the oop fields and metadata.
  template <typename T, class OopClosureType>
  inline void oop_oop_iterate(oop obj, OopClosureType* closure);

  // Reverse iteration
  // Iterate over the oop fields and metadata.
  template <typename T, class OopClosureType>
  inline void oop_oop_iterate_reverse(oop obj, OopClosureType* closure);

  // Bounded range iteration
  // Iterate over the oop fields and metadata.
  template <typename T, class OopClosureType>
  inline void oop_oop_iterate_bounded(oop obj, OopClosureType* closure, MemRegion mr);
};

#endif
