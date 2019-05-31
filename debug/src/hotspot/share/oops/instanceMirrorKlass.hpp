#ifndef SHARE_VM_OOPS_INSTANCEMIRRORKLASS_HPP
#define SHARE_VM_OOPS_INSTANCEMIRRORKLASS_HPP

#include "classfile/systemDictionary.hpp"
#include "oops/instanceKlass.hpp"
#include "runtime/handles.hpp"
#include "utilities/macros.hpp"

class ClassFileParser;

// An InstanceMirrorKlass is a specialized InstanceKlass for
// java.lang.Class instances.  These instances are special because
// they contain the static fields of the class in addition to the
// normal fields of Class.  This means they are variable sized
// instances and need special logic for computing their size and for
// iteration of their oops.

class InstanceMirrorKlass: public InstanceKlass {
  friend class VMStructs;
  friend class InstanceKlass;

 public:
  static const KlassID ID = InstanceMirrorKlassID;

 private:
  static int _offset_of_static_fields;

  InstanceMirrorKlass(const ClassFileParser& parser) : InstanceKlass(parser, InstanceKlass::_misc_kind_mirror, ID) { }

 public:
  InstanceMirrorKlass() {
    }

  // Casting from Klass*
  static InstanceMirrorKlass* cast(Klass* k) {
    return static_cast<InstanceMirrorKlass*>(k);
  }

  // Returns the size of the instance including the extra static fields.
  virtual int oop_size(oop obj) const;

  // Static field offset is an offset into the Heap, should be converted by
  // based on UseCompressedOop for traversal
  static HeapWord* start_of_static_fields(oop obj) {
    return (HeapWord*)(cast_from_oop<intptr_t>(obj) + offset_of_static_fields());
  }

  static void init_offset_of_static_fields() {
    // Cache the offset of the static fields in the Class instance
    _offset_of_static_fields = InstanceMirrorKlass::cast(SystemDictionary::Class_klass())->size_helper() << LogHeapWordSize;
  }

  static int offset_of_static_fields() {
    return _offset_of_static_fields;
  }

  int compute_static_oop_field_count(oop obj);

  // Given a Klass return the size of the instance
  int instance_size(Klass* k);

  // allocation
  instanceOop allocate_instance(Klass* k, TRAPS);

  // GC specific object visitors
  //

  // Oop fields (and metadata) iterators
  //
  // The InstanceMirrorKlass iterators also visit the hidden Klass pointer.

  // Iterate over the static fields.
  template <typename T, class OopClosureType>
  inline void oop_oop_iterate_statics(oop obj, OopClosureType* closure);

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

 private:

  // Iterate over the static fields.
  template <typename T, class OopClosureType>
  inline void oop_oop_iterate_statics_bounded(oop obj, OopClosureType* closure, MemRegion mr);
};

#endif
