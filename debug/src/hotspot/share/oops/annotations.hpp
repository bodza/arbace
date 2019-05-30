#ifndef SHARE_VM_OOPS_ANNOTATIONS_HPP
#define SHARE_VM_OOPS_ANNOTATIONS_HPP

#include "oops/array.hpp"
#include "oops/metadata.hpp"
#include "utilities/exceptions.hpp"
#include "utilities/globalDefinitions.hpp"

class ClassLoaderData;
class outputStream;
class KlassSizeStats;

typedef Array<u1> AnnotationArray;

// Class to hold the various types of annotations. The only metadata that points
// to this is InstanceKlass, or another Annotations instance if this is a
// a type_annotation instance.

class Annotations: public MetaspaceObj {
  // If you add a new field that points to any metaspace object, you
  // must add this field to Annotations::metaspace_pointers_do().

  // Annotations for this class, or null if none.
  AnnotationArray*             _class_annotations;
  // Annotation objects (byte arrays) for fields, or null if no annotations.
  // Indices correspond to entries (not indices) in fields array.
  Array<AnnotationArray*>*     _fields_annotations;
  // Type annotations for this class, or null if none.
  AnnotationArray*             _class_type_annotations;
  Array<AnnotationArray*>*     _fields_type_annotations;

 public:
  // Allocate instance of this class
  static Annotations* allocate(ClassLoaderData* loader_data, TRAPS);

  static void free_contents(ClassLoaderData* loader_data, Array<AnnotationArray*>* p);
  void deallocate_contents(ClassLoaderData* loader_data);

  // Sizing (in words)
  static int size()    { return sizeof(Annotations) / wordSize; }

  // Annotations should be stored in the read-only region of CDS archive.
  static bool is_read_only_by_default() { return true; }

  // Constructor to initialize to null
  Annotations() : _class_annotations(NULL),
                  _fields_annotations(NULL),
                  _class_type_annotations(NULL),
                  _fields_type_annotations(NULL) {}

  AnnotationArray* class_annotations() const                       { return _class_annotations; }
  Array<AnnotationArray*>* fields_annotations() const              { return _fields_annotations; }
  AnnotationArray* class_type_annotations() const                  { return _class_type_annotations; }
  Array<AnnotationArray*>* fields_type_annotations() const         { return _fields_type_annotations; }

  void set_class_annotations(AnnotationArray* md)                     { _class_annotations = md; }
  void set_fields_annotations(Array<AnnotationArray*>* md)            { _fields_annotations = md; }
  void set_class_type_annotations(AnnotationArray* cta)               { _class_type_annotations = cta; }
  void set_fields_type_annotations(Array<AnnotationArray*>* fta)      { _fields_type_annotations = fta; }

  // Turn metadata annotations into a Java heap object (oop)
  static typeArrayOop make_java_array(AnnotationArray* annotations, TRAPS);

  bool is_klass() const { return false; }
  void metaspace_pointers_do(MetaspaceClosure* it);
  MetaspaceObj::Type type() const { return AnnotationsType; }

 private:
  static julong count_bytes(Array<AnnotationArray*>* p);
 public:
  const char* internal_name() const { return "{constant pool}"; }
  void print_value_on(outputStream* st) const;
};
#endif
