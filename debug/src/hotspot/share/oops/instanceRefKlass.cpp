#include "precompiled.hpp"

#include "classfile/javaClasses.hpp"
#include "classfile/systemDictionary.hpp"
#include "oops/instanceRefKlass.inline.hpp"
#include "oops/oop.inline.hpp"

void InstanceRefKlass::update_nonstatic_oop_maps(Klass* k) {
  // Clear the nonstatic oop-map entries corresponding to referent
  // and discovered fields.  They are treated specially by the
  // garbage collector.
  InstanceKlass* ik = InstanceKlass::cast(k);

  OopMapBlock* map = ik->start_of_nonstatic_oop_maps();

  // Updated map starts at "queue", covers "queue" and "next".
  const int new_offset = java_lang_ref_Reference::queue_offset;
  const unsigned int new_count = 2; // queue and next

  map->set_offset(new_offset);
  map->set_count(new_count);
}
