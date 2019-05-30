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

  // Check that we have the right class
  assert(k == SystemDictionary::Reference_klass() && first_time, "Invalid update of maps");
  assert(ik->nonstatic_oop_map_count() == 1, "just checking");

  OopMapBlock* map = ik->start_of_nonstatic_oop_maps();

  // Updated map starts at "queue", covers "queue" and "next".
  const int new_offset = java_lang_ref_Reference::queue_offset;
  const unsigned int new_count = 2; // queue and next

  // Verify existing map is as expected, and update if needed.
  if (UseSharedSpaces) {
    assert(map->offset() == new_offset, "just checking");
    assert(map->count() == new_count, "just checking");
  } else {
    assert(map->offset() == referent_offset, "just checking");
    assert(map->count() == count, "just checking");
    map->set_offset(new_offset);
    map->set_count(new_count);
  }
}

// Verification

void InstanceRefKlass::oop_verify_on(oop obj, outputStream* st) {
  InstanceKlass::oop_verify_on(obj, st);
  // Verify referent field
  oop referent = java_lang_ref_Reference::referent(obj);
  if (referent != NULL) {
    guarantee(oopDesc::is_oop(referent), "referent field heap failed");
  }
  // Additional verification for next field, which must be a Reference or null
  oop next = java_lang_ref_Reference::next(obj);
  if (next != NULL) {
    guarantee(oopDesc::is_oop(next), "next field should be an oop");
    guarantee(next->is_instance(), "next field should be an instance");
    guarantee(InstanceKlass::cast(next->klass())->is_reference_instance_klass(), "next field verify failed");
  }
}
