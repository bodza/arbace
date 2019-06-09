#include "precompiled.hpp"

#include "ci/ciKlass.hpp"
#include "ci/ciSymbol.hpp"
#include "ci/ciUtilities.inline.hpp"
#include "oops/oop.inline.hpp"

// ciKlass
//
// This class represents a Klass* in the HotSpot virtual
// machine.

ciKlass::ciKlass(Klass* k) : ciType(k) {
  Klass* klass = get_Klass();
  _layout_helper = klass->layout_helper();
  Symbol* klass_name = klass->name();
  _name = ciEnv::current()->get_symbol(klass_name);
}

// Nameless klass variant.
ciKlass::ciKlass(Klass* k, ciSymbol* name) : ciType(k) {
  _name = name;
  _layout_helper = Klass::_lh_neutral_value;
}

// Unloaded klass variant.
ciKlass::ciKlass(ciSymbol* name, BasicType bt) : ciType(bt) {
  _name = name;
  _layout_helper = Klass::_lh_neutral_value;
}

bool ciKlass::is_subtype_of(ciKlass* that) {
  // Check to see if the klasses are identical.
  if (this == that) {
    return true;
  }

  VM_ENTRY_MARK;
  Klass* this_klass = get_Klass();
  Klass* that_klass = that->get_Klass();
  bool result = this_klass->is_subtype_of(that_klass);

  return result;
}

bool ciKlass::is_subclass_of(ciKlass* that) {
  GUARDED_VM_ENTRY(return get_Klass()->is_subclass_of(that->get_Klass());)
}

juint ciKlass::super_depth() {
  VM_ENTRY_MARK;
  Klass* this_klass = get_Klass();
  return this_klass->super_depth();
}

juint ciKlass::super_check_offset() {
  VM_ENTRY_MARK;
  Klass* this_klass = get_Klass();
  return this_klass->super_check_offset();
}

ciKlass* ciKlass::super_of_depth(juint i) {
  VM_ENTRY_MARK;
  Klass* this_klass = get_Klass();
  Klass* super = this_klass->primary_super_of_depth(i);
  return (super != NULL) ? ciEnv::current(thread)->get_klass(super) : NULL;
}

bool ciKlass::can_be_primary_super() {
  VM_ENTRY_MARK;
  Klass* this_klass = get_Klass();
  return this_klass->can_be_primary_super();
}

// Get the shared parent of two klasses.
//
// Implementation note: this method currently goes "over the wall"
// and does all of the work on the VM side.  It could be rewritten
// to use the super() method and do all of the work (aside from the
// lazy computation of super()) in native mode.  This may be
// worthwhile if the compiler is repeatedly requesting the same lca
// computation or possibly if most of the superklasses have already
// been created as ciObjects anyway.  Something to think about...
ciKlass*
ciKlass::least_common_ancestor(ciKlass* that) {
  // Check to see if the klasses are identical.
  if (this == that) {
    return this;
  }

  VM_ENTRY_MARK;
  Klass* this_klass = get_Klass();
  Klass* that_klass = that->get_Klass();
  Klass* lca        = this_klass->LCA(that_klass);

  // Many times the LCA will be either this_klass or that_klass.
  // Treat these as special cases.
  if (lca == that_klass) {
    return that;
  }
  if (this_klass == lca) {
    return this;
  }

  // Create the ciInstanceKlass for the lca.
  ciKlass* result = ciEnv::current(thread)->get_klass(lca);

  return result;
}

// Find a klass using this klass's class loader.
ciKlass* ciKlass::find_klass(ciSymbol* klass_name) {
  return ciEnv::current()->get_klass_by_name(this, klass_name, false);
}

// Get the instance of java.lang.Class corresponding to this klass.
// If it is an unloaded instance or array klass, return an unloaded
// mirror object of type Class.
ciInstance* ciKlass::java_mirror() {
  GUARDED_VM_ENTRY(
    if (!is_loaded())
      return ciEnv::current()->get_unloaded_klass_mirror(this);
    oop java_mirror = get_Klass()->java_mirror();
    return ciEnv::current()->get_instance(java_mirror);
  )
}

jint ciKlass::modifier_flags() {
  GUARDED_VM_ENTRY(
    return get_Klass()->modifier_flags();
  )
}

jint ciKlass::access_flags() {
  GUARDED_VM_ENTRY(
    return get_Klass()->access_flags().as_int();
  )
}

// Implementation of the print method
void ciKlass::print_impl(outputStream* st) {
  st->print(" name=");
  print_name_on(st);
}

// Print the name of this klass
void ciKlass::print_name_on(outputStream* st) {
  name()->print_symbol_on(st);
}

const char* ciKlass::external_name() const {
  GUARDED_VM_ENTRY(
    return get_Klass()->external_name();
  )
}
