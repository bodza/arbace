#include "precompiled.hpp"

#include "ci/ciObject.hpp"
#include "ci/ciUtilities.inline.hpp"
#include "gc/shared/collectedHeap.inline.hpp"
#include "oops/oop.inline.hpp"
#include "runtime/jniHandles.inline.hpp"

// ciObject
//
// This class represents an oop in the HotSpot virtual machine.
// Its subclasses are structured in a hierarchy which mirrors
// an aggregate of the VM's oop and klass hierarchies (see
// oopHierarchy.hpp).  Each instance of ciObject holds a handle
// to a corresponding oop on the VM side and provides routines
// for accessing the information in its oop.  By using the ciObject
// hierarchy for accessing oops in the VM, the compiler ensures
// that it is safe with respect to garbage collection; that is,
// GC and compilation can proceed independently without
// interference.
//
// Within the VM, the oop and klass hierarchies are separate.
// The compiler interface does not preserve this separation --
// the distinction between `Klass*' and `Klass' are not
// reflected in the interface and instead the Klass hierarchy
// is directly modeled as the subclasses of ciKlass.

ciObject::ciObject(oop o) {
  if (ciObjectFactory::is_initialized()) {
    _handle = JNIHandles::make_local(o);
  } else {
    Handle obj(Thread::current(), o);
    _handle = JNIHandles::make_global(obj);
  }
  _klass = NULL;
  init_flags_from(o);
}

ciObject::ciObject(Handle h) {
  if (ciObjectFactory::is_initialized()) {
    _handle = JNIHandles::make_local(h());
  } else {
    _handle = JNIHandles::make_global(h);
  }
  _klass = NULL;
  init_flags_from(h());
}

// Unloaded klass/method variant.  `klass' is the klass of the unloaded
// klass/method, if that makes sense.
ciObject::ciObject(ciKlass* klass) {
  _handle = NULL;
  _klass = klass;
}

// NULL variant.  Used only by ciNullObject.
ciObject::ciObject() {
  _handle = NULL;
  _klass = NULL;
}

// Get the oop of this ciObject.
oop ciObject::get_oop() const {
  return JNIHandles::resolve_non_null(_handle);
}

// Get the ciKlass of this ciObject.
ciKlass* ciObject::klass() {
  if (_klass == NULL) {
    if (_handle == NULL) {
      // When both _klass and _handle are NULL, we are dealing
      // with the distinguished instance of ciNullObject.
      // No one should ask it for its klass.
      ShouldNotReachHere();
      return NULL;
    }

    GUARDED_VM_ENTRY(
      oop o = get_oop();
      _klass = ciEnv::current()->get_klass(o->klass());
    );
  }
  return _klass;
}

// Are two ciObjects equal?
bool ciObject::equals(ciObject* obj) {
  return (this == obj);
}

// A hash value for the convenience of compilers.
//
// Implementation note: we use the address of the ciObject as the
// basis for the hash.  Use the _ident field, which is well-behaved.
int ciObject::hash() {
  return ident() * 31;
}

// The address which the compiler should embed into the
// generated code to represent this oop.  This address
// is not the true address of the oop -- it will get patched
// during nmethod creation.
//
//
//
// Implementation note: we use the handle as the encoding.  The
// nmethod constructor resolves the handle and patches in the oop.
//
// This method should be changed to return an generified address
// to discourage use of the JNI handle.
jobject ciObject::constant_encoding() {
  return handle();
}

bool ciObject::can_be_constant() {
  if (ScavengeRootsInCode >= 1)  return true;  // now everybody can encode as a constant
  return handle() == NULL;
}

bool ciObject::should_be_constant() {
  if (ScavengeRootsInCode >= 2)  return true;  // force everybody to be a constant
  if (is_null_object()) return true;

  ciEnv* env = ciEnv::current();

  // We want Strings and Classes to be embeddable by default since
  // they used to be in the perm world.  Not all Strings used to be
  // embeddable but there's no easy way to distinguish the interned
  // from the regulars ones so just treat them all that way.
  if (klass() == env->String_klass() || klass() == env->Class_klass()) {
    return true;
  }

  return handle() == NULL;
}

void ciObject::init_flags_from(oop x) {
  int flags = 0;
  if (x != NULL) {
    if (Universe::heap()->is_scavengable(x))
      flags |= SCAVENGABLE_FLAG;
  }
  _ident |= flags;
}

// Print debugging output about this ciObject.
//
// Implementation note: dispatch to the virtual print_impl behavior
// for this ciObject.
void ciObject::print(outputStream* st) {
  st->print("<%s", type_string());
  GUARDED_VM_ENTRY(print_impl(st);)
  st->print(" ident=%d %s address=" INTPTR_FORMAT ">", ident(), is_scavengable() ? "SCAVENGABLE" : "", p2i((address)this));
}

// Print debugging output about the oop this ciObject represents.
void ciObject::print_oop(outputStream* st) {
  if (is_null_object()) {
    st->print_cr("NULL");
  } else if (!is_loaded()) {
    st->print_cr("UNLOADED");
  } else {
    GUARDED_VM_ENTRY(get_oop()->print_on(st);)
  }
}
