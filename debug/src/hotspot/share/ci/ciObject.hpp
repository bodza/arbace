#ifndef SHARE_VM_CI_CIOBJECT_HPP
#define SHARE_VM_CI_CIOBJECT_HPP

#include "ci/ciBaseObject.hpp"
#include "ci/ciClassList.hpp"
#include "runtime/handles.hpp"
#include "runtime/jniHandles.hpp"

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
class ciObject : public ciBaseObject {
  CI_PACKAGE_ACCESS
  friend class ciEnv;

private:
  // A JNI handle referring to an oop in the VM.  This
  // handle may, in a small set of cases, correctly be NULL.
  jobject  _handle;
  ciKlass* _klass;

protected:
  ciObject();
  ciObject(oop o);
  ciObject(Handle h);
  ciObject(ciKlass* klass);

  jobject      handle()  const { return _handle; }
  // Get the VM oop that this object holds.
  oop get_oop() const;

  void init_flags_from(oop x);

  // Virtual behavior of the print() method.
  virtual void print_impl(outputStream* st) { }

  virtual const char* type_string() { return "ciObject"; }

public:
  // The klass of this ciObject.
  ciKlass* klass();

  // Are two ciObjects equal?
  bool equals(ciObject* obj);

  // A hash value for the convenience of compilers.
  int hash();

  // Tells if this oop has an encoding as a constant.
  // True if is_perm is true.
  // Also true if ScavengeRootsInCode is non-zero.
  // If it does not have an encoding, the compiler is responsible for
  // making other arrangements for dealing with the oop.
  // See ciEnv::make_array
  bool can_be_constant();

  // Tells if this oop should be made a constant.
  // True if is_perm is true or ScavengeRootsInCode > 1.
  bool should_be_constant();

  // Might this object possibly move during a scavenge operation?
  // If the answer is true and ScavengeRootsInCode==0, the oop cannot be embedded in code.
  bool is_scavengable() { return (_ident & SCAVENGABLE_FLAG) != 0; }

  // The address which the compiler should embed into the
  // generated code to represent this oop.  This address
  // is not the true address of the oop -- it will get patched
  // during nmethod creation.
  //
  // Usage note: no address arithmetic allowed.  Oop must
  // be registered with the oopRecorder.
  jobject constant_encoding();

  virtual bool is_object() const            { return true; }

  // What kind of ciObject is this?
  virtual bool is_null_object()       const { return false; }
  virtual bool is_call_site()         const { return false; }
  virtual bool is_cpcache()           const { return false; }
  virtual bool is_instance()                { return false; }
  virtual bool is_member_name()       const { return false; }
  virtual bool is_method_handle()     const { return false; }
  virtual bool is_method_type()       const { return false; }
  virtual bool is_array()                   { return false; }
  virtual bool is_obj_array()               { return false; }
  virtual bool is_type_array()              { return false; }

  // Is this a type or value which has no associated class?
  // It is true of primitive types and null objects.
  virtual bool is_classless() const         { return false; }
  virtual void dump_replay_data(outputStream* st) { /* do nothing */ }

  // Note: some ciObjects refer to oops which have yet to be created.
  // We refer to these as "unloaded".  Specifically, there are
  // unloaded instances of java.lang.Class,
  // java.lang.invoke.MethodHandle, and java.lang.invoke.MethodType.
  // By convention the ciNullObject is considered loaded, and
  // primitive types are considered loaded.
  bool is_loaded() const {
    return handle() != NULL || is_classless();
  }

  // Subclass casting with assertions.
  ciNullObject* as_null_object() {
    return (ciNullObject*)this;
  }
  ciCallSite* as_call_site() {
    return (ciCallSite*)this;
  }
  ciInstance* as_instance() {
    return (ciInstance*)this;
  }
  ciMemberName* as_member_name() {
    return (ciMemberName*)this;
  }
  ciMethodHandle* as_method_handle() {
    return (ciMethodHandle*)this;
  }
  ciMethodType* as_method_type() {
    return (ciMethodType*)this;
  }
  ciArray* as_array() {
    return (ciArray*)this;
  }
  ciObjArray* as_obj_array() {
    return (ciObjArray*)this;
  }
  ciTypeArray* as_type_array() {
    return (ciTypeArray*)this;
  }

  // Print debugging output about this ciObject.
  void print(outputStream* st);
  void print() { print(tty); }  // GDB cannot handle default arguments

  // Print debugging output about the oop this ciObject represents.
  void print_oop(outputStream* st = tty);
};

#endif
