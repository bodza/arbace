#ifndef SHARE_VM_OOPS_WEAKHANDLE_HPP
#define SHARE_VM_OOPS_WEAKHANDLE_HPP

#include "oops/oop.hpp"
#include "runtime/handles.hpp"

class outputStream;
class OopStorage;

// A WeakHandle is a pointer to an oop that is stored in an OopStorage that is
// processed weakly by GC.  The runtime structures that point to the oop must
// either peek or resolve the oop, the latter will keep the oop alive for
// the GC cycle.  The runtime structures that reference the oop must test
// if the value is NULL.  If it is NULL, it has been cleaned out by GC.
// This is the vm version of jweak but has different GC lifetimes and policies,
// depending on the type.

enum WeakHandleType { vm_class_loader_data, vm_string, vm_string_table_data };

template <WeakHandleType T>
class WeakHandle {
 public:
 private:
  oop* _obj;

  WeakHandle(oop* w) : _obj(w) {}
  static OopStorage* get_storage();
 public:
  WeakHandle() : _obj(NULL) {} // needed for init

  static WeakHandle create(Handle obj);
  inline oop resolve() const;
  inline oop peek() const;
  void release() const;
  bool is_null() const { return _obj == NULL; }

  void replace(oop with_obj);

  void print() const;
  void print_on(outputStream* st) const;
};

typedef WeakHandle<vm_class_loader_data> ClassLoaderWeakHandle;

#endif
