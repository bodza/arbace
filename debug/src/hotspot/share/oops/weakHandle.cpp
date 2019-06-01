#include "precompiled.hpp"

#include "classfile/systemDictionary.hpp"
#include "classfile/stringTable.hpp"
#include "gc/shared/oopStorage.hpp"
#include "oops/access.inline.hpp"
#include "oops/oop.hpp"
#include "oops/weakHandle.inline.hpp"
#include "utilities/debug.hpp"
#include "utilities/ostream.hpp"

template <> OopStorage* WeakHandle<vm_class_loader_data>::get_storage() {
  return SystemDictionary::vm_weak_oop_storage();
}

template <> OopStorage* WeakHandle<vm_string_table_data>::get_storage() {
  return StringTable::weak_storage();
}

template <WeakHandleType T>
WeakHandle<T> WeakHandle<T>::create(Handle obj) {
  oop* oop_addr = get_storage()->allocate();
  if (oop_addr == NULL) {
    vm_exit_out_of_memory(sizeof(oop*), OOM_MALLOC_ERROR, "Unable to create new weak oop handle in OopStorage");
  }
  // Create WeakHandle with address returned and store oop into it.
  NativeAccess<ON_PHANTOM_OOP_REF>::oop_store(oop_addr, obj());
  return WeakHandle(oop_addr);
}

template <WeakHandleType T>
void WeakHandle<T>::release() const {
  // Only release if the pointer to the object has been created.
  if (_obj != NULL) {
    // Clear the WeakHandle.  For race in creating ClassLoaderData, we can release this
    // WeakHandle before it is cleared by GC.
    NativeAccess<ON_PHANTOM_OOP_REF>::oop_store(_obj, (oop)NULL);
    get_storage()->release(_obj);
  }
}

template <WeakHandleType T>
void WeakHandle<T>::print() const { print_on(tty); }

template <WeakHandleType T>
void WeakHandle<T>::print_on(outputStream* st) const {
  st->print("WeakHandle: " PTR_FORMAT, p2i(peek()));
}

// Provide instantiation.
template class WeakHandle<vm_class_loader_data>;
template class WeakHandle<vm_string_table_data>;
