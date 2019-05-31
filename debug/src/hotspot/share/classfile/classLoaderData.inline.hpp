#ifndef SHARE_VM_CLASSFILE_CLASSLOADERDATA_INLINE_HPP
#define SHARE_VM_CLASSFILE_CLASSLOADERDATA_INLINE_HPP

#include "classfile/classLoaderData.hpp"
#include "classfile/javaClasses.hpp"
#include "oops/oop.inline.hpp"
#include "oops/oopHandle.inline.hpp"
#include "oops/weakHandle.inline.hpp"

inline oop ClassLoaderData::class_loader() const {
  return _class_loader.resolve();
}

inline bool ClassLoaderData::is_boot_class_loader_data() const {
    return class_loader() == NULL;
  }

inline ClassLoaderData* ClassLoaderData::class_loader_data_or_null(oop loader) {
  if (loader == NULL) {
    return ClassLoaderData::the_null_class_loader_data();
  }
  return java_lang_ClassLoader::loader_data(loader);
}

inline ClassLoaderData* ClassLoaderData::class_loader_data(oop loader) {
  ClassLoaderData* loader_data = class_loader_data_or_null(loader);
  return loader_data;
}

inline ClassLoaderData *ClassLoaderDataGraph::find_or_create(Handle loader) {
  guarantee(loader() != NULL && oopDesc::is_oop(loader()), "Loader must be oop");
  // Gets the class loader data out of the java/lang/ClassLoader object, if non-null
  // it's already in the loader_data, so no need to add
  ClassLoaderData* loader_data= java_lang_ClassLoader::loader_data(loader());
  if (loader_data) {
     return loader_data;
  }
  return ClassLoaderDataGraph::add(loader, false);
}

size_t ClassLoaderDataGraph::num_instance_classes() {
  return _num_instance_classes;
}

size_t ClassLoaderDataGraph::num_array_classes() {
  return _num_array_classes;
}

void ClassLoaderDataGraph::inc_instance_classes(size_t count) {
  Atomic::add(count, &_num_instance_classes);
}

void ClassLoaderDataGraph::dec_instance_classes(size_t count) {
  Atomic::sub(count, &_num_instance_classes);
}

void ClassLoaderDataGraph::inc_array_classes(size_t count) {
  Atomic::add(count, &_num_array_classes);
}

void ClassLoaderDataGraph::dec_array_classes(size_t count) {
  Atomic::sub(count, &_num_array_classes);
}

#endif
