#ifndef SHARE_VM_MEMORY_METADATAFACTORY_HPP
#define SHARE_VM_MEMORY_METADATAFACTORY_HPP

#include "classfile/classLoaderData.hpp"
#include "oops/array.hpp"
#include "utilities/exceptions.hpp"
#include "utilities/globalDefinitions.hpp"

class MetadataFactory : AllStatic {
 public:
  template <typename T>
  static Array<T>* new_array(ClassLoaderData* loader_data, int length, TRAPS) {
    // The "true" argument is because all metadata arrays are read only when
    // dumped to the shared archive
    return new (loader_data, length, THREAD) Array<T>(length);
  }

  template <typename T>
  static Array<T>* new_array(ClassLoaderData* loader_data, int length, T value, TRAPS) {
    Array<T>* array = new_array<T>(loader_data, length, CHECK_NULL);
    for (int i = 0; i < length; i++) {
      array->at_put(i, value);
    }
    return array;
  }

  template <typename T>
  static void free_array(ClassLoaderData* loader_data, Array<T>* data) {
    if (data != NULL) {
      int size = data->size();
      loader_data->metaspace_non_null()->deallocate((MetaWord*)data, size, false);
    }
  }

  // Deallocation method for metadata
  template <class T>
  static void free_metadata(ClassLoaderData* loader_data, T md) {
    if (md != NULL) {
      int size = md->size();
      md->deallocate_contents(loader_data);
      loader_data->metaspace_non_null()->deallocate((MetaWord*)md, size, md->is_klass());
    }
  }
};

#endif
