#ifndef SHARE_VM_SERVICES_CLASSLOADINGSERVICE_HPP
#define SHARE_VM_SERVICES_CLASSLOADINGSERVICE_HPP

#include "logging/log.hpp"
#include "runtime/handles.hpp"
#include "runtime/perfData.hpp"
#include "utilities/growableArray.hpp"
#include "utilities/macros.hpp"

class InstanceKlass;

// VM monitoring and management support for the Class Loading subsystem
class ClassLoadingService : public AllStatic {
private:
  // Counters for classes loaded from class files
  static PerfCounter*  _classes_loaded_count;
  static PerfCounter*  _classes_unloaded_count;
  static PerfCounter*  _classbytes_loaded;
  static PerfCounter*  _classbytes_unloaded;

  // Counters for classes loaded from shared archive
  static PerfCounter*  _shared_classes_loaded_count;
  static PerfCounter*  _shared_classes_unloaded_count;
  static PerfCounter*  _shared_classbytes_loaded;
  static PerfCounter*  _shared_classbytes_unloaded;

  static PerfVariable* _class_methods_size;

  static size_t compute_class_size(InstanceKlass* k);

public:
  static void init();

  static void reset_trace_class_unloading() { };

  static jlong loaded_class_count() {
    return _classes_loaded_count->get_value() + _shared_classes_loaded_count->get_value();
  }
  static jlong unloaded_class_count() {
    return _classes_unloaded_count->get_value() + _shared_classes_unloaded_count->get_value();
  }
  static jlong loaded_class_bytes() {
    if (UsePerfData) {
      return _classbytes_loaded->get_value() + _shared_classbytes_loaded->get_value();
    } else {
      return -1;
    }
  }
  static jlong unloaded_class_bytes() {
    if (UsePerfData) {
      return _classbytes_unloaded->get_value() + _shared_classbytes_unloaded->get_value();
    } else {
      return -1;
    }
  }

  static jlong loaded_shared_class_count() {
    return _shared_classes_loaded_count->get_value();
  }
  static jlong unloaded_shared_class_count() {
    return _shared_classes_unloaded_count->get_value();
  }
  static jlong loaded_shared_class_bytes() {
    if (UsePerfData) {
      return _shared_classbytes_loaded->get_value();
    } else {
      return -1;
    }
  }
  static jlong unloaded_shared_class_bytes() {
    if (UsePerfData) {
      return _shared_classbytes_unloaded->get_value();
    } else {
      return -1;
    }
  }
  static jlong class_method_data_size() {
    return (UsePerfData ? _class_methods_size->get_value() : -1);
  }

  static void notify_class_loaded(InstanceKlass* k, bool shared_class) { };
  // All unloaded classes are non-shared
  static void notify_class_unloaded(InstanceKlass* k) { };
  static void add_class_method_size(int size) { }
};

#endif
