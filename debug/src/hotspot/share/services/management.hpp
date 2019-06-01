#ifndef SHARE_VM_SERVICES_MANAGEMENT_HPP
#define SHARE_VM_SERVICES_MANAGEMENT_HPP

#include "memory/allocation.hpp"
#include "runtime/handles.hpp"
#include "runtime/timer.hpp"

class OopClosure;
class ThreadSnapshot;

class Management : public AllStatic {
private:
  static PerfVariable*      _begin_vm_creation_time;
  static PerfVariable*      _end_vm_creation_time;
  static PerfVariable*      _vm_init_done_time;
  static jmmOptionalSupport _optional_support;
  static TimeStamp          _stamp; // Timestamp since vm init done time

  // Management klasses
  static InstanceKlass*     _diagnosticCommandImpl_klass;
  static InstanceKlass*     _garbageCollectorExtImpl_klass;
  static InstanceKlass*     _garbageCollectorMXBean_klass;
  static InstanceKlass*     _gcInfo_klass;
  static InstanceKlass*     _managementFactoryHelper_klass;
  static InstanceKlass*     _memoryManagerMXBean_klass;
  static InstanceKlass*     _memoryPoolMXBean_klass;
  static InstanceKlass*     _memoryUsage_klass;
  static InstanceKlass*     _sensor_klass;
  static InstanceKlass*     _threadInfo_klass;
  static InstanceKlass* load_and_initialize_klass(Symbol* sh, TRAPS);
  static InstanceKlass* load_and_initialize_klass_or_null(Symbol* sh, TRAPS);
  static InstanceKlass* initialize_klass(Klass* k, TRAPS);

public:
  static void init();
  static void initialize(TRAPS);

  static jlong ticks_to_ms(jlong ticks) { return 0L; };
  static jlong timestamp() { return 0L; };

  static void  oops_do(OopClosure* f) { };
  static void* get_jmm_interface(int version);
  static void  get_optional_support(jmmOptionalSupport* support);

  static void get_loaded_classes(JavaThread* cur_thread, GrowableArray<Klass*>* klass_array);

  static void  record_vm_startup_time(jlong begin, jlong duration)
      { };
  static void  record_vm_init_completed() {
    // Initialize the timestamp to get the current time
    _vm_init_done_time->set_value(os::javaTimeMillis());

    // Update the timestamp to the vm init done time
    _stamp.update();
  }

  static jlong begin_vm_creation_time() {
    return _begin_vm_creation_time->get_value();
  }
  static jlong vm_init_done_time() {
    return _vm_init_done_time->get_value();
  }

  // methods to return a Klass*.
  static InstanceKlass* java_lang_management_ThreadInfo_klass(TRAPS);
  static InstanceKlass* java_lang_management_MemoryUsage_klass(TRAPS)
      { return NULL; };
  static InstanceKlass* java_lang_management_MemoryPoolMXBean_klass(TRAPS);
  static InstanceKlass* java_lang_management_MemoryManagerMXBean_klass(TRAPS);
  static InstanceKlass* java_lang_management_GarbageCollectorMXBean_klass(TRAPS);
  static InstanceKlass* sun_management_ManagementFactoryHelper_klass(TRAPS)
      { return NULL; };
  static InstanceKlass* sun_management_Sensor_klass(TRAPS)
      { return NULL; };
  static InstanceKlass* com_sun_management_internal_GarbageCollectorExtImpl_klass(TRAPS)
      { return NULL; };
  static InstanceKlass* com_sun_management_GcInfo_klass(TRAPS)
      { return NULL; };
  static InstanceKlass* com_sun_management_internal_DiagnosticCommandImpl_klass(TRAPS)
      { return NULL; };
};

class TraceVmCreationTime : public StackObj {
private:
  TimeStamp _timer;
  jlong     _begin_time;

public:
  TraceVmCreationTime() { }
  ~TraceVmCreationTime() { }

  void start()
  { _timer.update_to(0); _begin_time = os::javaTimeMillis(); }

  jlong begin_time() const {
    return _begin_time;
  }

  /**
   * Only call this if initialization completes successfully; it will
   * crash if PerfMemory_exit() has already been called (usually by
   * os::shutdown() when there was an initialization failure).
   */
  void end()
  { Management::record_vm_startup_time(_begin_time, _timer.milliseconds()); }
};

#endif
