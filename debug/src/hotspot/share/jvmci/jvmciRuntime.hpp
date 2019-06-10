#ifndef SHARE_VM_JVMCI_JVMCI_RUNTIME_HPP
#define SHARE_VM_JVMCI_JVMCI_RUNTIME_HPP

#include "memory/allocation.hpp"
#include "runtime/arguments.hpp"

#define JVMCI_ERROR(...) \
  { Exceptions::fthrow(THREAD_AND_LOCATION, vmSymbols::jdk_vm_ci_common_JVMCIError(), __VA_ARGS__); return; }

#define JVMCI_ERROR_(ret, ...) \
  { Exceptions::fthrow(THREAD_AND_LOCATION, vmSymbols::jdk_vm_ci_common_JVMCIError(), __VA_ARGS__); return ret; }

#define JVMCI_ERROR_0(...)    JVMCI_ERROR_(0, __VA_ARGS__)
#define JVMCI_ERROR_NULL(...) JVMCI_ERROR_(NULL, __VA_ARGS__)
#define JVMCI_ERROR_OK(...)   JVMCI_ERROR_(JVMCIEnv::ok, __VA_ARGS__)
#define CHECK_OK              CHECK_(JVMCIEnv::ok)

class JVMCIRuntime: public AllStatic {
 public:
  // Constants describing whether JVMCI wants to be able to adjust the compilation
  // level selected for a method by the VM compilation policy and if so, based on
  // what information about the method being schedule for compilation.
  enum CompLevelAdjustment {
     none = 0,             // no adjustment
     by_holder = 1,        // adjust based on declaring class of method
     by_full_signature = 2 // adjust based on declaring class, name and signature of method
  };

 private:
  static jobject _HotSpotJVMCIRuntime_instance;
  static bool _HotSpotJVMCIRuntime_initialized;
  static bool _well_known_classes_initialized;

  static CompLevelAdjustment _comp_level_adjustment;

  static bool _shutdown_called;

 public:
  static bool is_HotSpotJVMCIRuntime_initialized() {
    return _HotSpotJVMCIRuntime_initialized;
  }

  /**
   * Gets the singleton HotSpotJVMCIRuntime instance, initializing it if necessary
   */
  static Handle get_HotSpotJVMCIRuntime(TRAPS);

  static jobject get_HotSpotJVMCIRuntime_jobject(TRAPS) {
    initialize_JVMCI(CHECK_NULL);
    return _HotSpotJVMCIRuntime_instance;
  }

  static Handle callStatic(const char* className, const char* methodName, const char* returnType, JavaCallArguments* args, TRAPS);

  /**
   * Determines if the VM is sufficiently booted to initialize JVMCI.
   */
  static bool can_initialize_JVMCI();

  /**
   * Trigger initialization of HotSpotJVMCIRuntime through JVMCI.getRuntime()
   */
  static void initialize_JVMCI(TRAPS);

  /**
   * Explicitly initialize HotSpotJVMCIRuntime itself
   */
  static void initialize_HotSpotJVMCIRuntime(TRAPS);

  static void initialize_well_known_classes(TRAPS);

  static void metadata_do(void f(Metadata*));

  static void shutdown(TRAPS);

  static void bootstrap_finished(TRAPS);

  static bool shutdown_called() {
    return _shutdown_called;
  }

  static BasicType kindToBasicType(Handle kind, TRAPS);

  // The following routines are all called from compiled JVMCI code

  static void new_instance(JavaThread* thread, Klass* klass);
  static void new_array(JavaThread* thread, Klass* klass, jint length);
  static void new_multi_array(JavaThread* thread, Klass* klass, int rank, jint* dims);
  static void dynamic_new_array(JavaThread* thread, oopDesc* element_mirror, jint length);
  static void dynamic_new_instance(JavaThread* thread, oopDesc* type_mirror);
  static jboolean thread_is_interrupted(JavaThread* thread, oopDesc* obj, jboolean clear_interrupted);
  static void vm_message(jboolean vmError, jlong format, jlong v1, jlong v2, jlong v3);
  static jint identity_hash_code(JavaThread* thread, oopDesc* obj);
  static address exception_handler_for_pc(JavaThread* thread);
  static void monitorenter(JavaThread* thread, oopDesc* obj, BasicLock* lock);
  static void monitorexit (JavaThread* thread, oopDesc* obj, BasicLock* lock);
  static jboolean object_notify(JavaThread* thread, oopDesc* obj);
  static jboolean object_notifyAll(JavaThread* thread, oopDesc* obj);
  static void vm_error(JavaThread* thread, jlong where, jlong format, jlong value);
  static oopDesc* load_and_clear_exception(JavaThread* thread);
  static void log_printf(JavaThread* thread, const char* format, jlong v1, jlong v2, jlong v3);
  static void log_primitive(JavaThread* thread, jchar typeChar, jlong value, jboolean newline);
  // Print the passed in object, optionally followed by a newline.  If
  // as_string is true and the object is a java.lang.String then it
  // printed as a string, otherwise the type of the object is printed
  // followed by its address.
  static void log_object(JavaThread* thread, oopDesc* object, bool as_string, bool newline);
  static void write_barrier_pre(JavaThread* thread, oopDesc* obj);
  static void write_barrier_post(JavaThread* thread, void* card);
  static jboolean validate_object(JavaThread* thread, oopDesc* parent, oopDesc* child);

  // used to throw exceptions from compiled JVMCI code
  static void throw_and_post_jvmti_exception(JavaThread* thread, const char* exception, const char* message);
  // helper methods to throw exception with complex messages
  static void throw_klass_external_name_exception(JavaThread* thread, const char* exception, Klass* klass);
  static void throw_class_cast_exception(JavaThread* thread, const char* exception, Klass* caster_klass, Klass* target_klass);

  // Forces initialization of the JVMCI runtime.
  static void force_initialization(TRAPS);
};

#endif
