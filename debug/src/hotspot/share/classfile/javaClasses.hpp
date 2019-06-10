#ifndef SHARE_VM_CLASSFILE_JAVACLASSES_HPP
#define SHARE_VM_CLASSFILE_JAVACLASSES_HPP

#include "classfile/systemDictionary.hpp"
#include "oops/oop.hpp"
#include "runtime/os.hpp"
#include "utilities/utf8.hpp"

// Interface for manipulating the basic Java classes.
//
// All dependencies on layout of actual Java classes should be kept here.
// If the layout of any of the classes above changes the offsets must be adjusted.
//
// For most classes we hardwire the offsets for performance reasons. In certain
// cases (e.g. java.security.AccessControlContext) we compute the offsets at
// startup since the layout here differs between JDK1.2 and JDK1.3.
//
// Note that fields (static and non-static) are arranged with oops before non-oops
// on a per class basis. The offsets below have to reflect this ordering.
//
// When editing the layouts please update the check_offset verification code
// correspondingly. The names in the enums must be identical to the actual field
// names in order for the verification code to work.

// Interface to java.lang.String objects

class java_lang_String : AllStatic {
 private:
  static int value_offset;
  static int hash_offset;
  static int coder_offset;

  static bool initialized;

  static Handle basic_create(int length, bool byte_arr, TRAPS);

  static inline void set_coder(oop string, jbyte coder);

 public:
  // Coders
  enum Coder {
    CODER_LATIN1 =  0,
    CODER_UTF16  =  1
  };

  static void compute_offsets();
  static void serialize(SerializeClosure* f) { };

  // Instance creation
  static Handle create_from_unicode(jchar* unicode, int len, TRAPS);
  static oop    create_oop_from_unicode(jchar* unicode, int len, TRAPS);
  static Handle create_from_str(const char* utf8_str, TRAPS);
  static oop    create_oop_from_str(const char* utf8_str, TRAPS);
  static Handle create_from_symbol(Symbol* symbol, TRAPS);
  static Handle create_from_platform_dependent_str(const char* str, TRAPS);
  static Handle char_converter(Handle java_string, jchar from_char, jchar to_char, TRAPS);

  static void set_compact_strings(bool value);

  static int value_offset_in_bytes() { return value_offset; }
  static int hash_offset_in_bytes()  { return hash_offset; }
  static int coder_offset_in_bytes() { return coder_offset; }

  static inline void set_value_raw(oop string, typeArrayOop buffer);
  static inline void set_value(oop string, typeArrayOop buffer);
  static inline void set_hash(oop string, unsigned int hash);

  // Accessors
  static inline typeArrayOop value(oop java_string);
  static inline typeArrayOop value_no_keepalive(oop java_string);
  static inline unsigned int hash(oop java_string);
  static inline bool is_latin1(oop java_string);
  static inline int length(oop java_string);
  static int utf8_length(oop java_string);

  // String converters
  static char*  as_utf8_string(oop java_string);
  static char*  as_utf8_string(oop java_string, char* buf, int buflen);
  static char*  as_utf8_string(oop java_string, int start, int len);
  static char*  as_utf8_string(oop java_string, int start, int len, char* buf, int buflen);
  static char*  as_platform_dependent_str(Handle java_string, TRAPS);
  static jchar* as_unicode_string(oop java_string, int& length, TRAPS);
  // produce an ascii string with all other values quoted using \u####
  static char*  as_quoted_ascii(oop java_string);

  // Compute the hash value for a java.lang.String object which would
  // contain the characters passed in.
  //
  // As the hash value used by the String object itself, in
  // String.hashCode().  This value is normally calculated in Java code
  // in the String.hashCode method(), but is precomputed for String
  // objects in the shared archive file.
  // hash P(31) from Kernighan & Ritchie
  //
  // For this reason, THIS ALGORITHM MUST MATCH String.hashCode().
  static unsigned int hash_code(const jchar* s, int len) {
    unsigned int h = 0;
    while (len-- > 0) {
      h = 31*h + (unsigned int) *s;
      s++;
    }
    return h;
  }

  static unsigned int hash_code(const jbyte* s, int len) {
    unsigned int h = 0;
    while (len-- > 0) {
      h = 31*h + (((unsigned int) *s) & 0xFF);
      s++;
    }
    return h;
  }

  static unsigned int hash_code(oop java_string);

  static bool equals(oop java_string, jchar* chars, int len);
  static bool equals(oop str1, oop str2);

  // Conversion between '.' and '/' formats
  static Handle externalize_classname(Handle java_string, TRAPS) { return char_converter(java_string, '/', '.', THREAD); }
  static Handle internalize_classname(Handle java_string, TRAPS) { return char_converter(java_string, '.', '/', THREAD); }

  // Conversion
  static Symbol* as_symbol(oop java_string, TRAPS);
  static Symbol* as_symbol_or_null(oop java_string);

  // Testers
  static bool is_instance(oop obj);
  static inline bool is_instance_inlined(oop obj);

  // Debugging
  static void print(oop java_string, outputStream* st);
  friend class JavaClasses;
  friend class StringTable;
};

// Interface to java.lang.Class objects

#define CLASS_INJECTED_FIELDS(macro) \
  macro(java_lang_Class, klass,                  intptr_signature,  false) \
  macro(java_lang_Class, array_klass,            intptr_signature,  false) \
  macro(java_lang_Class, oop_size,               int_signature,     false) \
  macro(java_lang_Class, static_oop_field_count, int_signature,     false) \
  macro(java_lang_Class, protection_domain,      object_signature,  false) \
  macro(java_lang_Class, signers,                object_signature,  false) \
  macro(java_lang_Class, source_file,            object_signature,  false)

class java_lang_Class : AllStatic {
  friend class VMStructs;
  friend class JVMCIVMStructs;

 private:
  // The fake offsets are added by the class loader when java.lang.Class is loaded

  static int _klass_offset;
  static int _array_klass_offset;

  static int _oop_size_offset;
  static int _static_oop_field_count_offset;

  static int _protection_domain_offset;
  static int _init_lock_offset;
  static int _signers_offset;
  static int _class_loader_offset;
  static int _module_offset;
  static int _component_mirror_offset;
  static int _name_offset;
  static int _source_file_offset;

  static bool offsets_computed;
  static int classRedefinedCount_offset;

  static GrowableArray<Klass*>* _fixup_mirror_list;
  static GrowableArray<Klass*>* _fixup_module_field_list;

  static void set_init_lock(oop java_class, oop init_lock);
  static void set_protection_domain(oop java_class, oop protection_domain);
  static void set_class_loader(oop java_class, oop class_loader);
  static void set_component_mirror(oop java_class, oop comp_mirror);
  static void initialize_mirror_fields(Klass* k, Handle mirror, Handle protection_domain, TRAPS);
  static void set_mirror_module_field(Klass* K, Handle mirror, Handle module, TRAPS);
 public:
  static void allocate_fixup_lists();
  static void compute_offsets();

  // Instance creation
  static void create_mirror(Klass* k, Handle class_loader, Handle module, Handle protection_domain, TRAPS);
  static void fixup_mirror(Klass* k, TRAPS);
  static oop  create_basic_type_mirror(const char* basic_type_name, BasicType type, TRAPS);

  // Archiving
  static void serialize(SerializeClosure* f) { };
  static void archive_basic_type_mirrors(TRAPS) { };
  static oop  archive_mirror(Klass* k, TRAPS) { return NULL; };
  static oop  process_archived_mirror(Klass* k, oop mirror, oop archived_mirror, Thread *THREAD) { return NULL; };
  static bool restore_archived_mirror(Klass *k, Handle class_loader, Handle module, Handle protection_domain, TRAPS) { return false; };

  static void fixup_module_field(Klass* k, Handle module);

  // Conversion
  static Klass* as_Klass(oop java_class);
  static void set_klass(oop java_class, Klass* klass);
  static BasicType as_BasicType(oop java_class, Klass** reference_klass = NULL);
  static Symbol* as_signature(oop java_class, bool intern_if_not_found, TRAPS);
  static void print_signature(oop java_class, outputStream* st);
  static const char* as_external_name(oop java_class);
  // Testing
  static bool is_instance(oop obj);

  static bool is_primitive(oop java_class);
  static BasicType primitive_type(oop java_class);
  static oop primitive_mirror(BasicType t);
  // JVM_NewArray support
  static Klass* array_klass_acquire(oop java_class);
  static void release_set_array_klass(oop java_class, Klass* klass);
  // compiler support for class operations
  static int klass_offset_in_bytes()                { return _klass_offset; }
  static int array_klass_offset_in_bytes()          { return _array_klass_offset; }
  // Support for classRedefinedCount field
  static int classRedefinedCount(oop the_class_mirror);
  static void set_classRedefinedCount(oop the_class_mirror, int value);

  // Support for embedded per-class oops
  static oop  protection_domain(oop java_class);
  static oop  init_lock(oop java_class);
  static oop  component_mirror(oop java_class);
  static objArrayOop  signers(oop java_class);
  static void set_signers(oop java_class, objArrayOop signers);

  static oop class_loader(oop java_class);
  static void set_module(oop java_class, oop module);
  static oop module(oop java_class);

  static oop name(Handle java_class, TRAPS);

  static oop source_file(oop java_class);
  static void set_source_file(oop java_class, oop source_file);

  static int oop_size(oop java_class);
  static void set_oop_size(HeapWord* java_class, int size);
  static int static_oop_field_count(oop java_class);
  static void set_static_oop_field_count(oop java_class, int size);

  static GrowableArray<Klass*>* fixup_mirror_list() {
    return _fixup_mirror_list;
  }
  static void set_fixup_mirror_list(GrowableArray<Klass*>* v) {
    _fixup_mirror_list = v;
  }

  static GrowableArray<Klass*>* fixup_module_field_list() {
    return _fixup_module_field_list;
  }
  static void set_fixup_module_field_list(GrowableArray<Klass*>* v) {
    _fixup_module_field_list = v;
  }

  // Debugging
  friend class JavaClasses;
  friend class InstanceKlass;   // verification code accesses offsets
  friend class ClassFileParser; // access to number_of_fake_fields
};

// Interface to java.lang.Thread objects

class java_lang_Thread : AllStatic {
 private:
  // Note that for this class the layout changed between JDK1.2 and JDK1.3,
  // so we compute the offsets at startup rather than hard-wiring them.
  static int _name_offset;
  static int _group_offset;
  static int _contextClassLoader_offset;
  static int _inheritedAccessControlContext_offset;
  static int _priority_offset;
  static int _eetop_offset;
  static int _daemon_offset;
  static int _stillborn_offset;
  static int _stackSize_offset;
  static int _tid_offset;
  static int _thread_status_offset;
  static int _park_blocker_offset;
  static int _park_event_offset;

  static void compute_offsets();

 public:
  static void serialize(SerializeClosure* f) { };

  // Instance creation
  static oop create();
  // Returns the JavaThread associated with the thread obj
  static JavaThread* thread(oop java_thread);
  // Set JavaThread for instance
  static void set_thread(oop java_thread, JavaThread* thread);
  // Name
  static oop name(oop java_thread);
  static void set_name(oop java_thread, oop name);
  // Priority
  static ThreadPriority priority(oop java_thread);
  static void set_priority(oop java_thread, ThreadPriority priority);
  // Thread group
  static oop  threadGroup(oop java_thread);
  // Stillborn
  static bool is_stillborn(oop java_thread);
  static void set_stillborn(oop java_thread);
  // Alive (NOTE: this is not really a field, but provides the correct
  // definition without doing a Java call)
  static bool is_alive(oop java_thread);
  // Daemon
  static bool is_daemon(oop java_thread);
  static void set_daemon(oop java_thread);
  // Context ClassLoader
  static oop context_class_loader(oop java_thread);
  // Control context
  static oop inherited_access_control_context(oop java_thread);
  // Stack size hint
  static jlong stackSize(oop java_thread);
  // Thread ID
  static jlong thread_id(oop java_thread);

  // Blocker object responsible for thread parking
  static oop park_blocker(oop java_thread);

  // Pointer to type-stable park handler, encoded as jlong.
  // Should be set when apparently null
  // For details, see unsafe.cpp Unsafe_Unpark
  static jlong park_event(oop java_thread);
  static bool set_park_event(oop java_thread, jlong ptr);

#define JVMTI_THREAD_STATE_ALIVE                    0x0001
#define JVMTI_THREAD_STATE_TERMINATED               0x0002
#define JVMTI_THREAD_STATE_RUNNABLE                 0x0004
#define JVMTI_THREAD_STATE_WAITING_INDEFINITELY     0x0010
#define JVMTI_THREAD_STATE_WAITING_WITH_TIMEOUT     0x0020
#define JVMTI_THREAD_STATE_SLEEPING                 0x0040
#define JVMTI_THREAD_STATE_WAITING                  0x0080
#define JVMTI_THREAD_STATE_IN_OBJECT_WAIT           0x0100
#define JVMTI_THREAD_STATE_PARKED                   0x0200
#define JVMTI_THREAD_STATE_BLOCKED_ON_MONITOR_ENTER 0x0400

#define JVMTI_THREAD_STATE_SUSPENDED                0x100000
#define JVMTI_THREAD_STATE_INTERRUPTED              0x200000
#define JVMTI_THREAD_STATE_IN_NATIVE                0x400000

  // Java Thread Status for JVMTI and M&M use.
  // This thread status info is saved in threadStatus field of
  // java.lang.Thread java class.
  enum ThreadStatus {
    NEW                      = 0,
    RUNNABLE                 = JVMTI_THREAD_STATE_ALIVE +          // runnable / running
                               JVMTI_THREAD_STATE_RUNNABLE,
    SLEEPING                 = JVMTI_THREAD_STATE_ALIVE +          // Thread.sleep()
                               JVMTI_THREAD_STATE_WAITING +
                               JVMTI_THREAD_STATE_WAITING_WITH_TIMEOUT +
                               JVMTI_THREAD_STATE_SLEEPING,
    IN_OBJECT_WAIT           = JVMTI_THREAD_STATE_ALIVE +          // Object.wait()
                               JVMTI_THREAD_STATE_WAITING +
                               JVMTI_THREAD_STATE_WAITING_INDEFINITELY +
                               JVMTI_THREAD_STATE_IN_OBJECT_WAIT,
    IN_OBJECT_WAIT_TIMED     = JVMTI_THREAD_STATE_ALIVE +          // Object.wait(long)
                               JVMTI_THREAD_STATE_WAITING +
                               JVMTI_THREAD_STATE_WAITING_WITH_TIMEOUT +
                               JVMTI_THREAD_STATE_IN_OBJECT_WAIT,
    PARKED                   = JVMTI_THREAD_STATE_ALIVE +          // LockSupport.park()
                               JVMTI_THREAD_STATE_WAITING +
                               JVMTI_THREAD_STATE_WAITING_INDEFINITELY +
                               JVMTI_THREAD_STATE_PARKED,
    PARKED_TIMED             = JVMTI_THREAD_STATE_ALIVE +          // LockSupport.park(long)
                               JVMTI_THREAD_STATE_WAITING +
                               JVMTI_THREAD_STATE_WAITING_WITH_TIMEOUT +
                               JVMTI_THREAD_STATE_PARKED,
    BLOCKED_ON_MONITOR_ENTER = JVMTI_THREAD_STATE_ALIVE +          // (re-)entering a synchronization block
                               JVMTI_THREAD_STATE_BLOCKED_ON_MONITOR_ENTER,
    TERMINATED               = JVMTI_THREAD_STATE_TERMINATED
  };
  // Write thread status info to threadStatus field of java.lang.Thread.
  static void set_thread_status(oop java_thread_oop, ThreadStatus status);
  // Read thread status info from threadStatus field of java.lang.Thread.
  static ThreadStatus get_thread_status(oop java_thread_oop);

  static const char*  thread_status_name(oop java_thread_oop);

  // Debugging
  friend class JavaClasses;
};

// Interface to java.lang.ThreadGroup objects

class java_lang_ThreadGroup : AllStatic {
 private:
  static int _parent_offset;
  static int _name_offset;
  static int _threads_offset;
  static int _groups_offset;
  static int _maxPriority_offset;
  static int _destroyed_offset;
  static int _daemon_offset;
  static int _nthreads_offset;
  static int _ngroups_offset;

  static void compute_offsets();

 public:
  static void serialize(SerializeClosure* f) { };

  // parent ThreadGroup
  static oop  parent(oop java_thread_group);
  // name
  static const char* name(oop java_thread_group);
  // ("name as oop" accessor is not necessary)
  // Number of threads in group
  static int nthreads(oop java_thread_group);
  // threads
  static objArrayOop threads(oop java_thread_group);
  // Number of threads in group
  static int ngroups(oop java_thread_group);
  // groups
  static objArrayOop groups(oop java_thread_group);
  // maxPriority in group
  static ThreadPriority maxPriority(oop java_thread_group);
  // Destroyed
  static bool is_destroyed(oop java_thread_group);
  // Daemon
  static bool is_daemon(oop java_thread_group);
  // Debugging
  friend class JavaClasses;
};

// Interface to java.lang.Throwable objects

class java_lang_Throwable: AllStatic {
  friend class BacktraceBuilder;
  friend class BacktraceIterator;

 private:
  // Offsets
  enum {
    hc_backtrace_offset     =  0,
    hc_detailMessage_offset =  1,
    hc_cause_offset         =  2,  // New since 1.4
    hc_stackTrace_offset    =  3   // New since 1.4
  };
  // Trace constants
  enum {
    trace_methods_offset = 0,
    trace_bcis_offset    = 1,
    trace_mirrors_offset = 2,
    trace_names_offset   = 3,
    trace_next_offset    = 4,
    trace_size           = 5,
    trace_chunk_size     = 32
  };

  static int backtrace_offset;
  static int detailMessage_offset;
  static int stackTrace_offset;
  static int depth_offset;
  static int static_unassigned_stacktrace_offset;

  // StackTrace (programmatic access, new since 1.4)
  static void clear_stacktrace(oop throwable);
  // Stacktrace (post JDK 1.7.0 to allow immutability protocol to be followed)
  static void set_stacktrace(oop throwable, oop st_element_array);
  static oop unassigned_stacktrace();

 public:
  // Backtrace
  static oop backtrace(oop throwable);
  static void set_backtrace(oop throwable, oop value);
  static int depth(oop throwable);
  static void set_depth(oop throwable, int value);
  // Message
  static oop message(oop throwable);
  static void set_message(oop throwable, oop value);
  static Symbol* detail_message(oop throwable);
  static void print_stack_element(outputStream* st, const methodHandle& method, int bci);
  static void print_stack_usage(Handle stream);

  static void compute_offsets();
  static void serialize(SerializeClosure* f) { };

  // Allocate space for backtrace (created but stack trace not filled in)
  static void allocate_backtrace(Handle throwable, TRAPS);
  // Fill in current stack trace for throwable with preallocated backtrace (no GC)
  static void fill_in_stack_trace_of_preallocated_backtrace(Handle throwable);
  // Fill in current stack trace, can cause GC
  static void fill_in_stack_trace(Handle throwable, const methodHandle& method, TRAPS);
  static void fill_in_stack_trace(Handle throwable, const methodHandle& method = methodHandle());
  // Printing
  static void print(oop throwable, outputStream* st);
  static void print_stack_trace(Handle throwable, outputStream* st);
  static void java_printStackTrace(Handle throwable, TRAPS);
  // Debugging
  friend class JavaClasses;
};

// Interface to java.lang.reflect.AccessibleObject objects

class java_lang_reflect_AccessibleObject: AllStatic {
 private:
  // Note that to reduce dependencies on the JDK we compute these
  // offsets at run-time.
  static int override_offset;

  static void compute_offsets();

 public:
  static void serialize(SerializeClosure* f) { };

  // Accessors
  static jboolean override(oop reflect);
  static void set_override(oop reflect, jboolean value);

  // Debugging
  friend class JavaClasses;
};

// Interface to java.lang.reflect.Method objects

class java_lang_reflect_Method : public java_lang_reflect_AccessibleObject {
 private:
  // Note that to reduce dependencies on the JDK we compute these
  // offsets at run-time.
  static int clazz_offset;
  static int name_offset;
  static int returnType_offset;
  static int parameterTypes_offset;
  static int exceptionTypes_offset;
  static int slot_offset;
  static int modifiers_offset;
  static int signature_offset;
  static int annotations_offset;
  static int parameter_annotations_offset;
  static int annotation_default_offset;
  static int type_annotations_offset;

  static void compute_offsets();

 public:
  static void serialize(SerializeClosure* f) { };

  // Allocation
  static Handle create(TRAPS);

  // Accessors
  static oop clazz(oop reflect);
  static void set_clazz(oop reflect, oop value);

  static oop name(oop method);
  static void set_name(oop method, oop value);

  static oop return_type(oop method);
  static void set_return_type(oop method, oop value);

  static oop parameter_types(oop method);
  static void set_parameter_types(oop method, oop value);

  static oop exception_types(oop method);
  static void set_exception_types(oop method, oop value);

  static int slot(oop reflect);
  static void set_slot(oop reflect, int value);

  static int modifiers(oop method);
  static void set_modifiers(oop method, int value);

  static bool has_signature_field();
  static oop signature(oop method);
  static void set_signature(oop method, oop value);

  static bool has_annotations_field();
  static oop annotations(oop method);
  static void set_annotations(oop method, oop value);

  static bool has_parameter_annotations_field();
  static oop parameter_annotations(oop method);
  static void set_parameter_annotations(oop method, oop value);

  static bool has_annotation_default_field();
  static oop annotation_default(oop method);
  static void set_annotation_default(oop method, oop value);

  static bool has_type_annotations_field();
  static oop type_annotations(oop method);
  static void set_type_annotations(oop method, oop value);

  // Debugging
  friend class JavaClasses;
};

// Interface to java.lang.reflect.Constructor objects

class java_lang_reflect_Constructor : public java_lang_reflect_AccessibleObject {
 private:
  // Note that to reduce dependencies on the JDK we compute these
  // offsets at run-time.
  static int clazz_offset;
  static int parameterTypes_offset;
  static int exceptionTypes_offset;
  static int slot_offset;
  static int modifiers_offset;
  static int signature_offset;
  static int annotations_offset;
  static int parameter_annotations_offset;
  static int type_annotations_offset;

  static void compute_offsets();

 public:
  static void serialize(SerializeClosure* f) { };

  // Allocation
  static Handle create(TRAPS);

  // Accessors
  static oop clazz(oop reflect);
  static void set_clazz(oop reflect, oop value);

  static oop parameter_types(oop constructor);
  static void set_parameter_types(oop constructor, oop value);

  static oop exception_types(oop constructor);
  static void set_exception_types(oop constructor, oop value);

  static int slot(oop reflect);
  static void set_slot(oop reflect, int value);

  static int modifiers(oop constructor);
  static void set_modifiers(oop constructor, int value);

  static bool has_signature_field();
  static oop signature(oop constructor);
  static void set_signature(oop constructor, oop value);

  static bool has_annotations_field();
  static oop annotations(oop constructor);
  static void set_annotations(oop constructor, oop value);

  static bool has_parameter_annotations_field();
  static oop parameter_annotations(oop method);
  static void set_parameter_annotations(oop method, oop value);

  static bool has_type_annotations_field();
  static oop type_annotations(oop constructor);
  static void set_type_annotations(oop constructor, oop value);

  // Debugging
  friend class JavaClasses;
};

// Interface to java.lang.reflect.Field objects

class java_lang_reflect_Field : public java_lang_reflect_AccessibleObject {
 private:
  // Note that to reduce dependencies on the JDK we compute these
  // offsets at run-time.
  static int clazz_offset;
  static int name_offset;
  static int type_offset;
  static int slot_offset;
  static int modifiers_offset;
  static int signature_offset;
  static int annotations_offset;
  static int type_annotations_offset;

  static void compute_offsets();

 public:
  static void serialize(SerializeClosure* f) { };

  // Allocation
  static Handle create(TRAPS);

  // Accessors
  static oop clazz(oop reflect);
  static void set_clazz(oop reflect, oop value);

  static oop name(oop field);
  static void set_name(oop field, oop value);

  static oop type(oop field);
  static void set_type(oop field, oop value);

  static int slot(oop reflect);
  static void set_slot(oop reflect, int value);

  static int modifiers(oop field);
  static void set_modifiers(oop field, int value);

  static bool has_signature_field();
  static oop signature(oop constructor);
  static void set_signature(oop constructor, oop value);

  static bool has_annotations_field();
  static oop annotations(oop constructor);
  static void set_annotations(oop constructor, oop value);

  static bool has_parameter_annotations_field();
  static oop parameter_annotations(oop method);
  static void set_parameter_annotations(oop method, oop value);

  static bool has_annotation_default_field();
  static oop annotation_default(oop method);
  static void set_annotation_default(oop method, oop value);

  static bool has_type_annotations_field();
  static oop type_annotations(oop field);
  static void set_type_annotations(oop field, oop value);

  // Debugging
  friend class JavaClasses;
};

class java_lang_reflect_Parameter {
 private:
  // Note that to reduce dependencies on the JDK we compute these
  // offsets at run-time.
  static int name_offset;
  static int modifiers_offset;
  static int index_offset;
  static int executable_offset;

  static void compute_offsets();

 public:
  static void serialize(SerializeClosure* f) { };

  // Allocation
  static Handle create(TRAPS);

  // Accessors
  static oop name(oop field);
  static void set_name(oop field, oop value);

  static int index(oop reflect);
  static void set_index(oop reflect, int value);

  static int modifiers(oop reflect);
  static void set_modifiers(oop reflect, int value);

  static oop executable(oop constructor);
  static void set_executable(oop constructor, oop value);

  friend class JavaClasses;
};

#define MODULE_INJECTED_FIELDS(macro) \
  macro(java_lang_Module, module_entry, intptr_signature, false)

class java_lang_Module {
  private:
    static int loader_offset;
    static int name_offset;
    static int _module_entry_offset;
    static void compute_offsets();

  public:
    static void serialize(SerializeClosure* f) { };

    // Allocation
    static Handle create(Handle loader, Handle module_name, TRAPS);

    // Testers
    static bool is_instance(oop obj);

    // Accessors
    static oop loader(oop module);
    static void set_loader(oop module, oop value);

    static oop name(oop module);
    static void set_name(oop module, oop value);

    static ModuleEntry* module_entry(oop module);
    static void set_module_entry(oop module, ModuleEntry* module_entry);

  friend class JavaClasses;
};

// Interface to jdk.internal.reflect.ConstantPool objects
class reflect_ConstantPool {
 private:
  // Note that to reduce dependencies on the JDK we compute these
  // offsets at run-time.
  static int _oop_offset;

  static void compute_offsets();

 public:
  static void serialize(SerializeClosure* f) { };

  // Allocation
  static Handle create(TRAPS);

  // Accessors
  static void set_cp(oop reflect, ConstantPool* value);
  static int oop_offset() {
    return _oop_offset;
  }

  static ConstantPool* get_cp(oop reflect);

  // Debugging
  friend class JavaClasses;
};

// Interface to jdk.internal.reflect.UnsafeStaticFieldAccessorImpl objects
class reflect_UnsafeStaticFieldAccessorImpl {
 private:
  static int _base_offset;
  static void compute_offsets();

 public:
  static void serialize(SerializeClosure* f) { };

  static int base_offset() {
    return _base_offset;
  }

  // Debugging
  friend class JavaClasses;
};

// Interface to java.lang primitive type boxing objects:
//  - java.lang.Boolean
//  - java.lang.Character
//  - java.lang.Float
//  - java.lang.Double
//  - java.lang.Byte
//  - java.lang.Short
//  - java.lang.Integer
//  - java.lang.Long

// This could be separated out into 8 individual classes.

class java_lang_boxing_object: AllStatic {
 private:
  enum {
   hc_value_offset = 0
  };
  static int value_offset;
  static int long_value_offset;

  static oop initialize_and_allocate(BasicType type, TRAPS);
 public:
  // Allocation. Returns a boxed value, or NULL for invalid type.
  static oop create(BasicType type, jvalue* value, TRAPS);
  // Accessors. Returns the basic type being boxed, or T_ILLEGAL for invalid oop.
  static BasicType get_value(oop box, jvalue* value);
  static BasicType set_value(oop box, jvalue* value);
  static BasicType basic_type(oop box);
  static bool is_instance(oop box)                 { return basic_type(box) != T_ILLEGAL; }
  static bool is_instance(oop box, BasicType type) { return basic_type(box) == type; }
  static void print(oop box, outputStream* st)     { jvalue value;  print(get_value(box, &value), &value, st); }
  static void print(BasicType type, jvalue* value, outputStream* st);

  static int value_offset_in_bytes(BasicType type) {
    return ( type == T_LONG || type == T_DOUBLE ) ? long_value_offset : value_offset;
  }

  // Debugging
  friend class JavaClasses;
};

// Interface to java.lang.ref.Reference objects

class java_lang_ref_Reference: AllStatic {
 public:
  enum {
   hc_referent_offset   = 0,
   hc_queue_offset      = 1,
   hc_next_offset       = 2,
   hc_discovered_offset = 3  // Is not last, see SoftRefs.
  };

  static int referent_offset;
  static int queue_offset;
  static int next_offset;
  static int discovered_offset;

  // Accessors
  static inline oop referent(oop ref);
  static inline void set_referent(oop ref, oop value);
  static inline void set_referent_raw(oop ref, oop value);
  static inline HeapWord* referent_addr_raw(oop ref);
  static inline oop next(oop ref);
  static inline void set_next(oop ref, oop value);
  static inline void set_next_raw(oop ref, oop value);
  static inline HeapWord* next_addr_raw(oop ref);
  static inline oop discovered(oop ref);
  static inline void set_discovered(oop ref, oop value);
  static inline void set_discovered_raw(oop ref, oop value);
  static inline HeapWord* discovered_addr_raw(oop ref);
  static inline oop queue(oop ref);
  static inline void set_queue(oop ref, oop value);
  static bool is_referent_field(oop obj, ptrdiff_t offset);
  static inline bool is_phantom(oop ref);
};

// Interface to java.lang.ref.SoftReference objects

class java_lang_ref_SoftReference: public java_lang_ref_Reference {
 public:
  static int timestamp_offset;
  static int static_clock_offset;

  // Accessors
  static jlong timestamp(oop ref);

  // Accessors for statics
  static jlong clock();
  static void set_clock(jlong value);

  static void compute_offsets();
  static void serialize(SerializeClosure* f) { };
};

// Interface to java.security.AccessControlContext objects

class java_security_AccessControlContext: AllStatic {
 private:
  // Note that for this class the layout changed between JDK1.2 and JDK1.3,
  // so we compute the offsets at startup rather than hard-wiring them.
  static int _context_offset;
  static int _privilegedContext_offset;
  static int _isPrivileged_offset;
  static int _isAuthorized_offset;

  static void compute_offsets();
 public:
  static void serialize(SerializeClosure* f) { };
  static oop create(objArrayHandle context, bool isPrivileged, Handle privileged_context, TRAPS);

  static bool is_authorized(Handle context);

  // Debugging/initialization
  friend class JavaClasses;
};

// Interface to java.lang.ClassLoader objects

#define CLASSLOADER_INJECTED_FIELDS(macro) \
  macro(java_lang_ClassLoader, loader_data,  intptr_signature, false)

class java_lang_ClassLoader : AllStatic {
 private:
  static int _loader_data_offset;
  static bool offsets_computed;
  static int parent_offset;
  static int parallelCapable_offset;
  static int name_offset;
  static int nameAndId_offset;
  static int unnamedModule_offset;

 public:
  static void compute_offsets();
  static void serialize(SerializeClosure* f) { };

  static ClassLoaderData* loader_data(oop loader);
  static ClassLoaderData* cmpxchg_loader_data(ClassLoaderData* new_data, oop loader, ClassLoaderData* expected_data);

  static oop parent(oop loader);
  static oop name(oop loader);
  static oop nameAndId(oop loader);
  static bool isAncestor(oop loader, oop cl);

  // Support for parallelCapable field
  static bool parallelCapable(oop the_class_mirror);

  static bool is_trusted_loader(oop loader);

  // Return true if this is one of the class loaders associated with
  // the generated bytecodes for reflection.
  static bool is_reflection_class_loader(oop loader);

  // Fix for 4474172
  static oop  non_reflection_class_loader(oop loader);

  // Testers
  static bool is_subclass(Klass* klass) {
    return klass->is_subclass_of(SystemDictionary::ClassLoader_klass());
  }
  static bool is_instance(oop obj);

  static oop unnamedModule(oop loader);

  // Debugging
  friend class JavaClasses;
  friend class ClassFileParser; // access to number_of_fake_fields

  // Describe ClassLoader for exceptions, tracing ...
  // Prints "<name>" (instance of <classname>, child of "<name>" <classname>).
  // If a classloader has no name, it prints <unnamed> instead. The output
  // for well known loaders (system/platform) is abbreviated.
  static const char* describe_external(const oop loader);
};

// Interface to java.lang.System objects

class java_lang_System : AllStatic {
 private:
  static int  static_in_offset;
  static int static_out_offset;
  static int static_err_offset;
  static int static_security_offset;

 public:
  static int  in_offset_in_bytes();
  static int out_offset_in_bytes();
  static int err_offset_in_bytes();

  static bool has_security_manager();

  static void compute_offsets();
  static void serialize(SerializeClosure* f) { };

  // Debugging
  friend class JavaClasses;
};

class Backtrace: AllStatic {
 public:
  // Helper backtrace functions to store bci|version together.
  static int merge_bci_and_version(int bci, int version);
  static int merge_mid_and_cpref(int mid, int cpref);
  static int bci_at(unsigned int merged);
  static int version_at(unsigned int merged);
  static int mid_at(unsigned int merged);
  static int cpref_at(unsigned int merged);
  static int get_line_number(const methodHandle& method, int bci);
  static Symbol* get_source_file_name(InstanceKlass* holder, int version);

  // Debugging
  friend class JavaClasses;
};

// Interface to java.lang.AssertionStatusDirectives objects

class java_lang_AssertionStatusDirectives: AllStatic {
 private:
  static int classes_offset;
  static int classEnabled_offset;
  static int packages_offset;
  static int packageEnabled_offset;
  static int deflt_offset;

 public:
  // Setters
  static void set_classes(oop obj, oop val);
  static void set_classEnabled(oop obj, oop val);
  static void set_packages(oop obj, oop val);
  static void set_packageEnabled(oop obj, oop val);
  static void set_deflt(oop obj, bool val);

  static void compute_offsets();
  static void serialize(SerializeClosure* f) { };

  // Debugging
  friend class JavaClasses;
};

class java_nio_Buffer: AllStatic {
 private:
  static int _limit_offset;

 public:
  static int  limit_offset();
  static void compute_offsets();
  static void serialize(SerializeClosure* f) { };
};

class java_util_concurrent_locks_AbstractOwnableSynchronizer : AllStatic {
 private:
  static int  _owner_offset;
 public:
  static void compute_offsets();
  static oop  get_owner_threadObj(oop obj);
  static void serialize(SerializeClosure* f) { };
};

// Use to declare fields that need to be injected into Java classes
// for the JVM to use.  The name_index and signature_index are
// declared in vmSymbols.  The may_be_java flag is used to declare
// fields that might already exist in Java but should be injected if
// they don't.  Otherwise the field is unconditionally injected and
// the JVM uses the injected one.  This is to ensure that name
// collisions don't occur.  In general may_be_java should be false
// unless there's a good reason.

class InjectedField {
 public:
  const SystemDictionary::WKID klass_id;
  const vmSymbols::SID name_index;
  const vmSymbols::SID signature_index;
  const bool           may_be_java;

  Klass* klass()    const { return SystemDictionary::well_known_klass(klass_id); }
  Symbol* name()      const { return lookup_symbol(name_index); }
  Symbol* signature() const { return lookup_symbol(signature_index); }

  int compute_offset();

  // Find the Symbol for this index
  static Symbol* lookup_symbol(int symbol_index) {
    return vmSymbols::symbol_at((vmSymbols::SID)symbol_index);
  }
};

#define DECLARE_INJECTED_FIELD_ENUM(klass, name, signature, may_be_java) \
  klass##_##name##_enum,

#define ALL_INJECTED_FIELDS(macro) \
  CLASS_INJECTED_FIELDS(macro) \
  CLASSLOADER_INJECTED_FIELDS(macro) \
  MODULE_INJECTED_FIELDS(macro)

// Interface to hard-coded offset checking

class JavaClasses : AllStatic {
 private:
  static InjectedField _injected_fields[];

  static bool check_offset(const char *klass_name, int offset, const char *field_name, const char* field_sig) { return 0; };
 public:
  enum InjectedFieldID {
    ALL_INJECTED_FIELDS(DECLARE_INJECTED_FIELD_ENUM)
    MAX_enum
  };

  static int compute_injected_offset(InjectedFieldID id);

  static void compute_hard_coded_offsets();
  static void compute_offsets();
  static void check_offsets() { };

  static InjectedField* get_injected(Symbol* class_name, int* field_count);
};

#undef DECLARE_INJECTED_FIELD_ENUM

#endif
