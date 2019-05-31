#include "precompiled.hpp"
#include "aot/aotLoader.hpp"
#include "classfile/classLoader.hpp"
#include "classfile/classLoaderData.hpp"
#include "classfile/javaClasses.hpp"
#include "classfile/stringTable.hpp"
#include "classfile/systemDictionary.hpp"
#include "classfile/vmSymbols.hpp"
#include "code/codeCache.hpp"
#include "code/dependencies.hpp"
#include "gc/shared/collectedHeap.inline.hpp"
#include "gc/shared/gcArguments.hpp"
#include "gc/shared/gcConfig.hpp"
#include "gc/shared/gcTraceTime.inline.hpp"
#include "interpreter/interpreter.hpp"
#include "logging/log.hpp"
#include "logging/logStream.hpp"
#include "memory/filemap.hpp"
#include "memory/metadataFactory.hpp"
#include "memory/metaspaceClosure.hpp"
#include "memory/metaspaceCounters.hpp"
#include "memory/metaspaceShared.hpp"
#include "memory/oopFactory.hpp"
#include "memory/resourceArea.hpp"
#include "memory/universe.hpp"
#include "memory/universe.hpp"
#include "oops/constantPool.hpp"
#include "oops/instanceClassLoaderKlass.hpp"
#include "oops/instanceKlass.hpp"
#include "oops/instanceMirrorKlass.hpp"
#include "oops/instanceRefKlass.hpp"
#include "oops/objArrayOop.inline.hpp"
#include "oops/oop.inline.hpp"
#include "oops/typeArrayKlass.hpp"
#include "prims/resolvedMethodTable.hpp"
#include "runtime/arguments.hpp"
#include "runtime/atomic.hpp"
#include "runtime/flags/flagSetting.hpp"
#include "runtime/flags/jvmFlagConstraintList.hpp"
#include "runtime/deoptimization.hpp"
#include "runtime/handles.inline.hpp"
#include "runtime/init.hpp"
#include "runtime/java.hpp"
#include "runtime/javaCalls.hpp"
#include "runtime/sharedRuntime.hpp"
#include "runtime/synchronizer.hpp"
#include "runtime/thread.inline.hpp"
#include "runtime/timerTrace.hpp"
#include "runtime/vm_operations.hpp"
#include "services/memoryService.hpp"
#include "utilities/align.hpp"
#include "utilities/copy.hpp"
#include "utilities/debug.hpp"
#include "utilities/events.hpp"
#include "utilities/formatBuffer.hpp"
#include "utilities/hashtable.inline.hpp"
#include "utilities/macros.hpp"
#include "utilities/ostream.hpp"
#include "utilities/preserveException.hpp"

// Known objects
Klass* Universe::_boolArrayKlassObj                 = NULL;
Klass* Universe::_byteArrayKlassObj                 = NULL;
Klass* Universe::_charArrayKlassObj                 = NULL;
Klass* Universe::_intArrayKlassObj                  = NULL;
Klass* Universe::_shortArrayKlassObj                = NULL;
Klass* Universe::_longArrayKlassObj                 = NULL;
Klass* Universe::_singleArrayKlassObj               = NULL;
Klass* Universe::_doubleArrayKlassObj               = NULL;
Klass* Universe::_typeArrayKlassObjs[T_VOID+1]      = { NULL /*, NULL...*/ };
Klass* Universe::_objectArrayKlassObj               = NULL;
oop Universe::_int_mirror                             = NULL;
oop Universe::_float_mirror                           = NULL;
oop Universe::_double_mirror                          = NULL;
oop Universe::_byte_mirror                            = NULL;
oop Universe::_bool_mirror                            = NULL;
oop Universe::_char_mirror                            = NULL;
oop Universe::_long_mirror                            = NULL;
oop Universe::_short_mirror                           = NULL;
oop Universe::_void_mirror                            = NULL;
oop Universe::_mirrors[T_VOID+1]                      = { NULL /*, NULL...*/ };
oop Universe::_main_thread_group                      = NULL;
oop Universe::_system_thread_group                    = NULL;
objArrayOop Universe::_the_empty_class_klass_array    = NULL;
Array<Klass*>* Universe::_the_array_interfaces_array = NULL;
oop Universe::_the_null_sentinel                      = NULL;
oop Universe::_the_null_string                        = NULL;
oop Universe::_the_min_jint_string                   = NULL;
LatestMethodCache* Universe::_finalizer_register_cache = NULL;
LatestMethodCache* Universe::_loader_addClass_cache    = NULL;
LatestMethodCache* Universe::_pd_implies_cache         = NULL;
LatestMethodCache* Universe::_throw_illegal_access_error_cache = NULL;
LatestMethodCache* Universe::_do_stack_walk_cache     = NULL;
oop Universe::_out_of_memory_error_java_heap          = NULL;
oop Universe::_out_of_memory_error_metaspace          = NULL;
oop Universe::_out_of_memory_error_class_metaspace    = NULL;
oop Universe::_out_of_memory_error_array_size         = NULL;
oop Universe::_out_of_memory_error_gc_overhead_limit  = NULL;
oop Universe::_out_of_memory_error_realloc_objects    = NULL;
oop Universe::_delayed_stack_overflow_error_message   = NULL;
objArrayOop Universe::_preallocated_out_of_memory_error_array = NULL;
volatile jint Universe::_preallocated_out_of_memory_error_avail_count = 0;
bool Universe::_verify_in_progress                    = false;
long Universe::verify_flags                           = Universe::Verify_All;
oop Universe::_null_ptr_exception_instance            = NULL;
oop Universe::_arithmetic_exception_instance          = NULL;
oop Universe::_virtual_machine_error_instance         = NULL;
oop Universe::_vm_exception                           = NULL;
oop Universe::_reference_pending_list                 = NULL;

Array<int>* Universe::_the_empty_int_array            = NULL;
Array<u2>* Universe::_the_empty_short_array           = NULL;
Array<Klass*>* Universe::_the_empty_klass_array     = NULL;
Array<Method*>* Universe::_the_empty_method_array   = NULL;

// Heap
int             Universe::_verify_count = 0;

// Oop verification (see MacroAssembler::verify_oop)
uintptr_t       Universe::_verify_oop_mask = 0;
uintptr_t       Universe::_verify_oop_bits = (uintptr_t) -1;

int             Universe::_base_vtable_size = 0;
bool            Universe::_bootstrapping = false;
bool            Universe::_module_initialized = false;
bool            Universe::_fully_initialized = false;

size_t          Universe::_heap_capacity_at_last_gc;
size_t          Universe::_heap_used_at_last_gc = 0;

CollectedHeap*  Universe::_collectedHeap = NULL;

NarrowPtrStruct Universe::_narrow_oop = { NULL, 0, true };
NarrowPtrStruct Universe::_narrow_klass = { NULL, 0, true };
address Universe::_narrow_ptrs_base;
uint64_t Universe::_narrow_klass_range = (uint64_t(max_juint)+1);

void Universe::basic_type_classes_do(void f(Klass*)) {
  f(boolArrayKlassObj());
  f(byteArrayKlassObj());
  f(charArrayKlassObj());
  f(intArrayKlassObj());
  f(shortArrayKlassObj());
  f(longArrayKlassObj());
  f(singleArrayKlassObj());
  f(doubleArrayKlassObj());
}

void Universe::oops_do(OopClosure* f, bool do_all) {

  f->do_oop((oop*) &_int_mirror);
  f->do_oop((oop*) &_float_mirror);
  f->do_oop((oop*) &_double_mirror);
  f->do_oop((oop*) &_byte_mirror);
  f->do_oop((oop*) &_bool_mirror);
  f->do_oop((oop*) &_char_mirror);
  f->do_oop((oop*) &_long_mirror);
  f->do_oop((oop*) &_short_mirror);
  f->do_oop((oop*) &_void_mirror);

  for (int i = T_BOOLEAN; i < T_VOID+1; i++) {
    f->do_oop((oop*) &_mirrors[i]);
  }

  f->do_oop((oop*)&_the_empty_class_klass_array);
  f->do_oop((oop*)&_the_null_sentinel);
  f->do_oop((oop*)&_the_null_string);
  f->do_oop((oop*)&_the_min_jint_string);
  f->do_oop((oop*)&_out_of_memory_error_java_heap);
  f->do_oop((oop*)&_out_of_memory_error_metaspace);
  f->do_oop((oop*)&_out_of_memory_error_class_metaspace);
  f->do_oop((oop*)&_out_of_memory_error_array_size);
  f->do_oop((oop*)&_out_of_memory_error_gc_overhead_limit);
  f->do_oop((oop*)&_out_of_memory_error_realloc_objects);
  f->do_oop((oop*)&_delayed_stack_overflow_error_message);
  f->do_oop((oop*)&_preallocated_out_of_memory_error_array);
  f->do_oop((oop*)&_null_ptr_exception_instance);
  f->do_oop((oop*)&_arithmetic_exception_instance);
  f->do_oop((oop*)&_virtual_machine_error_instance);
  f->do_oop((oop*)&_main_thread_group);
  f->do_oop((oop*)&_system_thread_group);
  f->do_oop((oop*)&_vm_exception);
  f->do_oop((oop*)&_reference_pending_list);
}

void LatestMethodCache::metaspace_pointers_do(MetaspaceClosure* it) {
  it->push(&_klass);
}

void Universe::metaspace_pointers_do(MetaspaceClosure* it) {
  it->push(&_boolArrayKlassObj);
  it->push(&_byteArrayKlassObj);
  it->push(&_charArrayKlassObj);
  it->push(&_intArrayKlassObj);
  it->push(&_shortArrayKlassObj);
  it->push(&_longArrayKlassObj);
  it->push(&_singleArrayKlassObj);
  it->push(&_doubleArrayKlassObj);
  for (int i = 0; i < T_VOID+1; i++) {
    it->push(&_typeArrayKlassObjs[i]);
  }
  it->push(&_objectArrayKlassObj);

  it->push(&_the_empty_int_array);
  it->push(&_the_empty_short_array);
  it->push(&_the_empty_klass_array);
  it->push(&_the_empty_method_array);
  it->push(&_the_array_interfaces_array);

  _finalizer_register_cache->metaspace_pointers_do(it);
  _loader_addClass_cache->metaspace_pointers_do(it);
  _pd_implies_cache->metaspace_pointers_do(it);
  _throw_illegal_access_error_cache->metaspace_pointers_do(it);
  _do_stack_walk_cache->metaspace_pointers_do(it);
}

// Serialize metadata and pointers to primitive type mirrors in and out of CDS archive
void Universe::serialize(SerializeClosure* f, bool do_all) {

  f->do_ptr((void**)&_boolArrayKlassObj);
  f->do_ptr((void**)&_byteArrayKlassObj);
  f->do_ptr((void**)&_charArrayKlassObj);
  f->do_ptr((void**)&_intArrayKlassObj);
  f->do_ptr((void**)&_shortArrayKlassObj);
  f->do_ptr((void**)&_longArrayKlassObj);
  f->do_ptr((void**)&_singleArrayKlassObj);
  f->do_ptr((void**)&_doubleArrayKlassObj);
  f->do_ptr((void**)&_objectArrayKlassObj);

  {
    for (int i = 0; i < T_VOID+1; i++) {
      if (_typeArrayKlassObjs[i] != NULL) {
        f->do_ptr((void**)&_typeArrayKlassObjs[i]);
      } else if (do_all) {
        f->do_ptr((void**)&_typeArrayKlassObjs[i]);
      }
    }
  }

  f->do_ptr((void**)&_the_array_interfaces_array);
  f->do_ptr((void**)&_the_empty_int_array);
  f->do_ptr((void**)&_the_empty_short_array);
  f->do_ptr((void**)&_the_empty_method_array);
  f->do_ptr((void**)&_the_empty_klass_array);
  _finalizer_register_cache->serialize(f);
  _loader_addClass_cache->serialize(f);
  _pd_implies_cache->serialize(f);
  _throw_illegal_access_error_cache->serialize(f);
  _do_stack_walk_cache->serialize(f);
}

void Universe::check_alignment(uintx size, uintx alignment, const char* name) {
  if (size < alignment || size % alignment != 0) {
    vm_exit_during_initialization(err_msg("Size of %s (" UINTX_FORMAT " bytes) must be aligned to " UINTX_FORMAT " bytes", name, size, alignment));
  }
}

void initialize_basic_type_klass(Klass* k, TRAPS) {
  Klass* ok = SystemDictionary::Object_klass();
  {
    k->initialize_supers(ok, NULL, CHECK);
  }
  k->append_to_sibling_list();
}

void Universe::genesis(TRAPS) {
  ResourceMark rm;

  { FlagSetting fs(_bootstrapping, true);

    { MutexLocker mc(Compile_lock);

      java_lang_Class::allocate_fixup_lists();

      // determine base vtable size; without that we cannot create the array klasses
      compute_base_vtable_size();

      if (!UseSharedSpaces) {
        _boolArrayKlassObj      = TypeArrayKlass::create_klass(T_BOOLEAN, sizeof(jboolean), CHECK);
        _charArrayKlassObj      = TypeArrayKlass::create_klass(T_CHAR,    sizeof(jchar),    CHECK);
        _singleArrayKlassObj    = TypeArrayKlass::create_klass(T_FLOAT,   sizeof(jfloat),   CHECK);
        _doubleArrayKlassObj    = TypeArrayKlass::create_klass(T_DOUBLE,  sizeof(jdouble),  CHECK);
        _byteArrayKlassObj      = TypeArrayKlass::create_klass(T_BYTE,    sizeof(jbyte),    CHECK);
        _shortArrayKlassObj     = TypeArrayKlass::create_klass(T_SHORT,   sizeof(jshort),   CHECK);
        _intArrayKlassObj       = TypeArrayKlass::create_klass(T_INT,     sizeof(jint),     CHECK);
        _longArrayKlassObj      = TypeArrayKlass::create_klass(T_LONG,    sizeof(jlong),    CHECK);

        _typeArrayKlassObjs[T_BOOLEAN] = _boolArrayKlassObj;
        _typeArrayKlassObjs[T_CHAR]    = _charArrayKlassObj;
        _typeArrayKlassObjs[T_FLOAT]   = _singleArrayKlassObj;
        _typeArrayKlassObjs[T_DOUBLE]  = _doubleArrayKlassObj;
        _typeArrayKlassObjs[T_BYTE]    = _byteArrayKlassObj;
        _typeArrayKlassObjs[T_SHORT]   = _shortArrayKlassObj;
        _typeArrayKlassObjs[T_INT]     = _intArrayKlassObj;
        _typeArrayKlassObjs[T_LONG]    = _longArrayKlassObj;

        ClassLoaderData* null_cld = ClassLoaderData::the_null_class_loader_data();

        _the_array_interfaces_array = MetadataFactory::new_array<Klass*>(null_cld, 2, NULL, CHECK);
        _the_empty_int_array        = MetadataFactory::new_array<int>(null_cld, 0, CHECK);
        _the_empty_short_array      = MetadataFactory::new_array<u2>(null_cld, 0, CHECK);
        _the_empty_method_array     = MetadataFactory::new_array<Method*>(null_cld, 0, CHECK);
        _the_empty_klass_array      = MetadataFactory::new_array<Klass*>(null_cld, 0, CHECK);
      }
    }

    vmSymbols::initialize(CHECK);

    SystemDictionary::initialize(CHECK);

    Klass* ok = SystemDictionary::Object_klass();

    _the_null_string            = StringTable::intern("null", CHECK);
    _the_min_jint_string       = StringTable::intern("-2147483648", CHECK);

    {
      // Set up shared interfaces array.  (Do this before supers are set up.)
      _the_array_interfaces_array->at_put(0, SystemDictionary::Cloneable_klass());
      _the_array_interfaces_array->at_put(1, SystemDictionary::Serializable_klass());
    }

    initialize_basic_type_klass(boolArrayKlassObj(), CHECK);
    initialize_basic_type_klass(charArrayKlassObj(), CHECK);
    initialize_basic_type_klass(singleArrayKlassObj(), CHECK);
    initialize_basic_type_klass(doubleArrayKlassObj(), CHECK);
    initialize_basic_type_klass(byteArrayKlassObj(), CHECK);
    initialize_basic_type_klass(shortArrayKlassObj(), CHECK);
    initialize_basic_type_klass(intArrayKlassObj(), CHECK);
    initialize_basic_type_klass(longArrayKlassObj(), CHECK);
  }

  {
    Handle tns = java_lang_String::create_from_str("<null_sentinel>", CHECK);
    _the_null_sentinel = tns();
  }

  // Maybe this could be lifted up now that object array can be initialized
  // during the bootstrapping.

  // OLD
  // Initialize _objectArrayKlass after core bootstraping to make
  // sure the super class is set up properly for _objectArrayKlass.
  // ---
  // NEW
  // Since some of the old system object arrays have been converted to
  // ordinary object arrays, _objectArrayKlass will be loaded when
  // SystemDictionary::initialize(CHECK); is run. See the extra check
  // for Object_klass_loaded in objArrayKlassKlass::allocate_objArray_klass_impl.
  _objectArrayKlassObj = InstanceKlass::cast(SystemDictionary::Object_klass())->array_klass(1, CHECK);
  // OLD
  // Add the class to the class hierarchy manually to make sure that
  // its vtable is initialized after core bootstrapping is completed.
  // ---
  // New
  // Have already been initialized.
  _objectArrayKlassObj->append_to_sibling_list();
}

void Universe::initialize_basic_type_mirrors(TRAPS) {
    {
      _int_mirror     = java_lang_Class::create_basic_type_mirror("int",    T_INT, CHECK);
      _float_mirror   = java_lang_Class::create_basic_type_mirror("float",  T_FLOAT,   CHECK);
      _double_mirror  = java_lang_Class::create_basic_type_mirror("double", T_DOUBLE,  CHECK);
      _byte_mirror    = java_lang_Class::create_basic_type_mirror("byte",   T_BYTE, CHECK);
      _bool_mirror    = java_lang_Class::create_basic_type_mirror("boolean",T_BOOLEAN, CHECK);
      _char_mirror    = java_lang_Class::create_basic_type_mirror("char",   T_CHAR, CHECK);
      _long_mirror    = java_lang_Class::create_basic_type_mirror("long",   T_LONG, CHECK);
      _short_mirror   = java_lang_Class::create_basic_type_mirror("short",  T_SHORT,   CHECK);
      _void_mirror    = java_lang_Class::create_basic_type_mirror("void",   T_VOID, CHECK);
    }

    _mirrors[T_INT]     = _int_mirror;
    _mirrors[T_FLOAT]   = _float_mirror;
    _mirrors[T_DOUBLE]  = _double_mirror;
    _mirrors[T_BYTE]    = _byte_mirror;
    _mirrors[T_BOOLEAN] = _bool_mirror;
    _mirrors[T_CHAR]    = _char_mirror;
    _mirrors[T_LONG]    = _long_mirror;
    _mirrors[T_SHORT]   = _short_mirror;
    _mirrors[T_VOID]    = _void_mirror;
  //_mirrors[T_OBJECT]  = _object_klass->java_mirror();
  //_mirrors[T_ARRAY]   = _object_klass->java_mirror();
}

void Universe::fixup_mirrors(TRAPS) {
  // Bootstrap problem: all classes gets a mirror (java.lang.Class instance) assigned eagerly,
  // but we cannot do that for classes created before java.lang.Class is loaded. Here we simply
  // walk over permanent objects created so far (mostly classes) and fixup their mirrors. Note
  // that the number of objects allocated at this point is very small.
  HandleMark hm(THREAD);
  // Cache the start of the static fields
  InstanceMirrorKlass::init_offset_of_static_fields();

  GrowableArray <Klass*>* list = java_lang_Class::fixup_mirror_list();
  int list_length = list->length();
  for (int i = 0; i < list_length; i++) {
    Klass* k = list->at(i);
    EXCEPTION_MARK;
    java_lang_Class::fixup_mirror(k, CATCH);
  }
  delete java_lang_Class::fixup_mirror_list();
  java_lang_Class::set_fixup_mirror_list(NULL);
}

oop Universe::reference_pending_list() {
  return _reference_pending_list;
}

void Universe::set_reference_pending_list(oop list) {
  _reference_pending_list = list;
}

bool Universe::has_reference_pending_list() {
  return _reference_pending_list != NULL;
}

oop Universe::swap_reference_pending_list(oop list) {
  return Atomic::xchg(list, &_reference_pending_list);
}

// initialize_vtable could cause gc if
// 1) we specified true to initialize_vtable and
// 2) this ran after gc was enabled
// In case those ever change we use handles for oops
void Universe::reinitialize_vtable_of(Klass* ko, TRAPS) {
  // init vtable of k and all subclasses
  ko->vtable().initialize_vtable(false, CHECK);
  if (ko->is_instance_klass()) {
    for (Klass* sk = ko->subklass();
         sk != NULL;
         sk = sk->next_sibling()) {
      reinitialize_vtable_of(sk, CHECK);
    }
  }
}

void initialize_itable_for_klass(InstanceKlass* k, TRAPS) {
  k->itable().initialize_itable(false, CHECK);
}

void Universe::reinitialize_itables(TRAPS) {
  ClassLoaderDataGraph::dictionary_classes_do(initialize_itable_for_klass, CHECK);
}

bool Universe::on_page_boundary(void* addr) {
  return is_aligned(addr, os::vm_page_size());
}

bool Universe::should_fill_in_stack_trace(Handle throwable) {
  // never attempt to fill in the stack trace of preallocated errors that do not have
  // backtrace. These errors are kept alive forever and may be "re-used" when all
  // preallocated errors with backtrace have been consumed. Also need to avoid
  // a potential loop which could happen if an out of memory occurs when attempting
  // to allocate the backtrace.
  return ((!oopDesc::equals(throwable(), Universe::_out_of_memory_error_java_heap)) &&
          (!oopDesc::equals(throwable(), Universe::_out_of_memory_error_metaspace))  &&
          (!oopDesc::equals(throwable(), Universe::_out_of_memory_error_class_metaspace))  &&
          (!oopDesc::equals(throwable(), Universe::_out_of_memory_error_array_size)) &&
          (!oopDesc::equals(throwable(), Universe::_out_of_memory_error_gc_overhead_limit)) &&
          (!oopDesc::equals(throwable(), Universe::_out_of_memory_error_realloc_objects)));
}

oop Universe::gen_out_of_memory_error(oop default_err) {
  // generate an out of memory error:
  // - if there is a preallocated error and stack traces are available
  //   (j.l.Throwable is initialized), then return the preallocated
  //   error with a filled in stack trace, and with the message
  //   provided by the default error.
  // - otherwise, return the default error, without a stack trace.
  int next;
  if ((_preallocated_out_of_memory_error_avail_count > 0) && SystemDictionary::Throwable_klass()->is_initialized()) {
    next = (int)Atomic::add(-1, &_preallocated_out_of_memory_error_avail_count);
  } else {
    next = -1;
  }
  if (next < 0) {
    // all preallocated errors have been used.
    // return default
    return default_err;
  } else {
    Thread* THREAD = Thread::current();
    Handle default_err_h(THREAD, default_err);
    // get the error object at the slot and set set it to NULL so that the
    // array isn't keeping it alive anymore.
    Handle exc(THREAD, preallocated_out_of_memory_errors()->obj_at(next));
    preallocated_out_of_memory_errors()->obj_at_put(next, NULL);

    // use the message from the default error
    oop msg = java_lang_Throwable::message(default_err_h());
    java_lang_Throwable::set_message(exc(), msg);

    // populate the stack trace and return it.
    java_lang_Throwable::fill_in_stack_trace_of_preallocated_backtrace(exc);
    return exc();
  }
}

intptr_t Universe::_non_oop_bits = 0;

void* Universe::non_oop_word() {
  // Neither the high bits nor the low bits of this value is allowed
  // to look like (respectively) the high or low bits of a real oop.
  //
  // High and low are CPU-specific notions, but low always includes
  // the low-order bit.  Since oops are always aligned at least mod 4,
  // setting the low-order bit will ensure that the low half of the
  // word will never look like that of a real oop.
  //
  // Using the OS-supplied non-memory-address word (usually 0 or -1)
  // will take care of the high bits, however many there are.

  if (_non_oop_bits == 0) {
    _non_oop_bits = (intptr_t)os::non_memory_address_word() | 1;
  }

  return (void*)_non_oop_bits;
}

jint universe_init() {
  guarantee(1 << LogHeapWordSize == sizeof(HeapWord),
         "LogHeapWordSize is incorrect.");
  guarantee(sizeof(oop) >= sizeof(HeapWord), "HeapWord larger than oop?");
  guarantee(sizeof(oop) % sizeof(HeapWord) == 0,
            "oop size is not not a multiple of HeapWord size");

  TraceTime timer("Genesis", TRACETIME_LOG(Info, startuptime));

  JavaClasses::compute_hard_coded_offsets();

  jint status = Universe::initialize_heap();
  if (status != JNI_OK) {
    return status;
  }

  SystemDictionary::initialize_oop_storage();

  Metaspace::global_initialize();

  // Initialize performance counters for metaspaces
  MetaspaceCounters::initialize_performance_counters();
  CompressedClassSpaceCounters::initialize_performance_counters();

  AOTLoader::universe_init();

  // Checks 'AfterMemoryInit' constraints.
  if (!JVMFlagConstraintList::check_constraints(JVMFlagConstraint::AfterMemoryInit)) {
    return JNI_EINVAL;
  }

  // Create memory for metadata.  Must be after initializing heap for
  // DumpSharedSpaces.
  ClassLoaderData::init_null_class_loader_data();

  // We have a heap so create the Method* caches before
  // Metaspace::initialize_shared_spaces() tries to populate them.
  Universe::_finalizer_register_cache = new LatestMethodCache();
  Universe::_loader_addClass_cache    = new LatestMethodCache();
  Universe::_pd_implies_cache         = new LatestMethodCache();
  Universe::_throw_illegal_access_error_cache = new LatestMethodCache();
  Universe::_do_stack_walk_cache = new LatestMethodCache();

  {
    SymbolTable::create_table();
    StringTable::create_table();
  }
  if (strlen(VerifySubSet) > 0) {
    Universe::initialize_verify_flags();
  }

  ResolvedMethodTable::create_table();

  return JNI_OK;
}

CollectedHeap* Universe::create_heap() {
  return GCConfig::arguments()->create_heap();
}

// Choose the heap base address and oop encoding mode
// when compressed oops are used:
// Unscaled  - Use 32-bits oops without encoding when
//     NarrowOopHeapBaseMin + heap_size < 4Gb
// ZeroBased - Use zero based compressed oops with encoding when
//     NarrowOopHeapBaseMin + heap_size < 32Gb
// HeapBased - Use compressed oops with heap base + encoding.

jint Universe::initialize_heap() {
  _collectedHeap = create_heap();
  jint status = _collectedHeap->initialize();
  if (status != JNI_OK) {
    return status;
  }
  log_info(gc)("Using %s", _collectedHeap->name());

  ThreadLocalAllocBuffer::set_max_size(Universe::heap()->max_tlab_size());

  if (UseCompressedOops) {
    // Subtract a page because something can get allocated at heap base.
    // This also makes implicit null checking work, because the
    // memory+1 page below heap_base needs to cause a signal.
    // See needs_explicit_null_check.
    // Only set the heap base for compressed oops because it indicates
    // compressed oops for pstack code.
    if ((uint64_t)Universe::heap()->reserved_region().end() > UnscaledOopHeapMax) {
      // Didn't reserve heap below 4Gb.  Must shift.
      Universe::set_narrow_oop_shift(LogMinObjAlignmentInBytes);
    }
    if ((uint64_t)Universe::heap()->reserved_region().end() <= OopEncodingHeapMax) {
      // Did reserve heap below 32Gb. Can use base == 0;
      Universe::set_narrow_oop_base(0);
    }
    AOTLoader::set_narrow_oop_shift();

    Universe::set_narrow_ptrs_base(Universe::narrow_oop_base());

    LogTarget(Info, gc, heap, coops) lt;
    if (lt.is_enabled()) {
      ResourceMark rm;
      LogStream ls(lt);
      Universe::print_compressed_oops_mode(&ls);
    }

    // Tell tests in which mode we run.
    Arguments::PropertyList_add(new SystemProperty("java.vm.compressedOopsMode", narrow_oop_mode_to_string(narrow_oop_mode()), false));
  }

  // We will never reach the CATCH below since Exceptions::_throw will cause
  // the VM to exit if an exception is thrown during initialization

  if (UseTLAB) {
    ThreadLocalAllocBuffer::startup_initialization();
  }
  return JNI_OK;
}

void Universe::print_compressed_oops_mode(outputStream* st) {
  st->print("Heap address: " PTR_FORMAT ", size: " SIZE_FORMAT " MB",
            p2i(Universe::heap()->base()), Universe::heap()->reserved_region().byte_size()/M);

  st->print(", Compressed Oops mode: %s", narrow_oop_mode_to_string(narrow_oop_mode()));

  if (Universe::narrow_oop_base() != 0) {
    st->print(": " PTR_FORMAT, p2i(Universe::narrow_oop_base()));
  }

  if (Universe::narrow_oop_shift() != 0) {
    st->print(", Oop shift amount: %d", Universe::narrow_oop_shift());
  }

  if (!Universe::narrow_oop_use_implicit_null_checks()) {
    st->print(", no protected page in front of the heap");
  }
  st->cr();
}

ReservedSpace Universe::reserve_heap(size_t heap_size, size_t alignment) {

  size_t total_reserved = align_up(heap_size, alignment);

  bool use_large_pages = UseLargePages && is_aligned(alignment, os::large_page_size());

  // Now create the space.
  ReservedHeapSpace total_rs(total_reserved, alignment, use_large_pages, AllocateHeapAt);

  if (total_rs.is_reserved()) {
    // We are good.

    if (UseCompressedOops) {
      // Universe::initialize_heap() will reset this to NULL if unscaled
      // or zero-based narrow oops are actually used.
      // Else heap start and base MUST differ, so that NULL can be encoded nonambigous.
      Universe::set_narrow_oop_base((address)total_rs.compressed_oop_base());
    }

    if (AllocateHeapAt != NULL) {
      log_info(gc,heap)("Successfully allocated Java heap at location %s", AllocateHeapAt);
    }
    return total_rs;
  }

  vm_exit_during_initialization(err_msg("Could not reserve enough space for " SIZE_FORMAT "KB object heap", total_reserved/K));

  // satisfy compiler
  ShouldNotReachHere();
  return ReservedHeapSpace(0, 0, false);
}

// It's the caller's responsibility to ensure glitch-freedom
// (if required).
void Universe::update_heap_info_at_gc() {
  _heap_capacity_at_last_gc = heap()->capacity();
  _heap_used_at_last_gc     = heap()->used();
}

const char* Universe::narrow_oop_mode_to_string(Universe::NARROW_OOP_MODE mode) {
  switch (mode) {
    case UnscaledNarrowOop:
      return "32-bit";
    case ZeroBasedNarrowOop:
      return "Zero based";
    case DisjointBaseNarrowOop:
      return "Non-zero disjoint base";
    case HeapBasedNarrowOop:
      return "Non-zero based";
    default:
      ShouldNotReachHere();
      return "";
  }
}

Universe::NARROW_OOP_MODE Universe::narrow_oop_mode() {
  if (narrow_oop_base_disjoint()) {
    return DisjointBaseNarrowOop;
  }

  if (narrow_oop_base() != 0) {
    return HeapBasedNarrowOop;
  }

  if (narrow_oop_shift() != 0) {
    return ZeroBasedNarrowOop;
  }

  return UnscaledNarrowOop;
}

void initialize_known_method(LatestMethodCache* method_cache, InstanceKlass* ik, const char* method, Symbol* signature, bool is_static, TRAPS) {
  TempNewSymbol name = SymbolTable::new_symbol(method, CHECK);
  Method* m = NULL;
  // The klass must be linked before looking up the method.
  if (!ik->link_class_or_fail(THREAD) || ((m = ik->find_method(name, signature)) == NULL) || is_static != m->is_static()) {
    ResourceMark rm(THREAD);
    // NoSuchMethodException doesn't actually work because it tries to run the
    // <init> function before java_lang_Class is linked. Print error and exit.
    vm_exit_during_initialization(err_msg("Unable to link/verify %s.%s method",
                                 ik->name()->as_C_string(), method));
  }
  method_cache->init(ik, m);
}

void Universe::initialize_known_methods(TRAPS) {
  // Set up static method for registering finalizers
  initialize_known_method(_finalizer_register_cache,
                          SystemDictionary::Finalizer_klass(),
                          "register",
                          vmSymbols::object_void_signature(), true, CHECK);

  initialize_known_method(_throw_illegal_access_error_cache,
                          SystemDictionary::internal_Unsafe_klass(),
                          "throwIllegalAccessError",
                          vmSymbols::void_method_signature(), true, CHECK);

  // Set up method for registering loaded classes in class loader vector
  initialize_known_method(_loader_addClass_cache,
                          SystemDictionary::ClassLoader_klass(),
                          "addClass",
                          vmSymbols::class_void_signature(), false, CHECK);

  // Set up method for checking protection domain
  initialize_known_method(_pd_implies_cache,
                          SystemDictionary::ProtectionDomain_klass(),
                          "impliesCreateAccessControlContext",
                          vmSymbols::void_boolean_signature(), false, CHECK);

  // Set up method for stack walking
  initialize_known_method(_do_stack_walk_cache,
                          SystemDictionary::AbstractStackWalker_klass(),
                          "doStackWalk",
                          vmSymbols::doStackWalk_signature(), false, CHECK);
}

void universe2_init() {
  EXCEPTION_MARK;
  Universe::genesis(CATCH);
}

// Set after initialization of the module runtime, call_initModuleRuntime
void universe_post_module_init() {
  Universe::_module_initialized = true;
}

bool universe_post_init() {
  Universe::_fully_initialized = true;
  EXCEPTION_MARK;
  { ResourceMark rm;
    Interpreter::initialize();      // needed for interpreter entry points
    if (!UseSharedSpaces) {
      HandleMark hm(THREAD);
      Klass* ok = SystemDictionary::Object_klass();
      Universe::reinitialize_vtable_of(ok, CHECK_false);
      Universe::reinitialize_itables(CHECK_false);
    }
  }

  HandleMark hm(THREAD);
  // Setup preallocated empty java.lang.Class array
  Universe::_the_empty_class_klass_array = oopFactory::new_objArray(SystemDictionary::Class_klass(), 0, CHECK_false);

  // Setup preallocated OutOfMemoryError errors
  Klass* k = SystemDictionary::resolve_or_fail(vmSymbols::java_lang_OutOfMemoryError(), true, CHECK_false);
  InstanceKlass* ik = InstanceKlass::cast(k);
  Universe::_out_of_memory_error_java_heap = ik->allocate_instance(CHECK_false);
  Universe::_out_of_memory_error_metaspace = ik->allocate_instance(CHECK_false);
  Universe::_out_of_memory_error_class_metaspace = ik->allocate_instance(CHECK_false);
  Universe::_out_of_memory_error_array_size = ik->allocate_instance(CHECK_false);
  Universe::_out_of_memory_error_gc_overhead_limit = ik->allocate_instance(CHECK_false);
  Universe::_out_of_memory_error_realloc_objects = ik->allocate_instance(CHECK_false);

  // Setup preallocated cause message for delayed StackOverflowError
  if (StackReservedPages > 0) {
    Universe::_delayed_stack_overflow_error_message = java_lang_String::create_oop_from_str("Delayed StackOverflowError due to ReservedStackAccess annotated method", CHECK_false);
  }

  // Setup preallocated NullPointerException
  // (this is currently used for a cheap & dirty solution in compiler exception handling)
  k = SystemDictionary::resolve_or_fail(vmSymbols::java_lang_NullPointerException(), true, CHECK_false);
  Universe::_null_ptr_exception_instance = InstanceKlass::cast(k)->allocate_instance(CHECK_false);
  // Setup preallocated ArithmeticException
  // (this is currently used for a cheap & dirty solution in compiler exception handling)
  k = SystemDictionary::resolve_or_fail(vmSymbols::java_lang_ArithmeticException(), true, CHECK_false);
  Universe::_arithmetic_exception_instance = InstanceKlass::cast(k)->allocate_instance(CHECK_false);
  // Virtual Machine Error for when we get into a situation we can't resolve
  k = SystemDictionary::resolve_or_fail(vmSymbols::java_lang_VirtualMachineError(), true, CHECK_false);
  bool linked = InstanceKlass::cast(k)->link_class_or_fail(CHECK_false);
  if (!linked) {
     tty->print_cr("Unable to link/verify VirtualMachineError class");
     return false; // initialization failed
  }
  Universe::_virtual_machine_error_instance = InstanceKlass::cast(k)->allocate_instance(CHECK_false);

  Universe::_vm_exception = InstanceKlass::cast(k)->allocate_instance(CHECK_false);

  Handle msg = java_lang_String::create_from_str("Java heap space", CHECK_false);
  java_lang_Throwable::set_message(Universe::_out_of_memory_error_java_heap, msg());

  msg = java_lang_String::create_from_str("Metaspace", CHECK_false);
  java_lang_Throwable::set_message(Universe::_out_of_memory_error_metaspace, msg());
  msg = java_lang_String::create_from_str("Compressed class space", CHECK_false);
  java_lang_Throwable::set_message(Universe::_out_of_memory_error_class_metaspace, msg());

  msg = java_lang_String::create_from_str("Requested array size exceeds VM limit", CHECK_false);
  java_lang_Throwable::set_message(Universe::_out_of_memory_error_array_size, msg());

  msg = java_lang_String::create_from_str("GC overhead limit exceeded", CHECK_false);
  java_lang_Throwable::set_message(Universe::_out_of_memory_error_gc_overhead_limit, msg());

  msg = java_lang_String::create_from_str("Java heap space: failed reallocation of scalar replaced objects", CHECK_false);
  java_lang_Throwable::set_message(Universe::_out_of_memory_error_realloc_objects, msg());

  msg = java_lang_String::create_from_str("/ by zero", CHECK_false);
  java_lang_Throwable::set_message(Universe::_arithmetic_exception_instance, msg());

  // Setup the array of errors that have preallocated backtrace
  k = Universe::_out_of_memory_error_java_heap->klass();
  ik = InstanceKlass::cast(k);

  int len = (StackTraceInThrowable) ? (int)PreallocatedOutOfMemoryErrorCount : 0;
  Universe::_preallocated_out_of_memory_error_array = oopFactory::new_objArray(ik, len, CHECK_false);
  for (int i=0; i<len; i++) {
    oop err = ik->allocate_instance(CHECK_false);
    Handle err_h = Handle(THREAD, err);
    java_lang_Throwable::allocate_backtrace(err_h, CHECK_false);
    Universe::preallocated_out_of_memory_errors()->obj_at_put(i, err_h());
  }
  Universe::_preallocated_out_of_memory_error_avail_count = (jint)len;

  Universe::initialize_known_methods(CHECK_false);

  // This needs to be done before the first scavenge/gc, since
  // it's an input to soft ref clearing policy.
  {
    MutexLocker x(Heap_lock);
    Universe::update_heap_info_at_gc();
  }

  // ("weak") refs processing infrastructure initialization
  Universe::heap()->post_initialize();

  MemoryService::add_metaspace_memory_pools();

  MemoryService::set_universe_heap(Universe::heap());
  return true;
}

void Universe::compute_base_vtable_size() {
  _base_vtable_size = ClassLoader::compute_Object_vtable();
}

void Universe::print_on(outputStream* st) {
  GCMutexLocker hl(Heap_lock); // Heap_lock might be locked by caller thread.
  st->print_cr("Heap");
  heap()->print_on(st);
}

void Universe::print_heap_at_SIGBREAK() {
  if (PrintHeapAtSIGBREAK) {
    print_on(tty);
    tty->cr();
    tty->flush();
  }
}

void Universe::print_heap_before_gc() {
  LogTarget(Debug, gc, heap) lt;
  if (lt.is_enabled()) {
    LogStream ls(lt);
    ls.print("Heap before GC invocations=%u (full %u):", heap()->total_collections(), heap()->total_full_collections());
    ResourceMark rm;
    heap()->print_on(&ls);
  }
}

void Universe::print_heap_after_gc() {
  LogTarget(Debug, gc, heap) lt;
  if (lt.is_enabled()) {
    LogStream ls(lt);
    ls.print("Heap after GC invocations=%u (full %u):", heap()->total_collections(), heap()->total_full_collections());
    ResourceMark rm;
    heap()->print_on(&ls);
  }
}

void Universe::initialize_verify_flags() {
  verify_flags = 0;
  const char delimiter[] = " ,";

  size_t length = strlen(VerifySubSet);
  char* subset_list = NEW_C_HEAP_ARRAY(char, length + 1, mtInternal);
  strncpy(subset_list, VerifySubSet, length + 1);

  char* token = strtok(subset_list, delimiter);
  while (token != NULL) {
    if (strcmp(token, "threads") == 0) {
      verify_flags |= Verify_Threads;
    } else if (strcmp(token, "heap") == 0) {
      verify_flags |= Verify_Heap;
    } else if (strcmp(token, "symbol_table") == 0) {
      verify_flags |= Verify_SymbolTable;
    } else if (strcmp(token, "string_table") == 0) {
      verify_flags |= Verify_StringTable;
    } else if (strcmp(token, "codecache") == 0) {
      verify_flags |= Verify_CodeCache;
    } else if (strcmp(token, "dictionary") == 0) {
      verify_flags |= Verify_SystemDictionary;
    } else if (strcmp(token, "classloader_data_graph") == 0) {
      verify_flags |= Verify_ClassLoaderDataGraph;
    } else if (strcmp(token, "metaspace") == 0) {
      verify_flags |= Verify_MetaspaceUtils;
    } else if (strcmp(token, "jni_handles") == 0) {
      verify_flags |= Verify_JNIHandles;
    } else if (strcmp(token, "codecache_oops") == 0) {
      verify_flags |= Verify_CodeCacheOops;
    } else {
      vm_exit_during_initialization(err_msg("VerifySubSet: \'%s\' memory sub-system is unknown, please correct it", token));
    }
    token = strtok(NULL, delimiter);
  }
  FREE_C_HEAP_ARRAY(char, subset_list);
}

bool Universe::should_verify_subset(uint subset) {
  if (verify_flags & subset) {
    return true;
  }
  return false;
}

void Universe::verify(VerifyOption option, const char* prefix) {
  // The use of _verify_in_progress is a temporary work around for
  // 6320749.  Don't bother with a creating a class to set and clear
  // it since it is only used in this method and the control flow is
  // straight forward.
  _verify_in_progress = true;

  ResourceMark rm;
  HandleMark hm;  // Handles created during verification can be zapped
  _verify_count++;

  FormatBuffer<> title("Verifying %s", prefix);
  GCTraceTime(Info, gc, verify) tm(title.buffer());
  if (should_verify_subset(Verify_Threads)) {
    log_debug(gc, verify)("Threads");
    Threads::verify();
  }
  if (should_verify_subset(Verify_Heap)) {
    log_debug(gc, verify)("Heap");
    heap()->verify(option);
  }
  if (should_verify_subset(Verify_SymbolTable)) {
    log_debug(gc, verify)("SymbolTable");
    SymbolTable::verify();
  }
  if (should_verify_subset(Verify_StringTable)) {
    log_debug(gc, verify)("StringTable");
    StringTable::verify();
  }
  if (should_verify_subset(Verify_CodeCache)) {
  {
    MutexLockerEx mu(CodeCache_lock, Mutex::_no_safepoint_check_flag);
    log_debug(gc, verify)("CodeCache");
    CodeCache::verify();
  }
  }
  if (should_verify_subset(Verify_SystemDictionary)) {
    log_debug(gc, verify)("SystemDictionary");
    SystemDictionary::verify();
  }
  if (should_verify_subset(Verify_MetaspaceUtils)) {
    log_debug(gc, verify)("MetaspaceUtils");
    MetaspaceUtils::verify_free_chunks();
  }
  if (should_verify_subset(Verify_JNIHandles)) {
    log_debug(gc, verify)("JNIHandles");
    JNIHandles::verify();
  }
  if (should_verify_subset(Verify_CodeCacheOops)) {
    log_debug(gc, verify)("CodeCache Oops");
    CodeCache::verify_oops();
  }

  _verify_in_progress = false;
}

void Universe::compute_verify_oop_data() {
  verify_oop_mask();
  verify_oop_bits();
  verify_mark_mask();
  verify_mark_bits();
}

void LatestMethodCache::init(Klass* k, Method* m) {
  if (!UseSharedSpaces) {
    _klass = k;
  }

  _method_idnum = m->method_idnum();
}

Method* LatestMethodCache::get_method() {
  if (klass() == NULL) return NULL;
  InstanceKlass* ik = InstanceKlass::cast(klass());
  Method* m = ik->method_with_idnum(method_idnum());
  return m;
}
