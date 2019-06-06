#include "precompiled.hpp"

#include "jvm.h"
#include "ci/ciConstant.hpp"
#include "ci/ciEnv.hpp"
#include "ci/ciField.hpp"
#include "ci/ciInstance.hpp"
#include "ci/ciInstanceKlass.hpp"
#include "ci/ciMethod.hpp"
#include "ci/ciNullObject.hpp"
#include "ci/ciReplay.hpp"
#include "ci/ciUtilities.inline.hpp"
#include "classfile/systemDictionary.hpp"
#include "classfile/vmSymbols.hpp"
#include "code/codeCache.hpp"
#include "code/scopeDesc.hpp"
#include "compiler/compileBroker.hpp"
#include "compiler/disassembler.hpp"
#include "gc/shared/collectedHeap.inline.hpp"
#include "interpreter/linkResolver.hpp"
#include "memory/allocation.inline.hpp"
#include "memory/oopFactory.hpp"
#include "memory/resourceArea.hpp"
#include "memory/universe.hpp"
#include "oops/constantPool.inline.hpp"
#include "oops/cpCache.inline.hpp"
#include "oops/method.inline.hpp"
#include "oops/methodData.hpp"
#include "oops/objArrayKlass.hpp"
#include "oops/objArrayOop.inline.hpp"
#include "oops/oop.inline.hpp"
#include "runtime/init.hpp"
#include "runtime/reflection.hpp"
#include "runtime/jniHandles.inline.hpp"
#include "runtime/safepointVerifiers.hpp"
#include "runtime/sharedRuntime.hpp"
#include "runtime/thread.inline.hpp"
#include "utilities/macros.hpp"
#include "c1/c1_Runtime1.hpp"

// ciEnv
//
// This class is the top level broker for requests from the compiler
// to the VM.

ciObject*              ciEnv::_null_object_instance;

#define WK_KLASS_DEFN(name, ignore_s, ignore_o) ciInstanceKlass* ciEnv::_##name = NULL;
WK_KLASSES_DO(WK_KLASS_DEFN)
#undef WK_KLASS_DEFN

ciSymbol*        ciEnv::_unloaded_cisymbol = NULL;
ciInstanceKlass* ciEnv::_unloaded_ciinstance_klass = NULL;
ciObjArrayKlass* ciEnv::_unloaded_ciobjarrayklass = NULL;

jobject ciEnv::_ArrayIndexOutOfBoundsException_handle = NULL;
jobject ciEnv::_ArrayStoreException_handle = NULL;
jobject ciEnv::_ClassCastException_handle = NULL;

// ------------------------------------------------------------------
// ciEnv::ciEnv
ciEnv::ciEnv(CompileTask* task, int system_dictionary_modification_counter)
  : _ciEnv_arena(mtCompiler) {
  VM_ENTRY_MARK;

  // Set up ciEnv::current immediately, for the sake of ciObjectFactory, etc.
  thread->set_env(this);

  _oop_recorder = NULL;
  _debug_info = NULL;
  _dependencies = NULL;
  _failure_reason = NULL;
  _inc_decompile_count_on_failure = true;
  _compilable = MethodCompilable;
  _break_at_compile = false;
  _compiler_data = NULL;

  _system_dictionary_modification_counter = system_dictionary_modification_counter;
  _num_inlined_bytecodes = 0;
  _task = task;

  // Temporary buffer for creating symbols and such.
  _name_buffer = NULL;
  _name_buffer_len = 0;

  _arena   = &_ciEnv_arena;
  _factory = new (_arena) ciObjectFactory(_arena, 128);

  // Preload commonly referenced system ciObjects.

  // During VM initialization, these instances have not yet been created.
  // Assertions ensure that these instances are not accessed before
  // their initialization.

  oop o = Universe::null_ptr_exception_instance();
  _NullPointerException_instance = get_object(o)->as_instance();
  o = Universe::arithmetic_exception_instance();
  _ArithmeticException_instance = get_object(o)->as_instance();

  _ArrayIndexOutOfBoundsException_instance = NULL;
  _ArrayStoreException_instance = NULL;
  _ClassCastException_instance = NULL;
  _the_null_string = NULL;
  _the_min_jint_string = NULL;
}

ciEnv::ciEnv(Arena* arena) : _ciEnv_arena(mtCompiler) {
  ASSERT_IN_VM;

  // Set up ciEnv::current immediately, for the sake of ciObjectFactory, etc.
  CompilerThread* current_thread = CompilerThread::current();
  current_thread->set_env(this);

  _oop_recorder = NULL;
  _debug_info = NULL;
  _dependencies = NULL;
  _failure_reason = NULL;
  _inc_decompile_count_on_failure = true;
  _compilable = MethodCompilable_never;
  _break_at_compile = false;
  _compiler_data = NULL;

  _system_dictionary_modification_counter = 0;
  _num_inlined_bytecodes = 0;
  _task = NULL;

  // Temporary buffer for creating symbols and such.
  _name_buffer = NULL;
  _name_buffer_len = 0;

  _arena   = arena;
  _factory = new (_arena) ciObjectFactory(_arena, 128);

  // Preload commonly referenced system ciObjects.

  // During VM initialization, these instances have not yet been created.
  // Assertions ensure that these instances are not accessed before
  // their initialization.

  _NullPointerException_instance = NULL;
  _ArithmeticException_instance = NULL;
  _ArrayIndexOutOfBoundsException_instance = NULL;
  _ArrayStoreException_instance = NULL;
  _ClassCastException_instance = NULL;
  _the_null_string = NULL;
  _the_min_jint_string = NULL;
}

ciEnv::~ciEnv() {
  GUARDED_VM_ENTRY(
      CompilerThread* current_thread = CompilerThread::current();
      _factory->remove_symbols();
      // Need safepoint to clear the env on the thread.  RedefineClasses might
      // be reading it.
      current_thread->set_env(NULL);
  )
}

// ------------------------------------------------------------------
// helper for lazy exception creation
ciInstance* ciEnv::get_or_create_exception(jobject& handle, Symbol* name) {
  VM_ENTRY_MARK;
  if (handle == NULL) {
    // Cf. universe.cpp, creation of Universe::_null_ptr_exception_instance.
    Klass* k = SystemDictionary::find(name, Handle(), Handle(), THREAD);
    jobject objh = NULL;
    if (!HAS_PENDING_EXCEPTION && k != NULL) {
      oop obj = InstanceKlass::cast(k)->allocate_instance(THREAD);
      if (!HAS_PENDING_EXCEPTION)
        objh = JNIHandles::make_global(Handle(THREAD, obj));
    }
    if (HAS_PENDING_EXCEPTION) {
      CLEAR_PENDING_EXCEPTION;
    } else {
      handle = objh;
    }
  }
  oop obj = JNIHandles::resolve(handle);
  return obj == NULL? NULL: get_object(obj)->as_instance();
}

ciInstance* ciEnv::ArrayIndexOutOfBoundsException_instance() {
  if (_ArrayIndexOutOfBoundsException_instance == NULL) {
    _ArrayIndexOutOfBoundsException_instance = get_or_create_exception(_ArrayIndexOutOfBoundsException_handle, vmSymbols::java_lang_ArrayIndexOutOfBoundsException());
  }
  return _ArrayIndexOutOfBoundsException_instance;
}
ciInstance* ciEnv::ArrayStoreException_instance() {
  if (_ArrayStoreException_instance == NULL) {
    _ArrayStoreException_instance = get_or_create_exception(_ArrayStoreException_handle, vmSymbols::java_lang_ArrayStoreException());
  }
  return _ArrayStoreException_instance;
}
ciInstance* ciEnv::ClassCastException_instance() {
  if (_ClassCastException_instance == NULL) {
    _ClassCastException_instance = get_or_create_exception(_ClassCastException_handle, vmSymbols::java_lang_ClassCastException());
  }
  return _ClassCastException_instance;
}

ciInstance* ciEnv::the_null_string() {
  if (_the_null_string == NULL) {
    VM_ENTRY_MARK;
    _the_null_string = get_object(Universe::the_null_string())->as_instance();
  }
  return _the_null_string;
}

ciInstance* ciEnv::the_min_jint_string() {
  if (_the_min_jint_string == NULL) {
    VM_ENTRY_MARK;
    _the_min_jint_string = get_object(Universe::the_min_jint_string())->as_instance();
  }
  return _the_min_jint_string;
}

// ------------------------------------------------------------------
// ciEnv::get_method_from_handle
ciMethod* ciEnv::get_method_from_handle(Method* method) {
  VM_ENTRY_MARK;
  return get_metadata(method)->as_method();
}

// ------------------------------------------------------------------
// ciEnv::array_element_offset_in_bytes
int ciEnv::array_element_offset_in_bytes(ciArray* a_h, ciObject* o_h) {
  VM_ENTRY_MARK;
  objArrayOop a = (objArrayOop)a_h->get_oop();
  int length = a->length();
  oop o = o_h->get_oop();
  for (int i = 0; i < length; i++) {
    if (a->obj_at(i) == o)  return i;
  }
  return -1;
}

// ------------------------------------------------------------------
// ciEnv::check_klass_accessiblity
//
// Note: the logic of this method should mirror the logic of
// ConstantPool::verify_constant_pool_resolve.
bool ciEnv::check_klass_accessibility(ciKlass* accessing_klass, Klass* resolved_klass) {
  if (accessing_klass == NULL || !accessing_klass->is_loaded()) {
    return true;
  }
  if (accessing_klass->is_obj_array_klass()) {
    accessing_klass = accessing_klass->as_obj_array_klass()->base_element_klass();
  }
  if (!accessing_klass->is_instance_klass()) {
    return true;
  }

  if (resolved_klass->is_objArray_klass()) {
    // Find the element klass, if this is an array.
    resolved_klass = ObjArrayKlass::cast(resolved_klass)->bottom_klass();
  }
  if (resolved_klass->is_instance_klass()) {
    return (Reflection::verify_class_access(accessing_klass->get_Klass(), InstanceKlass::cast(resolved_klass), true) == Reflection::ACCESS_OK);
  }
  return true;
}

// ------------------------------------------------------------------
// ciEnv::get_klass_by_name_impl
ciKlass* ciEnv::get_klass_by_name_impl(ciKlass* accessing_klass,
                                       const constantPoolHandle& cpool,
                                       ciSymbol* name,
                                       bool require_local) {
  ASSERT_IN_VM;
  EXCEPTION_CONTEXT;

  // Now we need to check the SystemDictionary
  Symbol* sym = name->get_symbol();
  if (sym->byte_at(0) == 'L' && sym->byte_at(sym->utf8_length()-1) == ';') {
    // This is a name from a signature.  Strip off the trimmings.
    // Call recursive to keep scope of strippedsym.
    TempNewSymbol strippedsym = SymbolTable::new_symbol(sym->as_utf8()+1, sym->utf8_length()-2, KILL_COMPILE_ON_FATAL_(_unloaded_ciinstance_klass));
    ciSymbol* strippedname = get_symbol(strippedsym);
    return get_klass_by_name_impl(accessing_klass, cpool, strippedname, require_local);
  }

  // Check for prior unloaded klass.  The SystemDictionary's answers
  // can vary over time but the compiler needs consistency.
  ciKlass* unloaded_klass = check_get_unloaded_klass(accessing_klass, name);
  if (unloaded_klass != NULL) {
    if (require_local)  return NULL;
    return unloaded_klass;
  }

  Handle loader(THREAD, (oop)NULL);
  Handle domain(THREAD, (oop)NULL);
  if (accessing_klass != NULL) {
    loader = Handle(THREAD, accessing_klass->loader());
    domain = Handle(THREAD, accessing_klass->protection_domain());
  }

  // setup up the proper type to return on OOM
  ciKlass* fail_type;
  if (sym->byte_at(0) == '[') {
    fail_type = _unloaded_ciobjarrayklass;
  } else {
    fail_type = _unloaded_ciinstance_klass;
  }
  Klass* found_klass;
  {
    ttyUnlocker ttyul;  // release tty lock to avoid ordering problems
    MutexLocker ml(Compile_lock);
    Klass* kls;
    if (!require_local) {
      kls = SystemDictionary::find_constrained_instance_or_array_klass(sym, loader,
                                                                       KILL_COMPILE_ON_FATAL_(fail_type));
    } else {
      kls = SystemDictionary::find_instance_or_array_klass(sym, loader, domain,
                                                           KILL_COMPILE_ON_FATAL_(fail_type));
    }
    found_klass = kls;
  }

  // If we fail to find an array klass, look again for its element type.
  // The element type may be available either locally or via constraints.
  // In either case, if we can find the element type in the system dictionary,
  // we must build an array type around it.  The CI requires array klasses
  // to be loaded if their element klasses are loaded, except when memory
  // is exhausted.
  if (sym->byte_at(0) == '[' && (sym->byte_at(1) == '[' || sym->byte_at(1) == 'L')) {
    // We have an unloaded array.
    // Build it on the fly if the element class exists.
    TempNewSymbol elem_sym = SymbolTable::new_symbol(sym->as_utf8()+1, sym->utf8_length()-1, KILL_COMPILE_ON_FATAL_(fail_type));

    // Get element ciKlass recursively.
    ciKlass* elem_klass = get_klass_by_name_impl(accessing_klass, cpool, get_symbol(elem_sym), require_local);
    if (elem_klass != NULL && elem_klass->is_loaded()) {
      // Now make an array for it
      return ciObjArrayKlass::make_impl(elem_klass);
    }
  }

  if (found_klass == NULL && !cpool.is_null() && cpool->has_preresolution()) {
    // Look inside the constant pool for pre-resolved class entries.
    for (int i = cpool->length() - 1; i >= 1; i--) {
      if (cpool->tag_at(i).is_klass()) {
        Klass* kls = cpool->resolved_klass_at(i);
        if (kls->name() == sym) {
          found_klass = kls;
          break;
        }
      }
    }
  }

  if (found_klass != NULL) {
    // Found it.  Build a CI handle.
    return get_klass(found_klass);
  }

  if (require_local)  return NULL;

  // Not yet loaded into the VM, or not governed by loader constraints.
  // Make a CI representative for it.
  return get_unloaded_klass(accessing_klass, name);
}

// ------------------------------------------------------------------
// ciEnv::get_klass_by_name
ciKlass* ciEnv::get_klass_by_name(ciKlass* accessing_klass, ciSymbol* klass_name, bool require_local) {
  GUARDED_VM_ENTRY(return get_klass_by_name_impl(accessing_klass, constantPoolHandle(), klass_name, require_local);)
}

// ------------------------------------------------------------------
// ciEnv::get_klass_by_index_impl
//
// Implementation of get_klass_by_index.
ciKlass* ciEnv::get_klass_by_index_impl(const constantPoolHandle& cpool, int index, bool& is_accessible, ciInstanceKlass* accessor) {
  EXCEPTION_CONTEXT;
  Klass* klass = NULL;
  Symbol* klass_name = NULL;

  if (cpool->tag_at(index).is_symbol()) {
    klass_name = cpool->symbol_at(index);
  } else {
    // Check if it's resolved if it's not a symbol constant pool entry.
    klass =  ConstantPool::klass_at_if_loaded(cpool, index);
    // Try to look it up by name.
    if (klass == NULL) {
      klass_name = cpool->klass_name_at(index);
    }
  }

  if (klass == NULL) {
    // Not found in constant pool.  Use the name to do the lookup.
    ciKlass* k = get_klass_by_name_impl(accessor, cpool, get_symbol(klass_name), false);
    // Calculate accessibility the hard way.
    if (!k->is_loaded()) {
      is_accessible = false;
    } else if (!oopDesc::equals(k->loader(), accessor->loader()) && get_klass_by_name_impl(accessor, cpool, k->name(), true) == NULL) {
      // Loaded only remotely.  Not linked yet.
      is_accessible = false;
    } else {
      // Linked locally, and we must also check public/private, etc.
      is_accessible = check_klass_accessibility(accessor, k->get_Klass());
    }
    return k;
  }

  // Check for prior unloaded klass.  The SystemDictionary's answers
  // can vary over time but the compiler needs consistency.
  ciSymbol* name = get_symbol(klass->name());
  ciKlass* unloaded_klass = check_get_unloaded_klass(accessor, name);
  if (unloaded_klass != NULL) {
    is_accessible = false;
    return unloaded_klass;
  }

  // It is known to be accessible, since it was found in the constant pool.
  is_accessible = true;
  return get_klass(klass);
}

// ------------------------------------------------------------------
// ciEnv::get_klass_by_index
//
// Get a klass from the constant pool.
ciKlass* ciEnv::get_klass_by_index(const constantPoolHandle& cpool, int index, bool& is_accessible, ciInstanceKlass* accessor) {
  GUARDED_VM_ENTRY(return get_klass_by_index_impl(cpool, index, is_accessible, accessor);)
}

// ------------------------------------------------------------------
// ciEnv::get_constant_by_index_impl
//
// Implementation of get_constant_by_index().
ciConstant ciEnv::get_constant_by_index_impl(const constantPoolHandle& cpool, int pool_index, int cache_index, ciInstanceKlass* accessor) {
  bool ignore_will_link;
  EXCEPTION_CONTEXT;
  int index = pool_index;
  if (cache_index >= 0) {
    index = cpool->object_to_cp_index(cache_index);
    oop obj = cpool->resolved_references()->obj_at(cache_index);
    if (obj != NULL) {
      if (oopDesc::equals(obj, Universe::the_null_sentinel())) {
        return ciConstant(T_OBJECT, get_object(NULL));
      }
      BasicType bt = T_OBJECT;
      if (cpool->tag_at(index).is_dynamic_constant())
        bt = FieldType::basic_type(cpool->uncached_signature_ref_at(index));
      if (is_reference_type(bt)) {
      } else {
        // we have to unbox the primitive value
        if (!is_java_primitive(bt))  return ciConstant();
        jvalue value;
        BasicType bt2 = java_lang_boxing_object::get_value(obj, &value);
        switch (bt2) {
        case T_DOUBLE:  return ciConstant(value.d);
        case T_FLOAT:   return ciConstant(value.f);
        case T_LONG:    return ciConstant(value.j);
        case T_INT:     return ciConstant(bt2, value.i);
        case T_SHORT:   return ciConstant(bt2, value.s);
        case T_BYTE:    return ciConstant(bt2, value.b);
        case T_CHAR:    return ciConstant(bt2, value.c);
        case T_BOOLEAN: return ciConstant(bt2, value.z);
        default:  return ciConstant();
        }
      }
      ciObject* ciobj = get_object(obj);
      if (ciobj->is_array()) {
        return ciConstant(T_ARRAY, ciobj);
      } else {
        return ciConstant(T_OBJECT, ciobj);
      }
    }
  }
  constantTag tag = cpool->tag_at(index);
  if (tag.is_int()) {
    return ciConstant(T_INT, (jint)cpool->int_at(index));
  } else if (tag.is_long()) {
    return ciConstant((jlong)cpool->long_at(index));
  } else if (tag.is_float()) {
    return ciConstant((jfloat)cpool->float_at(index));
  } else if (tag.is_double()) {
    return ciConstant((jdouble)cpool->double_at(index));
  } else if (tag.is_string()) {
    oop string = NULL;
    if (cpool->is_pseudo_string_at(index)) {
      string = cpool->pseudo_string_at(index, cache_index);
    } else {
      string = cpool->string_at(index, cache_index, THREAD);
      if (HAS_PENDING_EXCEPTION) {
        CLEAR_PENDING_EXCEPTION;
        record_out_of_memory_failure();
        return ciConstant();
      }
    }
    ciObject* constant = get_object(string);
    if (constant->is_array()) {
      return ciConstant(T_ARRAY, constant);
    } else {
      return ciConstant(T_OBJECT, constant);
    }
  } else if (tag.is_klass() || tag.is_unresolved_klass()) {
    // 4881222: allow ldc to take a class type
    ciKlass* klass = get_klass_by_index_impl(cpool, index, ignore_will_link, accessor);
    if (HAS_PENDING_EXCEPTION) {
      CLEAR_PENDING_EXCEPTION;
      record_out_of_memory_failure();
      return ciConstant();
    }
    return ciConstant(T_OBJECT, klass->java_mirror());
  } else if (tag.is_method_type()) {
    // must execute Java code to link this CP entry into cache[i].f1
    ciSymbol* signature = get_symbol(cpool->method_type_signature_at(index));
    ciObject* ciobj = get_unloaded_method_type_constant(signature);
    return ciConstant(T_OBJECT, ciobj);
  } else if (tag.is_method_handle()) {
    // must execute Java code to link this CP entry into cache[i].f1
    int ref_kind        = cpool->method_handle_ref_kind_at(index);
    int callee_index    = cpool->method_handle_klass_index_at(index);
    ciKlass* callee     = get_klass_by_index_impl(cpool, callee_index, ignore_will_link, accessor);
    ciSymbol* name      = get_symbol(cpool->method_handle_name_ref_at(index));
    ciSymbol* signature = get_symbol(cpool->method_handle_signature_ref_at(index));
    ciObject* ciobj     = get_unloaded_method_handle_constant(callee, name, signature, ref_kind);
    return ciConstant(T_OBJECT, ciobj);
  } else if (tag.is_dynamic_constant()) {
    return ciConstant();
  } else {
    ShouldNotReachHere();
    return ciConstant();
  }
}

// ------------------------------------------------------------------
// ciEnv::get_constant_by_index
//
// Pull a constant out of the constant pool.  How appropriate.
//
// Implementation note: this query is currently in no way cached.
ciConstant ciEnv::get_constant_by_index(const constantPoolHandle& cpool,
                                        int pool_index, int cache_index,
                                        ciInstanceKlass* accessor) {
  GUARDED_VM_ENTRY(return get_constant_by_index_impl(cpool, pool_index, cache_index, accessor);)
}

// ------------------------------------------------------------------
// ciEnv::get_field_by_index_impl
//
// Implementation of get_field_by_index.
//
// Implementation note: the results of field lookups are cached
// in the accessor klass.
ciField* ciEnv::get_field_by_index_impl(ciInstanceKlass* accessor, int index) {
  ciConstantPoolCache* cache = accessor->field_cache();
  if (cache == NULL) {
    ciField* field = new (arena()) ciField(accessor, index);
    return field;
  } else {
    ciField* field = (ciField*)cache->get(index);
    if (field == NULL) {
      field = new (arena()) ciField(accessor, index);
      cache->insert(index, field);
    }
    return field;
  }
}

// ------------------------------------------------------------------
// ciEnv::get_field_by_index
//
// Get a field by index from a klass's constant pool.
ciField* ciEnv::get_field_by_index(ciInstanceKlass* accessor, int index) {
  GUARDED_VM_ENTRY(return get_field_by_index_impl(accessor, index);)
}

// ------------------------------------------------------------------
// ciEnv::lookup_method
//
// Perform an appropriate method lookup based on accessor, holder,
// name, signature, and bytecode.
Method* ciEnv::lookup_method(ciInstanceKlass* accessor, ciKlass* holder, Symbol* name, Symbol* sig, Bytecodes::Code bc, constantTag tag) {
  InstanceKlass* accessor_klass = accessor->get_instanceKlass();
  Klass* holder_klass = holder->get_Klass();
  methodHandle dest_method;
  LinkInfo link_info(holder_klass, name, sig, accessor_klass, LinkInfo::needs_access_check, tag);
  switch (bc) {
  case Bytecodes::_invokestatic:
    dest_method = LinkResolver::resolve_static_call_or_null(link_info);
    break;
  case Bytecodes::_invokespecial:
    dest_method = LinkResolver::resolve_special_call_or_null(link_info);
    break;
  case Bytecodes::_invokeinterface:
    dest_method = LinkResolver::linktime_resolve_interface_method_or_null(link_info);
    break;
  case Bytecodes::_invokevirtual:
    dest_method = LinkResolver::linktime_resolve_virtual_method_or_null(link_info);
    break;
  default: ShouldNotReachHere();
  }

  return dest_method();
}

// ------------------------------------------------------------------
// ciEnv::get_method_by_index_impl
ciMethod* ciEnv::get_method_by_index_impl(const constantPoolHandle& cpool, int index, Bytecodes::Code bc, ciInstanceKlass* accessor) {
  if (bc == Bytecodes::_invokedynamic) {
    ConstantPoolCacheEntry* cpce = cpool->invokedynamic_cp_cache_entry_at(index);
    bool is_resolved = !cpce->is_f1_null();
    // FIXME: code generation could allow for null (unlinked) call site
    // The call site could be made patchable as follows:
    // Load the appendix argument from the constant pool.
    // Test the appendix argument and jump to a known deopt routine if it is null.
    // Jump through a patchable call site, which is initially a deopt routine.
    // Patch the call site to the nmethod entry point of the static compiled lambda form.
    // As with other two-component call sites, both values must be independently verified.

    if (is_resolved) {
      // Get the invoker Method* from the constant pool.
      // (The appendix argument, if any, will be noted in the method's signature.)
      Method* adapter = cpce->f1_as_method();
      return get_method(adapter);
    }

    // Fake a method that is equivalent to a declared method.
    ciInstanceKlass* holder    = get_instance_klass(SystemDictionary::MethodHandle_klass());
    ciSymbol*        name      = ciSymbol::invokeBasic_name();
    ciSymbol*        signature = get_symbol(cpool->signature_ref_at(index));
    return get_unloaded_method(holder, name, signature, accessor);
  } else {
    const int holder_index = cpool->klass_ref_index_at(index);
    bool holder_is_accessible;
    ciKlass* holder = get_klass_by_index_impl(cpool, holder_index, holder_is_accessible, accessor);

    // Get the method's name and signature.
    Symbol* name_sym = cpool->name_ref_at(index);
    Symbol* sig_sym  = cpool->signature_ref_at(index);

    if (cpool->has_preresolution() || ((holder == ciEnv::MethodHandle_klass() || holder == ciEnv::VarHandle_klass()) && MethodHandles::is_signature_polymorphic_name(holder->get_Klass(), name_sym))) {
      // Short-circuit lookups for JSR 292-related call sites.
      // That is, do not rely only on name-based lookups, because they may fail
      // if the names are not resolvable in the boot class loader (7056328).
      switch (bc) {
      case Bytecodes::_invokevirtual:
      case Bytecodes::_invokeinterface:
      case Bytecodes::_invokespecial:
      case Bytecodes::_invokestatic:
        {
          Method* m = ConstantPool::method_at_if_loaded(cpool, index);
          if (m != NULL) {
            return get_method(m);
          }
        }
        break;
      default:
        break;
      }
    }

    if (holder_is_accessible) {  // Our declared holder is loaded.
      constantTag tag = cpool->tag_ref_at(index);
      Method* m = lookup_method(accessor, holder, name_sym, sig_sym, bc, tag);
      if (m != NULL &&
          (bc == Bytecodes::_invokestatic
           ?  m->method_holder()->is_not_initialized()
           : !m->method_holder()->is_loaded())) {
        m = NULL;
      }
      if (m != NULL) {
        // We found the method.
        return get_method(m);
      }
    }

    // Either the declared holder was not loaded, or the method could
    // not be found.  Create a dummy ciMethod to represent the failed
    // lookup.
    ciSymbol* name      = get_symbol(name_sym);
    ciSymbol* signature = get_symbol(sig_sym);
    return get_unloaded_method(holder, name, signature, accessor);
  }
}

// ------------------------------------------------------------------
// ciEnv::get_instance_klass_for_declared_method_holder
ciInstanceKlass* ciEnv::get_instance_klass_for_declared_method_holder(ciKlass* method_holder) {
  // For the case of <array>.clone(), the method holder can be a ciArrayKlass
  // instead of a ciInstanceKlass.  For that case simply pretend that the
  // declared holder is Object.clone since that's where the call will bottom out.
  // A more correct fix would trickle out through many interfaces in CI,
  // requiring ciInstanceKlass* to become ciKlass* and many more places would
  // require checks to make sure the expected type was found.  Given that this
  // only occurs for clone() the more extensive fix seems like overkill so
  // instead we simply smear the array type into Object.
  guarantee(method_holder != NULL, "no method holder");
  if (method_holder->is_instance_klass()) {
    return method_holder->as_instance_klass();
  } else if (method_holder->is_array_klass()) {
    return current()->Object_klass();
  } else {
    ShouldNotReachHere();
  }
  return NULL;
}

// ------------------------------------------------------------------
// ciEnv::get_method_by_index
ciMethod* ciEnv::get_method_by_index(const constantPoolHandle& cpool, int index, Bytecodes::Code bc, ciInstanceKlass* accessor) {
  GUARDED_VM_ENTRY(return get_method_by_index_impl(cpool, index, bc, accessor);)
}

// ------------------------------------------------------------------
// ciEnv::name_buffer
char *ciEnv::name_buffer(int req_len) {
  if (_name_buffer_len < req_len) {
    if (_name_buffer == NULL) {
      _name_buffer = (char*)arena()->Amalloc(sizeof(char)*req_len);
      _name_buffer_len = req_len;
    } else {
      _name_buffer = (char*)arena()->Arealloc(_name_buffer, _name_buffer_len, req_len);
      _name_buffer_len = req_len;
    }
  }
  return _name_buffer;
}

// ------------------------------------------------------------------
// ciEnv::is_in_vm
bool ciEnv::is_in_vm() {
  return JavaThread::current()->thread_state() == _thread_in_vm;
}

bool ciEnv::system_dictionary_modification_counter_changed() {
  return _system_dictionary_modification_counter != SystemDictionary::number_of_modifications();
}

// ------------------------------------------------------------------
// ciEnv::validate_compile_task_dependencies
//
// Check for changes during compilation (e.g. class loads, evolution,
// breakpoints, call site invalidation).
void ciEnv::validate_compile_task_dependencies(ciMethod* target) {
  if (failing())  return;  // no need for further checks

  bool counter_changed = system_dictionary_modification_counter_changed();
  Dependencies::DepType result = dependencies()->validate_dependencies(_task, counter_changed);
  if (result != Dependencies::end_marker) {
    if (result == Dependencies::call_site_target_value) {
      _inc_decompile_count_on_failure = false;
      record_failure("call site target change");
    } else if (Dependencies::is_klass_type(result)) {
      record_failure("concurrent class loading");
    } else {
      record_failure("invalid non-klass dependency");
    }
  }
}

// ------------------------------------------------------------------
// ciEnv::register_method
void ciEnv::register_method(ciMethod* target, int entry_bci, CodeOffsets* offsets, int orig_pc_offset, CodeBuffer* code_buffer, int frame_words, OopMapSet* oop_map_set, ExceptionHandlerTable* handler_table, ImplicitExceptionTable* inc_table, AbstractCompiler* compiler, bool has_unsafe_access, bool has_wide_vectors, RTMState rtm_state) {
  VM_ENTRY_MARK;
  nmethod* nm = NULL;
  {
    // To prevent compile queue updates.
    MutexLocker locker(MethodCompileQueue_lock, THREAD);

    // Prevent SystemDictionary::add_to_hierarchy from running
    // and invalidating our dependencies until we install this method.
    // No safepoints are allowed. Otherwise, class redefinition can occur in between.
    MutexLocker ml(Compile_lock);
    NoSafepointVerifier nsv;

    if (!failing()) {
      // Encode the dependencies now, so we can check them right away.
      dependencies()->encode_content_bytes();

      // Check for {class loads, evolution, breakpoints, ...} during compilation
      validate_compile_task_dependencies(target);
    }

    methodHandle method(THREAD, target->get_Method());

#if INCLUDE_RTM_OPT
    if (!failing() && (rtm_state != NoRTM) && (method()->method_data() != NULL) && (method()->method_data()->rtm_state() != rtm_state)) {
      // Preemptive decompile if rtm state was changed.
      record_failure("RTM state change invalidated rtm code");
    }
#endif

    if (failing()) {
      // While not a true deoptimization, it is a preemptive decompile.
      MethodData* mdo = method()->method_data();
      if (mdo != NULL && _inc_decompile_count_on_failure) {
        mdo->inc_decompile_count();
      }

      // All buffers in the CodeBuffer are allocated in the CodeCache.
      // If the code buffer is created on each compile attempt
      // as in C2, then it must be freed.
      code_buffer->free_blob();
      return;
    }

    nm =  nmethod::new_nmethod(method, compile_id(), entry_bci, offsets, orig_pc_offset, debug_info(), dependencies(), code_buffer, frame_words, oop_map_set, handler_table, inc_table, compiler, task()->comp_level());

    // Free codeBlobs
    code_buffer->free_blob();

    if (nm != NULL) {
      nm->set_has_unsafe_access(has_unsafe_access);
      nm->set_has_wide_vectors(has_wide_vectors);
#if INCLUDE_RTM_OPT
      nm->set_rtm_state(rtm_state);
#endif

      // Record successful registration.
      // (Put nm into the task handle *before* publishing to the Java heap.)
      if (task() != NULL) {
        task()->set_code(nm);
      }

      if (entry_bci == InvocationEntryBci) {
        // Allow the code to be executed
        method->set_code(method, nm);
      } else {
        method->method_holder()->add_osr_nmethod(nm);
      }
      nm->make_in_use();
    }
  }

  if (nm != NULL) {
    // JVMTI -- compiled method notification (must be done outside lock)
    nm->post_compiled_method_load_event();
  } else {
    // The CodeCache is full.
    record_failure("code cache is full");
  }
}

// ------------------------------------------------------------------
// ciEnv::find_system_klass
ciKlass* ciEnv::find_system_klass(ciSymbol* klass_name) {
  VM_ENTRY_MARK;
  return get_klass_by_name_impl(NULL, constantPoolHandle(), klass_name, false);
}

// ------------------------------------------------------------------
// ciEnv::comp_level
int ciEnv::comp_level() {
  if (task() == NULL)  return CompLevel_highest_tier;
  return task()->comp_level();
}

// ------------------------------------------------------------------
// ciEnv::compile_id
uint ciEnv::compile_id() {
  if (task() == NULL)  return 0;
  return task()->compile_id();
}

// ------------------------------------------------------------------
// ciEnv::notice_inlined_method()
void ciEnv::notice_inlined_method(ciMethod* method) {
  _num_inlined_bytecodes += method->code_size_for_inlining();
}

// ------------------------------------------------------------------
// ciEnv::num_inlined_bytecodes()
int ciEnv::num_inlined_bytecodes() const {
  return _num_inlined_bytecodes;
}

// ------------------------------------------------------------------
// ciEnv::record_failure()
void ciEnv::record_failure(const char* reason) {
  if (_failure_reason == NULL) {
    // Record the first failure reason.
    _failure_reason = reason;
  }
}

// ------------------------------------------------------------------
// ciEnv::record_method_not_compilable()
void ciEnv::record_method_not_compilable(const char* reason, bool all_tiers) {
  int new_compilable = all_tiers ? MethodCompilable_never : MethodCompilable_not_at_tier;

  // Only note transitions to a worse state
  if (new_compilable > _compilable) {
    _compilable = new_compilable;

    // Reset failure reason; this one is more important.
    _failure_reason = NULL;
    record_failure(reason);
  }
}

// ------------------------------------------------------------------
// ciEnv::record_out_of_memory_failure()
void ciEnv::record_out_of_memory_failure() {
  // If memory is low, we stop compiling methods.
  record_method_not_compilable("out of memory");
}

ciInstance* ciEnv::unloaded_ciinstance() {
  GUARDED_VM_ENTRY(return _factory->get_unloaded_object_constant();)
}

// ------------------------------------------------------------------
// ciEnv::dump_replay_data*

// Don't change thread state and acquire any locks.
// Safe to call from VM error reporter.

void ciEnv::dump_compile_data(outputStream* out) {
  CompileTask* task = this->task();
  if (task) {
    Method* method = task->method();
    int entry_bci = task->osr_bci();
    int comp_level = task->comp_level();
    out->print("compile %s %s %s %d %d", method->klass_name()->as_quoted_ascii(), method->name()->as_quoted_ascii(), method->signature()->as_quoted_ascii(), entry_bci, comp_level);
    if (compiler_data() != NULL) {
      if (is_c2_compile(comp_level)) {
      } else if (is_c1_compile(comp_level)) {
        // Dump C1 inlining data.
        ((Compilation*)compiler_data())->dump_inline_data(out);
      }
    }
    out->cr();
  }
}

void ciEnv::dump_replay_data_unsafe(outputStream* out) {
  ResourceMark rm;

  GrowableArray<ciMetadata*>* objects = _factory->get_ci_metadata();
  out->print_cr("# %d ciObject found", objects->length());
  for (int i = 0; i < objects->length(); i++) {
    objects->at(i)->dump_replay_data(out);
  }
  dump_compile_data(out);
  out->flush();
}

void ciEnv::dump_replay_data(outputStream* out) {
  GUARDED_VM_ENTRY(
    MutexLocker ml(Compile_lock);
    dump_replay_data_unsafe(out);
  )
}

void ciEnv::dump_replay_data(int compile_id) {
  static char buffer[O_BUFLEN];
  int ret = jio_snprintf(buffer, O_BUFLEN, "replay_pid%p_compid%d.log", os::current_process_id(), compile_id);
  if (ret > 0) {
    int fd = open(buffer, O_RDWR | O_CREAT | O_TRUNC, 0666);
    if (fd != -1) {
      FILE* replay_data_file = os::open(fd, "w");
      if (replay_data_file != NULL) {
        fileStream replay_data_stream(replay_data_file, /*need_close=*/true);
        dump_replay_data(&replay_data_stream);
        tty->print_cr("# Compiler replay data is saved as: %s", buffer);
      } else {
        tty->print_cr("# Can't open file to dump replay data.");
      }
    }
  }
}

void ciEnv::dump_inline_data(int compile_id) {
  static char buffer[O_BUFLEN];
  int ret = jio_snprintf(buffer, O_BUFLEN, "inline_pid%p_compid%d.log", os::current_process_id(), compile_id);
  if (ret > 0) {
    int fd = open(buffer, O_RDWR | O_CREAT | O_TRUNC, 0666);
    if (fd != -1) {
      FILE* inline_data_file = os::open(fd, "w");
      if (inline_data_file != NULL) {
        fileStream replay_data_stream(inline_data_file, /*need_close=*/true);
        GUARDED_VM_ENTRY(
          MutexLocker ml(Compile_lock);
          dump_compile_data(&replay_data_stream);
        )
        replay_data_stream.flush();
        tty->print("# Compiler inline data is saved as: ");
        tty->print_cr("%s", buffer);
      } else {
        tty->print_cr("# Can't open file to dump inline data.");
      }
    }
  }
}
