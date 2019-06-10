#include "precompiled.hpp"

#include "classfile/resolutionErrors.hpp"
#include "interpreter/bytecodeStream.hpp"
#include "interpreter/bytecodes.hpp"
#include "interpreter/linkResolver.hpp"
#include "memory/metadataFactory.hpp"
#include "memory/metaspaceClosure.hpp"
#include "memory/resourceArea.hpp"
#include "memory/universe.hpp"
#include "oops/access.inline.hpp"
#include "oops/constantPool.inline.hpp"
#include "oops/cpCache.inline.hpp"
#include "oops/objArrayOop.inline.hpp"
#include "oops/oop.inline.hpp"
#include "runtime/atomic.hpp"
#include "runtime/handles.inline.hpp"
#include "runtime/orderAccess.hpp"
#include "utilities/macros.hpp"

// Implementation of ConstantPoolCacheEntry

void ConstantPoolCacheEntry::initialize_entry(int index) {
  _indices = index;
  _f1 = NULL;
  _f2 = _flags = 0;
}

void ConstantPoolCacheEntry::verify_just_initialized(bool f2_used) { }

void ConstantPoolCacheEntry::reinitialize(bool f2_used) {
  _indices &= cp_index_mask;
  _f1 = NULL;
  _flags = 0;
  if (!f2_used) {
    _f2 = 0;
  }
}

int ConstantPoolCacheEntry::make_flags(TosState state, int option_bits, int field_index_or_method_params) {
  int f = ((int)state << tos_state_shift) | option_bits | field_index_or_method_params;
  // Preserve existing flag bit values
  // The low bits are a field offset, or else the method parameter size.
  return (_flags | f);
}

void ConstantPoolCacheEntry::set_bytecode_1(Bytecodes::Code code) {
  // Need to flush pending stores here before bytecode is written.
  OrderAccess::release_store(&_indices, _indices | ((u_char)code << bytecode_1_shift));
}

void ConstantPoolCacheEntry::set_bytecode_2(Bytecodes::Code code) {
  // Need to flush pending stores here before bytecode is written.
  OrderAccess::release_store(&_indices, _indices | ((u_char)code << bytecode_2_shift));
}

// Sets f1, ordering with previous writes.
void ConstantPoolCacheEntry::release_set_f1(Metadata* f1) {
  OrderAccess::release_store(&_f1, f1);
}

void ConstantPoolCacheEntry::set_indy_resolution_failed() {
  OrderAccess::release_store(&_flags, _flags | (1 << indy_resolution_failed_shift));
}

// Note that concurrent update of both bytecodes can leave one of them
// reset to zero.  This is harmless; the interpreter will simply re-resolve
// the damaged entry.  More seriously, the memory synchronization is needed
// to flush other fields (f1, f2) completely to memory before the bytecodes
// are updated, lest other processors see a non-zero bytecode but zero f1/f2.
void ConstantPoolCacheEntry::set_field(Bytecodes::Code get_code, Bytecodes::Code put_code, Klass* field_holder, int field_index, int field_offset, TosState field_type, bool is_final, bool is_volatile, Klass* root_klass) {
  set_f1(field_holder);
  set_f2(field_offset);
  set_field_flags(field_type,
                  ((is_volatile ? 1 : 0) << is_volatile_shift) |
                  ((is_final    ? 1 : 0) << is_final_shift),
                  field_index);
  set_bytecode_1(get_code);
  set_bytecode_2(put_code);
}

void ConstantPoolCacheEntry::set_parameter_size(int value) {
  // This routine is called only in corner cases where the CPCE is not yet initialized.
  //
  // Setting the parameter size by itself is only safe if the
  // current value of _flags is 0, otherwise another thread may have
  // updated it and we don't want to overwrite that value.  Don't
  // bother trying to update it once it's nonzero but always make
  // sure that the final parameter size agrees with what was passed.
  if (_flags == 0) {
    intx newflags = (value & parameter_size_mask);
    Atomic::cmpxchg(newflags, &_flags, (intx)0);
  }
  guarantee(parameter_size() == value, "size must not change: parameter_size=%d, value=%d", parameter_size(), value);
}

void ConstantPoolCacheEntry::set_direct_or_vtable_call(Bytecodes::Code invoke_code, const methodHandle& method, int vtable_index, bool sender_is_interface) {
  bool is_vtable_call = (vtable_index >= 0);  // FIXME: split this method on this boolean

  int byte_no = -1;
  bool change_to_virtual = false;
  InstanceKlass* holder = NULL;  // have to declare this outside the switch
  switch (invoke_code) {
    case Bytecodes::_invokeinterface:
      holder = method->method_holder();
      // check for private interface method invocations
      if (vtable_index == Method::nonvirtual_vtable_index && holder->is_interface()) {
        // set_f2_as_vfinal_method checks if is_vfinal flag is true.
        set_method_flags(as_TosState(method->result_type()),
                         (                             1      << is_vfinal_shift) |
                         ((method->is_final_method() ? 1 : 0) << is_final_shift),
                         method()->size_of_parameters());
        set_f2_as_vfinal_method(method());
        byte_no = 2;
        set_f1(holder); // interface klass*
        break;
      } else {
        // We get here from ...::resolve_invoke when an invokeinterface instruction
        // links to a non-interface method (in Object). This can happen when an interface
        // redeclares an Object method (like CharSequence declaring toString()) or when
        // invokeinterface is used explicitly.
        // In that case, the method has no itable index and must be invoked as a virtual.
        // Set a flag to keep track of this corner case.
        change_to_virtual = true;

        // ...and fall through as if we were handling invokevirtual:
      }
    case Bytecodes::_invokevirtual:
      {
        if (!is_vtable_call) {
          // set_f2_as_vfinal_method checks if is_vfinal flag is true.
          set_method_flags(as_TosState(method->result_type()),
                           (                             1      << is_vfinal_shift) |
                           ((method->is_final_method() ? 1 : 0) << is_final_shift)  |
                           ((change_to_virtual         ? 1 : 0) << is_forced_virtual_shift),
                           method()->size_of_parameters());
          set_f2_as_vfinal_method(method());
        } else {
          set_method_flags(as_TosState(method->result_type()),
                           ((change_to_virtual ? 1 : 0) << is_forced_virtual_shift),
                           method()->size_of_parameters());
          set_f2(vtable_index);
        }
        byte_no = 2;
        break;
      }

    case Bytecodes::_invokespecial:
    case Bytecodes::_invokestatic:
      // Note:  Read and preserve the value of the is_vfinal flag on any
      // invokevirtual bytecode shared with this constant pool cache entry.
      // It is cheap and safe to consult is_vfinal() at all times.
      // Once is_vfinal is set, it must stay that way, lest we get a dangling oop.
      set_method_flags(as_TosState(method->result_type()),
                       ((is_vfinal()               ? 1 : 0) << is_vfinal_shift) |
                       ((method->is_final_method() ? 1 : 0) << is_final_shift),
                       method()->size_of_parameters());
      set_f1(method());
      byte_no = 1;
      break;
    default:
      ShouldNotReachHere();
      break;
  }

  if (byte_no == 1) {
    bool do_resolve = true;
    // Don't mark invokespecial to method as resolved if sender is an interface.  The receiver
    // has to be checked that it is a subclass of the current class every time this bytecode
    // is executed.
    if (invoke_code == Bytecodes::_invokespecial && sender_is_interface && method->name() != vmSymbols::object_initializer_name()) {
      do_resolve = false;
    }
    // Don't mark invokestatic to method as resolved if the holder class has not yet completed
    // initialization. An invokestatic must only proceed if the class is initialized, but if
    // we resolve it before then that class initialization check is skipped.
    if (invoke_code == Bytecodes::_invokestatic && !method->method_holder()->is_initialized()) {
      do_resolve = false;
    }
    if (do_resolve) {
      set_bytecode_1(invoke_code);
    }
  } else if (byte_no == 2) {
    if (change_to_virtual) {
      // NOTE: THIS IS A HACK - BE VERY CAREFUL!!!
      //
      // Workaround for the case where we encounter an invokeinterface, but we
      // should really have an _invokevirtual since the resolved method is a
      // virtual method in java.lang.Object. This is a corner case in the spec
      // but is presumably legal. javac does not generate this code.
      //
      // We do not set bytecode_1() to _invokeinterface, because that is the
      // bytecode # used by the interpreter to see if it is resolved.  In this
      // case, the method gets reresolved with caller for each interface call
      // because the actual selected method may not be public.
      //
      // We set bytecode_2() to _invokevirtual.
    } else {
      if (invoke_code == Bytecodes::_invokeinterface && (method->is_private() || method->is_final())) {
        // We set bytecode_1() to _invokeinterface, because that is the
        // bytecode # used by the interpreter to see if it is resolved.
        // We set bytecode_2() to _invokevirtual.
        set_bytecode_1(invoke_code);
      }
    }
    // set up for invokevirtual, even if linking for invokeinterface also:
    set_bytecode_2(Bytecodes::_invokevirtual);
  } else {
    ShouldNotReachHere();
  }
}

void ConstantPoolCacheEntry::set_direct_call(Bytecodes::Code invoke_code, const methodHandle& method, bool sender_is_interface) {
  int index = Method::nonvirtual_vtable_index;
  // index < 0; FIXME: inline and customize set_direct_or_vtable_call
  set_direct_or_vtable_call(invoke_code, method, index, sender_is_interface);
}

void ConstantPoolCacheEntry::set_vtable_call(Bytecodes::Code invoke_code, const methodHandle& method, int index) {
  // either the method is a miranda or its holder should accept the given index
  // index >= 0; FIXME: inline and customize set_direct_or_vtable_call
  set_direct_or_vtable_call(invoke_code, method, index, false);
}

void ConstantPoolCacheEntry::set_itable_call(Bytecodes::Code invoke_code, Klass* referenced_klass, const methodHandle& method, int index) {
  InstanceKlass* interf = method->method_holder();
  set_f1(referenced_klass);
  set_f2((intx)method());
  set_method_flags(as_TosState(method->result_type()), 0, method()->size_of_parameters());
  set_bytecode_1(Bytecodes::_invokeinterface);
}

bool ConstantPoolCacheEntry::save_and_throw_indy_exc(const constantPoolHandle& cpool, int cpool_index, int index, constantTag tag, TRAPS) {
  // Use the resolved_references() lock for this cpCache entry.
  // resolved_references are created for all classes with Invokedynamic, MethodHandle
  // or MethodType constant pool cache entries.
  objArrayHandle resolved_references(Thread::current(), cpool->resolved_references());
  ObjectLocker ol(resolved_references, THREAD);

  // if f1 is not null or the indy_resolution_failed flag is set then another
  // thread either succeeded in resolving the method or got a LinkageError
  // exception, before this thread was able to record its failure.  So, clear
  // this thread's exception and return false so caller can use the earlier
  // thread's result.
  if (!is_f1_null() || indy_resolution_failed()) {
    CLEAR_PENDING_EXCEPTION;
    return false;
  }

  Symbol* error = PENDING_EXCEPTION->klass()->name();
  Symbol* message = java_lang_Throwable::detail_message(PENDING_EXCEPTION);

  SystemDictionary::add_resolution_error(cpool, index, error, message);
  set_indy_resolution_failed();
  return true;
}

Method* ConstantPoolCacheEntry::method_if_resolved(const constantPoolHandle& cpool) {
  // Decode the action of set_method and set_interface_call
  Bytecodes::Code invoke_code = bytecode_1();
  if (invoke_code != (Bytecodes::Code)0) {
    Metadata* f1 = f1_ord();
    if (f1 != NULL) {
      switch (invoke_code) {
      case Bytecodes::_invokeinterface:
        return klassItable::method_for_itable_index((Klass*)f1, f2_as_index());
      case Bytecodes::_invokestatic:
      case Bytecodes::_invokespecial:
        return (Method*)f1;
      default:
        break;
      }
    }
  }
  invoke_code = bytecode_2();
  if (invoke_code != (Bytecodes::Code)0) {
    switch (invoke_code) {
    case Bytecodes::_invokevirtual:
      if (is_vfinal()) {
        // invokevirtual
        Method* m = f2_as_vfinal_method();
        return m;
      } else {
        int holder_index = cpool->uncached_klass_ref_index_at(constant_pool_index());
        if (cpool->tag_at(holder_index).is_klass()) {
          Klass* klass = cpool->resolved_klass_at(holder_index);
          return klass->method_at_vtable(f2_as_index());
        }
      }
      break;
    default:
      break;
    }
  }
  return NULL;
}

oop ConstantPoolCacheEntry::appendix_if_resolved(const constantPoolHandle& cpool) {
  if (!has_appendix())
    return NULL;
  const int ref_index = f2_as_index() + _indy_resolved_references_appendix_offset;
  objArrayOop resolved_references = cpool->resolved_references();
  return resolved_references->obj_at(ref_index);
}

void ConstantPoolCacheEntry::print(outputStream* st, int index) const {
  // print separator
  if (index == 0) st->print_cr("                 -------------");
  // print entry
  st->print("%3d  (" PTR_FORMAT ")  ", index, (intptr_t)this);
  st->print_cr("[%02x|%02x|%5d]", bytecode_2(), bytecode_1(),
               constant_pool_index());
  st->print_cr("                 [   " PTR_FORMAT "]", (intptr_t)_f1);
  st->print_cr("                 [   " PTR_FORMAT "]", (intptr_t)_f2);
  st->print_cr("                 [   " PTR_FORMAT "]", (intptr_t)_flags);
  st->print_cr("                 -------------");
}

ConstantPoolCache* ConstantPoolCache::allocate(ClassLoaderData* loader_data, const intStack& index_map, const intStack& invokedynamic_index_map, const intStack& invokedynamic_map, TRAPS) {
  const int length = index_map.length() + invokedynamic_index_map.length();
  int size = ConstantPoolCache::size(length);

  return new (loader_data, size, MetaspaceObj::ConstantPoolCacheType, THREAD)
    ConstantPoolCache(length, index_map, invokedynamic_index_map, invokedynamic_map);
}

void ConstantPoolCache::initialize(const intArray& inverse_index_map, const intArray& invokedynamic_inverse_index_map, const intArray& invokedynamic_references_map) {
  for (int i = 0; i < inverse_index_map.length(); i++) {
    ConstantPoolCacheEntry* e = entry_at(i);
    int original_index = inverse_index_map.at(i);
    e->initialize_entry(original_index);
  }

  // Append invokedynamic entries at the end
  int invokedynamic_offset = inverse_index_map.length();
  for (int i = 0; i < invokedynamic_inverse_index_map.length(); i++) {
    int offset = i + invokedynamic_offset;
    ConstantPoolCacheEntry* e = entry_at(offset);
    int original_index = invokedynamic_inverse_index_map.at(i);
    e->initialize_entry(original_index);
  }

  for (int ref = 0; ref < invokedynamic_references_map.length(); ref++) {
    const int cpci = invokedynamic_references_map.at(ref);
    if (cpci >= 0) {
      entry_at(cpci)->initialize_resolved_reference_index(ref);
      ref += ConstantPoolCacheEntry::_indy_resolved_references_entries - 1;  // skip extra entries
    }
  }
}

void ConstantPoolCache::verify_just_initialized() { }

void ConstantPoolCache::remove_unshareable_info() {
  walk_entries_for_initialization(/*check_only = */ false);
}

void ConstantPoolCache::walk_entries_for_initialization(bool check_only) {
  // When dumping the archive, we want to clean up the ConstantPoolCache
  // to remove any effect of linking due to the execution of Java code --
  // each ConstantPoolCacheEntry will have the same contents as if
  // ConstantPoolCache::initialize has just returned:
  //
  // - We keep the ConstantPoolCache::constant_pool_index() bits for all entries.
  // - We keep the "f2" field for entries used by invokedynamic and invokehandle
  // - All other bits in the entries are cleared to zero.
  ResourceMark rm;

  bool* f2_used = NEW_RESOURCE_ARRAY(bool, length());
  memset(f2_used, 0, sizeof(bool) * length());

  if (!check_only) {
    for (int i = 0; i < length(); i++) {
      entry_at(i)->reinitialize(f2_used[i]);
    }
  }
}

void ConstantPoolCache::deallocate_contents(ClassLoaderData* data) {
  data->remove_handle(_resolved_references);
  set_resolved_references(NULL);
  MetadataFactory::free_array<u2>(data, _reference_map);
  set_reference_map(NULL);
}

void ConstantPoolCache::metaspace_pointers_do(MetaspaceClosure* it) {
  it->push(&_constant_pool);
  it->push(&_reference_map);
}

// Printing

void ConstantPoolCache::print_on(outputStream* st) const {
  st->print_cr("%s", internal_name());
  // print constant pool cache entries
  for (int i = 0; i < length(); i++) entry_at(i)->print(st, i);
}

void ConstantPoolCache::print_value_on(outputStream* st) const {
  st->print("cache [%d]", length());
  print_address_on(st);
  st->print(" for ");
  constant_pool()->print_value_on(st);
}
