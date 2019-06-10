#include "precompiled.hpp"

#include "jvm.h"
#include "classfile/defaultMethods.hpp"
#include "classfile/javaClasses.hpp"
#include "classfile/resolutionErrors.hpp"
#include "classfile/symbolTable.hpp"
#include "classfile/systemDictionary.hpp"
#include "classfile/vmSymbols.hpp"
#include "compiler/compileBroker.hpp"
#include "gc/shared/collectedHeap.inline.hpp"
#include "interpreter/bytecode.hpp"
#include "interpreter/linkResolver.hpp"
#include "memory/resourceArea.hpp"
#include "memory/universe.hpp"
#include "oops/cpCache.inline.hpp"
#include "oops/instanceKlass.hpp"
#include "oops/method.hpp"
#include "oops/objArrayKlass.hpp"
#include "oops/objArrayOop.hpp"
#include "oops/oop.inline.hpp"
#include "prims/nativeLookup.hpp"
#include "runtime/compilationPolicy.hpp"
#include "runtime/fieldDescriptor.hpp"
#include "runtime/frame.inline.hpp"
#include "runtime/handles.inline.hpp"
#include "runtime/reflection.hpp"
#include "runtime/safepointVerifiers.hpp"
#include "runtime/signature.hpp"
#include "runtime/thread.inline.hpp"
#include "runtime/vmThread.hpp"

//------------------------------------------------------------------------------------------------------------------------
// Implementation of CallInfo

void CallInfo::set_static(Klass* resolved_klass, const methodHandle& resolved_method, TRAPS) {
  int vtable_index = Method::nonvirtual_vtable_index;
  set_common(resolved_klass, resolved_klass, resolved_method, resolved_method, CallInfo::direct_call, vtable_index, CHECK);
}

void CallInfo::set_interface(Klass* resolved_klass, Klass* selected_klass, const methodHandle& resolved_method, const methodHandle& selected_method, int itable_index, TRAPS) {
  set_common(resolved_klass, selected_klass, resolved_method, selected_method, CallInfo::itable_call, itable_index, CHECK);
}

void CallInfo::set_virtual(Klass* resolved_klass, Klass* selected_klass, const methodHandle& resolved_method, const methodHandle& selected_method, int vtable_index, TRAPS) {
  CallKind kind = (vtable_index >= 0 && !resolved_method->can_be_statically_bound() ? CallInfo::vtable_call : CallInfo::direct_call);
  set_common(resolved_klass, selected_klass, resolved_method, selected_method, kind, vtable_index, CHECK);
}

void CallInfo::set_common(Klass* resolved_klass, Klass* selected_klass, const methodHandle& resolved_method, const methodHandle& selected_method, CallKind kind, int index, TRAPS) {
  _resolved_klass  = resolved_klass;
  _selected_klass  = selected_klass;
  _resolved_method = resolved_method;
  _selected_method = selected_method;
  _call_kind       = kind;
  _call_index      = index;

  CompilationPolicy::compile_if_required(selected_method, THREAD);
}

// utility query for unreflecting a method
CallInfo::CallInfo(Method* resolved_method, Klass* resolved_klass, TRAPS) {
  Klass* resolved_method_holder = resolved_method->method_holder();
  if (resolved_klass == NULL) { // 2nd argument defaults to holder of 1st
    resolved_klass = resolved_method_holder;
  }
  _resolved_klass  = resolved_klass;
  _selected_klass  = resolved_klass;
  _resolved_method = resolved_method;
  _selected_method = resolved_method;
  // classify:
  CallKind kind = CallInfo::unknown_kind;
  int index = resolved_method->vtable_index();
  if (resolved_method->can_be_statically_bound()) {
    kind = CallInfo::direct_call;
  } else if (!resolved_method_holder->is_interface()) {
    // Could be an Object method inherited into an interface, but still a vtable call.
    kind = CallInfo::vtable_call;
  } else if (!resolved_klass->is_interface()) {
    // A default or miranda method.  Compute the vtable index.
    index = LinkResolver::vtable_index_of_interface_method(resolved_klass, resolved_method);

    kind = CallInfo::vtable_call;
  } else if (resolved_method->has_vtable_index()) {
    // Can occur if an interface redeclares a method of Object.

    kind = CallInfo::vtable_call;
  } else {
    // A regular interface call.
    kind = CallInfo::itable_call;
    index = resolved_method->itable_index();
  }
  _call_kind  = kind;
  _call_index = index;
}

//------------------------------------------------------------------------------------------------------------------------
// Implementation of LinkInfo

LinkInfo::LinkInfo(const constantPoolHandle& pool, int index, const methodHandle& current_method, TRAPS) {
   // resolve klass
  _resolved_klass = pool->klass_ref_at(index, CHECK);

  // Get name, signature, and static klass
  _name          = pool->name_ref_at(index);
  _signature     = pool->signature_ref_at(index);
  _tag           = pool->tag_ref_at(index);
  _current_klass = pool->pool_holder();
  _current_method = current_method;

  // Coming from the constant pool always checks access
  _check_access  = true;
}

LinkInfo::LinkInfo(const constantPoolHandle& pool, int index, TRAPS) {
   // resolve klass
  _resolved_klass = pool->klass_ref_at(index, CHECK);

  // Get name, signature, and static klass
  _name          = pool->name_ref_at(index);
  _signature     = pool->signature_ref_at(index);
  _tag           = pool->tag_ref_at(index);
  _current_klass = pool->pool_holder();
  _current_method = methodHandle();

  // Coming from the constant pool always checks access
  _check_access  = true;
}

char* LinkInfo::method_string() const {
  return Method::name_and_sig_as_C_string(_resolved_klass, _name, _signature);
}

//------------------------------------------------------------------------------------------------------------------------
// Klass resolution

void LinkResolver::check_klass_accessability(Klass* ref_klass, Klass* sel_klass, bool fold_type_to_class, TRAPS) {
  Klass* base_klass = sel_klass;
  if (fold_type_to_class) {
    if (sel_klass->is_objArray_klass()) {
      base_klass = ObjArrayKlass::cast(sel_klass)->bottom_klass();
    }
    // The element type could be a typeArray - we only need the access
    // check if it is a reference to another class.
    if (!base_klass->is_instance_klass()) {
      return;  // no relevant check to do
    }
  }
  Reflection::VerifyClassAccessResults vca_result = Reflection::verify_class_access(ref_klass, InstanceKlass::cast(base_klass), true);
  if (vca_result != Reflection::ACCESS_OK) {
    ResourceMark rm(THREAD);
    char* msg = Reflection::verify_class_access_msg(ref_klass,
                                                    InstanceKlass::cast(base_klass),
                                                    vca_result);
    bool same_module = (base_klass->module() == ref_klass->module());
    if (msg == NULL) {
      Exceptions::fthrow(
        THREAD_AND_LOCATION,
        vmSymbols::java_lang_IllegalAccessError(),
        "failed to access class %s from class %s (%s%s%s)",
        base_klass->external_name(),
        ref_klass->external_name(),
        (same_module) ? base_klass->joint_in_module_of_loader(ref_klass) : base_klass->class_in_module_of_loader(),
        (same_module) ? "" : "; ",
        (same_module) ? "" : ref_klass->class_in_module_of_loader());
    } else {
      // Use module specific message returned by verify_class_access_msg().
      Exceptions::fthrow(THREAD_AND_LOCATION, vmSymbols::java_lang_IllegalAccessError(), "%s", msg);
    }
  }
}

//------------------------------------------------------------------------------------------------------------------------
// Method resolution
//
// According to JVM spec. $5.4.3c & $5.4.3d

// Look up method in klasses, including static methods
// Then look up local default methods
Method* LinkResolver::lookup_method_in_klasses(const LinkInfo& link_info, bool in_imethod_resolve) {
  NoSafepointVerifier nsv;  // Method* returned may not be reclaimed

  Klass* klass = link_info.resolved_klass();
  Symbol* name = link_info.name();
  Symbol* signature = link_info.signature();

  // Ignore overpasses so statics can be found during resolution
  Method* result = klass->uncached_lookup_method(name, signature, Klass::skip_overpass);

  if (klass->is_array_klass()) {
    // Only consider klass and super klass for arrays
    return result;
  }

  InstanceKlass* ik = InstanceKlass::cast(klass);

  // JDK 8, JVMS 5.4.3.4: Interface method resolution should
  // ignore static and non-public methods of java.lang.Object,
  // like clone, finalize, registerNatives.
  if (in_imethod_resolve && result != NULL && ik->is_interface() && (result->is_static() || !result->is_public()) && result->method_holder() == SystemDictionary::Object_klass()) {
    result = NULL;
  }

  // Before considering default methods, check for an overpass in the
  // current class if a method has not been found.
  if (result == NULL) {
    result = ik->find_method(name, signature);
  }

  if (result == NULL) {
    Array<Method*>* default_methods = ik->default_methods();
    if (default_methods != NULL) {
      result = InstanceKlass::find_method(default_methods, name, signature);
    }
  }

  return result;
}

// returns first instance method
// Looks up method in classes, then looks up local default methods
methodHandle LinkResolver::lookup_instance_method_in_klasses(Klass* klass, Symbol* name, Symbol* signature, Klass::PrivateLookupMode private_mode, TRAPS) {
  Method* result = klass->uncached_lookup_method(name, signature, Klass::find_overpass, private_mode);

  while (result != NULL && result->is_static() && result->method_holder()->super() != NULL) {
    Klass* super_klass = result->method_holder()->super();
    result = super_klass->uncached_lookup_method(name, signature, Klass::find_overpass, private_mode);
  }

  if (klass->is_array_klass()) {
    // Only consider klass and super klass for arrays
    return methodHandle(THREAD, result);
  }

  if (result == NULL) {
    Array<Method*>* default_methods = InstanceKlass::cast(klass)->default_methods();
    if (default_methods != NULL) {
      result = InstanceKlass::find_method(default_methods, name, signature);
    }
  }
  return methodHandle(THREAD, result);
}

int LinkResolver::vtable_index_of_interface_method(Klass* klass, const methodHandle& resolved_method) {
  int vtable_index = Method::invalid_vtable_index;
  Symbol* name = resolved_method->name();
  Symbol* signature = resolved_method->signature();
  InstanceKlass* ik = InstanceKlass::cast(klass);

  // First check in default method array
  if (!resolved_method->is_abstract() && ik->default_methods() != NULL) {
    int index = InstanceKlass::find_method_index(ik->default_methods(), name, signature, Klass::find_overpass, Klass::find_static, Klass::find_private);
    if (index >= 0) {
      vtable_index = ik->default_vtable_indices()->at(index);
    }
  }
  if (vtable_index == Method::invalid_vtable_index) {
    // get vtable_index for miranda methods
    klassVtable vt = ik->vtable();
    vtable_index = vt.index_of_miranda(name, signature);
  }
  return vtable_index;
}

Method* LinkResolver::lookup_method_in_interfaces(const LinkInfo& cp_info) {
  InstanceKlass *ik = InstanceKlass::cast(cp_info.resolved_klass());

  // Specify 'true' in order to skip default methods when searching the interfaces.
  // Function lookup_method_in_klasses() already looked for the method in the default methods table.
  return ik->lookup_method_in_all_interfaces(cp_info.name(), cp_info.signature(), Klass::skip_defaults);
}

void LinkResolver::check_method_accessability(Klass* ref_klass, Klass* resolved_klass, Klass* sel_klass, const methodHandle& sel_method, TRAPS) {
  AccessFlags flags = sel_method->access_flags();

  // Special case:  arrays always override "clone". JVMS 2.15.
  // If the resolved klass is an array class, and the declaring class
  // is java.lang.Object and the method is "clone", set the flags
  // to public.
  //
  // We'll check for the method name first, as that's most likely
  // to be false (so we'll short-circuit out of these tests).
  if (sel_method->name() == vmSymbols::clone_name() && sel_klass == SystemDictionary::Object_klass() && resolved_klass->is_array_klass()) {
    jint new_flags = flags.as_int();
    new_flags = new_flags & (~JVM_ACC_PROTECTED);
    new_flags = new_flags | JVM_ACC_PUBLIC;
    flags.set_flags(new_flags);
  }

  bool can_access = Reflection::verify_member_access(ref_klass, resolved_klass, sel_klass, flags, true, false, CHECK);
  // Any existing exceptions that may have been thrown, for example LinkageErrors
  // from nest-host resolution, have been allowed to propagate.
  if (!can_access) {
    ResourceMark rm(THREAD);
    bool same_module = (sel_klass->module() == ref_klass->module());
    Exceptions::fthrow(
      THREAD_AND_LOCATION,
      vmSymbols::java_lang_IllegalAccessError(),
      "class %s tried to access %s%s%smethod %s.%s%s (%s%s%s)",
      ref_klass->external_name(),
      sel_method->is_abstract()  ? "abstract "  : "",
      sel_method->is_protected() ? "protected " : "",
      sel_method->is_private()   ? "private "   : "",
      sel_klass->external_name(),
      sel_method->name()->as_C_string(),
      sel_method->signature()->as_C_string(),
      (same_module) ? ref_klass->joint_in_module_of_loader(sel_klass) : ref_klass->class_in_module_of_loader(),
      (same_module) ? "" : "; ",
      (same_module) ? "" : sel_klass->class_in_module_of_loader()
    );
    return;
  }
}

methodHandle LinkResolver::resolve_method_statically(Bytecodes::Code code, const constantPoolHandle& pool, int index, TRAPS) {
  // This method is used only
  // (1) in C2 from InlineTree::ok_to_inline (via ciMethod::check_call),
  // and
  // (2) in Bytecode_invoke::static_target
  LinkInfo link_info(pool, index, methodHandle(), CHECK_NULL);
  Klass* resolved_klass = link_info.resolved_klass();

  if (pool->has_preresolution()) {
    Method* result = ConstantPool::method_at_if_loaded(pool, index);
    if (result != NULL) {
      return methodHandle(THREAD, result);
    }
  }

  if (code == Bytecodes::_invokeinterface) {
    return resolve_interface_method(link_info, code, THREAD);
  } else if (code == Bytecodes::_invokevirtual) {
    return resolve_method(link_info, code, THREAD);
  } else if (!resolved_klass->is_interface()) {
    return resolve_method(link_info, code, THREAD);
  } else {
    return resolve_interface_method(link_info, code, THREAD);
  }
}

// Check and print a loader constraint violation message for method or interface method
void LinkResolver::check_method_loader_constraints(const LinkInfo& link_info, const methodHandle& resolved_method, const char* method_type, TRAPS) {
  Handle current_loader(THREAD, link_info.current_klass()->class_loader());
  Handle resolved_loader(THREAD, resolved_method->method_holder()->class_loader());

  ResourceMark rm(THREAD);
  Symbol* failed_type_symbol = SystemDictionary::check_signature_loaders(link_info.signature(), current_loader, resolved_loader, true, CHECK);
  if (failed_type_symbol != NULL) {
    const char* msg = "loader constraint violation: when resolving %s"
      " \"%s\" the class loader %s of the current class, %s,"
      " and the class loader %s for the method's defining class, %s, have"
      " different Class objects for the type %s used in the signature";
    char* sig = link_info.method_string();
    const char* loader1_name = java_lang_ClassLoader::describe_external(current_loader());
    char* current = link_info.current_klass()->name()->as_C_string();
    const char* loader2_name = java_lang_ClassLoader::describe_external(resolved_loader());
    char* target = resolved_method->method_holder()->name()->as_C_string();
    char* failed_type_name = failed_type_symbol->as_C_string();
    size_t buflen = strlen(msg) + strlen(sig) + strlen(loader1_name) + strlen(current) + strlen(loader2_name) + strlen(target) + strlen(failed_type_name) + strlen(method_type) + 1;
    char* buf = NEW_RESOURCE_ARRAY_IN_THREAD(THREAD, char, buflen);
    jio_snprintf(buf, buflen, msg, method_type, sig, loader1_name, current, loader2_name, target, failed_type_name);
    THROW_MSG(vmSymbols::java_lang_LinkageError(), buf);
  }
}

void LinkResolver::check_field_loader_constraints(Symbol* field, Symbol* sig, Klass* current_klass, Klass* sel_klass, TRAPS) {
  Handle ref_loader(THREAD, current_klass->class_loader());
  Handle sel_loader(THREAD, sel_klass->class_loader());

  ResourceMark rm(THREAD);  // needed for check_signature_loaders
  Symbol* failed_type_symbol = SystemDictionary::check_signature_loaders(sig, ref_loader, sel_loader, false, CHECK);
  if (failed_type_symbol != NULL) {
    const char* msg = "loader constraint violation: when resolving field"
      " \"%s\" of type %s, the class loader %s of the current class, "
      "%s, and the class loader %s for the field's defining "
      "type, %s, have different Class objects for type %s";
    const char* field_name = field->as_C_string();
    const char* loader1_name = java_lang_ClassLoader::describe_external(ref_loader());
    const char* sel = sel_klass->external_name();
    const char* loader2_name = java_lang_ClassLoader::describe_external(sel_loader());
    const char* failed_type_name = failed_type_symbol->as_klass_external_name();
    const char* curr_klass_name = current_klass->external_name();
    size_t buflen = strlen(msg) + strlen(field_name) + 2 * strlen(failed_type_name) + strlen(loader1_name) + strlen(curr_klass_name) + strlen(loader2_name) + strlen(sel) + 1;
    char* buf = NEW_RESOURCE_ARRAY_IN_THREAD(THREAD, char, buflen);
    jio_snprintf(buf, buflen, msg, field_name, failed_type_name, loader1_name, curr_klass_name, loader2_name, sel, failed_type_name);
    THROW_MSG(vmSymbols::java_lang_LinkageError(), buf);
  }
}

methodHandle LinkResolver::resolve_method(const LinkInfo& link_info, Bytecodes::Code code, TRAPS) {
  Handle nested_exception;
  Klass* resolved_klass = link_info.resolved_klass();

  // 1. For invokevirtual, cannot call an interface method
  if (code == Bytecodes::_invokevirtual && resolved_klass->is_interface()) {
    ResourceMark rm(THREAD);
    char buf[200];
    jio_snprintf(buf, sizeof(buf), "Found interface %s, but class was expected", resolved_klass->external_name());
    THROW_MSG_NULL(vmSymbols::java_lang_IncompatibleClassChangeError(), buf);
  }

  // 2. check constant pool tag for called method - must be JVM_CONSTANT_Methodref
  if (!link_info.tag().is_invalid() && !link_info.tag().is_method()) {
    ResourceMark rm(THREAD);
    char buf[200];
    jio_snprintf(buf, sizeof(buf), "Method %s must be Methodref constant", link_info.method_string());
    THROW_MSG_NULL(vmSymbols::java_lang_IncompatibleClassChangeError(), buf);
  }

  // 3. lookup method in resolved klass and its super klasses
  methodHandle resolved_method(THREAD, lookup_method_in_klasses(link_info, false));

  // 4. lookup method in all the interfaces implemented by the resolved klass
  if (resolved_method.is_null() && !resolved_klass->is_array_klass()) { // not found in the class hierarchy
    resolved_method = methodHandle(THREAD, lookup_method_in_interfaces(link_info));
  }

  // 5. method lookup failed
  if (resolved_method.is_null()) {
    ResourceMark rm(THREAD);
    THROW_MSG_CAUSE_(vmSymbols::java_lang_NoSuchMethodError(), Method::name_and_sig_as_C_string(resolved_klass, link_info.name(), link_info.signature()), nested_exception, NULL);
  }

  // 6. access checks, access checking may be turned off when calling from within the VM.
  Klass* current_klass = link_info.current_klass();
  if (link_info.check_access()) {
    // check if method can be accessed by the referring class
    check_method_accessability(current_klass, resolved_klass, resolved_method->method_holder(), resolved_method, CHECK_NULL);

    // check loader constraints
    check_method_loader_constraints(link_info, resolved_method, "method", CHECK_NULL);
  }

  return resolved_method;
}

static void trace_method_resolution(const char* prefix, Klass* klass, Klass* resolved_klass, const methodHandle& method, bool logitables, int index = -1) { }

// Do linktime resolution of a method in the interface within the context of the specied bytecode.
methodHandle LinkResolver::resolve_interface_method(const LinkInfo& link_info, Bytecodes::Code code, TRAPS) {
  Klass* resolved_klass = link_info.resolved_klass();

  // check if klass is interface
  if (!resolved_klass->is_interface()) {
    ResourceMark rm(THREAD);
    char buf[200];
    jio_snprintf(buf, sizeof(buf), "Found class %s, but interface was expected", resolved_klass->external_name());
    THROW_MSG_NULL(vmSymbols::java_lang_IncompatibleClassChangeError(), buf);
  }

  // check constant pool tag for called method - must be JVM_CONSTANT_InterfaceMethodref
  if (!link_info.tag().is_invalid() && !link_info.tag().is_interface_method()) {
    ResourceMark rm(THREAD);
    char buf[200];
    jio_snprintf(buf, sizeof(buf), "Method %s must be InterfaceMethodref constant", link_info.method_string());
    THROW_MSG_NULL(vmSymbols::java_lang_IncompatibleClassChangeError(), buf);
  }

  // lookup method in this interface or its super, java.lang.Object
  // JDK8: also look for static methods
  methodHandle resolved_method(THREAD, lookup_method_in_klasses(link_info, true));

  if (resolved_method.is_null() && !resolved_klass->is_array_klass()) {
    // lookup method in all the super-interfaces
    resolved_method = methodHandle(THREAD, lookup_method_in_interfaces(link_info));
  }

  if (resolved_method.is_null()) {
    // no method found
    ResourceMark rm(THREAD);
    THROW_MSG_NULL(vmSymbols::java_lang_NoSuchMethodError(),
                   Method::name_and_sig_as_C_string(resolved_klass,
                                                    link_info.name(),
                                                    link_info.signature()));
  }

  if (link_info.check_access()) {
    // JDK8 adds non-public interface methods, and accessability check requirement
    Klass* current_klass = link_info.current_klass();

    // check if method can be accessed by the referring class
    check_method_accessability(current_klass, resolved_klass, resolved_method->method_holder(), resolved_method, CHECK_NULL);

    check_method_loader_constraints(link_info, resolved_method, "interface method", CHECK_NULL);
  }

  if (code != Bytecodes::_invokestatic && resolved_method->is_static()) {
    ResourceMark rm(THREAD);
    char buf[200];
    jio_snprintf(buf, sizeof(buf), "Expected instance not static method %s", Method::name_and_sig_as_C_string(resolved_klass, resolved_method->name(), resolved_method->signature()));
    THROW_MSG_NULL(vmSymbols::java_lang_IncompatibleClassChangeError(), buf);
  }

  return resolved_method;
}

//------------------------------------------------------------------------------------------------------------------------
// Field resolution

void LinkResolver::check_field_accessability(Klass* ref_klass, Klass* resolved_klass, Klass* sel_klass, const fieldDescriptor& fd, TRAPS) {
  bool can_access = Reflection::verify_member_access(ref_klass, resolved_klass, sel_klass, fd.access_flags(), true, false, CHECK);
  // Any existing exceptions that may have been thrown, for example LinkageErrors
  // from nest-host resolution, have been allowed to propagate.
  if (!can_access) {
    bool same_module = (sel_klass->module() == ref_klass->module());
    ResourceMark rm(THREAD);
    Exceptions::fthrow(
      THREAD_AND_LOCATION,
      vmSymbols::java_lang_IllegalAccessError(),
      "class %s tried to access %s%sfield %s.%s (%s%s%s)",
      ref_klass->external_name(),
      fd.is_protected() ? "protected " : "",
      fd.is_private()   ? "private "   : "",
      sel_klass->external_name(),
      fd.name()->as_C_string(),
      (same_module) ? ref_klass->joint_in_module_of_loader(sel_klass) : ref_klass->class_in_module_of_loader(),
      (same_module) ? "" : "; ",
      (same_module) ? "" : sel_klass->class_in_module_of_loader()
    );
    return;
  }
}

void LinkResolver::resolve_field_access(fieldDescriptor& fd, const constantPoolHandle& pool, int index, const methodHandle& method, Bytecodes::Code byte, TRAPS) {
  LinkInfo link_info(pool, index, method, CHECK);
  resolve_field(fd, link_info, byte, true, CHECK);
}

void LinkResolver::resolve_field(fieldDescriptor& fd, const LinkInfo& link_info, Bytecodes::Code byte, bool initialize_class, TRAPS) {
  bool is_static = (byte == Bytecodes::_getstatic || byte == Bytecodes::_putstatic);
  bool is_put    = (byte == Bytecodes::_putfield  || byte == Bytecodes::_putstatic);
  // Check if there's a resolved klass containing the field
  Klass* resolved_klass = link_info.resolved_klass();
  Symbol* field = link_info.name();
  Symbol* sig = link_info.signature();

  if (resolved_klass == NULL) {
    ResourceMark rm(THREAD);
    THROW_MSG(vmSymbols::java_lang_NoSuchFieldError(), field->as_C_string());
  }

  // Resolve instance field
  Klass* sel_klass = resolved_klass->find_field(field, sig, &fd);
  // check if field exists; i.e., if a klass containing the field def has been selected
  if (sel_klass == NULL) {
    ResourceMark rm(THREAD);
    THROW_MSG(vmSymbols::java_lang_NoSuchFieldError(), field->as_C_string());
  }

  // Access checking may be turned off when calling from within the VM.
  Klass* current_klass = link_info.current_klass();
  if (link_info.check_access()) {
    // check access
    check_field_accessability(current_klass, resolved_klass, sel_klass, fd, CHECK);

    // check for errors
    if (is_static != fd.is_static()) {
      ResourceMark rm(THREAD);
      char msg[200];
      jio_snprintf(msg, sizeof(msg), "Expected %s field %s.%s", is_static ? "static" : "non-static", resolved_klass->external_name(), fd.name()->as_C_string());
      THROW_MSG(vmSymbols::java_lang_IncompatibleClassChangeError(), msg);
    }

    // A final field can be modified only
    // (1) by methods declared in the class declaring the field and
    // (2) by the <clinit> method (in case of a static field)
    //     or by the <init> method (in case of an instance field).
    if (is_put && fd.access_flags().is_final()) {
      ResourceMark rm(THREAD);
      stringStream ss;

      if (sel_klass != current_klass) {
        ss.print("Update to %s final field %s.%s attempted from a different class (%s) than the field's declaring class",
                 is_static ? "static" : "non-static", resolved_klass->external_name(), fd.name()->as_C_string(),
                current_klass->external_name());
        THROW_MSG(vmSymbols::java_lang_IllegalAccessError(), ss.as_string());
      }

      if (fd.constants()->pool_holder()->major_version() >= 53) {
        methodHandle m = link_info.current_method();
        bool is_initialized_static_final_update = (byte == Bytecodes::_putstatic && fd.is_static() && !m()->is_static_initializer());
        bool is_initialized_instance_final_update = (byte == Bytecodes::_putfield && !fd.is_static() && !m->is_object_initializer());

        if (is_initialized_static_final_update || is_initialized_instance_final_update) {
          ss.print("Update to %s final field %s.%s attempted from a different method (%s) than the initializer method %s ",
                   is_static ? "static" : "non-static", resolved_klass->external_name(), fd.name()->as_C_string(),
                   m()->name()->as_C_string(),
                   is_static ? "<clinit>" : "<init>");
          THROW_MSG(vmSymbols::java_lang_IllegalAccessError(), ss.as_string());
        }
      }
    }

    // initialize resolved_klass if necessary
    // note 1: the klass which declared the field must be initialized (i.e, sel_klass)
    //         according to the newest JVM spec (5.5, p.170) - was bug (gri 7/28/99)
    //
    // note 2: we don't want to force initialization if we are just checking
    //         if the field access is legal; e.g., during compilation
    if (is_static && initialize_class) {
      sel_klass->initialize(CHECK);
    }
  }

  if ((sel_klass != current_klass) && (current_klass != NULL)) {
    check_field_loader_constraints(field, sig, current_klass, sel_klass, CHECK);
  }

  // return information. note that the klass is set to the actual klass containing the
  // field, otherwise access of static fields in superclasses will not work.
}

//------------------------------------------------------------------------------------------------------------------------
// Invoke resolution
//
// Naming conventions:
//
// resolved_method    the specified method (i.e., static receiver specified via constant pool index)
// sel_method         the selected method  (selected via run-time lookup; e.g., based on dynamic receiver class)
// resolved_klass     the specified klass  (i.e., specified via constant pool index)
// recv_klass         the receiver klass

void LinkResolver::resolve_static_call(CallInfo& result, const LinkInfo& link_info, bool initialize_class, TRAPS) {
  methodHandle resolved_method = linktime_resolve_static_method(link_info, CHECK);

  // The resolved class can change as a result of this resolution.
  Klass* resolved_klass = resolved_method->method_holder();

  // Initialize klass (this should only happen if everything is ok)
  if (initialize_class && resolved_klass->should_be_initialized()) {
    resolved_klass->initialize(CHECK);
    // Use updated LinkInfo to reresolve with resolved method holder
    LinkInfo new_info(resolved_klass, link_info.name(), link_info.signature(),
                      link_info.current_klass(),
                      link_info.check_access() ? LinkInfo::needs_access_check : LinkInfo::skip_access_check);
    resolved_method = linktime_resolve_static_method(new_info, CHECK);
  }

  // setup result
  result.set_static(resolved_klass, resolved_method, CHECK);
}

// throws linktime exceptions
methodHandle LinkResolver::linktime_resolve_static_method(const LinkInfo& link_info, TRAPS) {
  Klass* resolved_klass = link_info.resolved_klass();
  methodHandle resolved_method;
  if (!resolved_klass->is_interface()) {
    resolved_method = resolve_method(link_info, Bytecodes::_invokestatic, CHECK_NULL);
  } else {
    resolved_method = resolve_interface_method(link_info, Bytecodes::_invokestatic, CHECK_NULL);
  }

  // check if static
  if (!resolved_method->is_static()) {
    ResourceMark rm(THREAD);
    char buf[200];
    jio_snprintf(buf, sizeof(buf), "Expected static method %s", Method::name_and_sig_as_C_string(resolved_klass, resolved_method->name(), resolved_method->signature()));
    THROW_MSG_NULL(vmSymbols::java_lang_IncompatibleClassChangeError(), buf);
  }
  return resolved_method;
}

void LinkResolver::resolve_special_call(CallInfo& result, Handle recv, const LinkInfo& link_info, TRAPS) {
  methodHandle resolved_method = linktime_resolve_special_method(link_info, CHECK);
  runtime_resolve_special_method(result, link_info, resolved_method, recv, CHECK);
}

// throws linktime exceptions
methodHandle LinkResolver::linktime_resolve_special_method(const LinkInfo& link_info, TRAPS) {
  // Invokespecial is called for multiple special reasons:
  // <init>
  // local private method invocation, for classes and interfaces
  // superclass.method, which can also resolve to a default method
  // and the selected method is recalculated relative to the direct superclass
  // superinterface.method, which explicitly does not check shadowing
  Klass* resolved_klass = link_info.resolved_klass();
  methodHandle resolved_method;

  if (!resolved_klass->is_interface()) {
    resolved_method = resolve_method(link_info, Bytecodes::_invokespecial, CHECK_NULL);
  } else {
    resolved_method = resolve_interface_method(link_info, Bytecodes::_invokespecial, CHECK_NULL);
  }

  // check if method name is <init>, that it is found in same klass as static type
  if (resolved_method->name() == vmSymbols::object_initializer_name() && resolved_method->method_holder() != resolved_klass) {
    ResourceMark rm(THREAD);
    Exceptions::fthrow(
      THREAD_AND_LOCATION,
      vmSymbols::java_lang_NoSuchMethodError(),
      "%s: method %s%s not found",
      resolved_klass->external_name(),
      resolved_method->name()->as_C_string(),
      resolved_method->signature()->as_C_string()
    );
    return NULL;
  }

  // ensure that invokespecial's interface method reference is in
  // a direct superinterface, not an indirect superinterface
  Klass* current_klass = link_info.current_klass();
  if (current_klass != NULL && resolved_klass->is_interface()) {
    InstanceKlass* ck = InstanceKlass::cast(current_klass);
    InstanceKlass *klass_to_check = !ck->is_anonymous() ? ck : InstanceKlass::cast(ck->host_klass());
    // Disable verification for the dynamically-generated reflection bytecodes.
    bool is_reflect = klass_to_check->is_subclass_of(SystemDictionary::reflect_MagicAccessorImpl_klass());

    if (!is_reflect && !klass_to_check->is_same_or_direct_interface(resolved_klass)) {
      ResourceMark rm(THREAD);
      char buf[200];
      jio_snprintf(buf, sizeof(buf), "Interface method reference: %s, is in an indirect superinterface of %s",
                   Method::name_and_sig_as_C_string(resolved_klass, resolved_method->name(), resolved_method->signature()),
                   current_klass->external_name());
      THROW_MSG_NULL(vmSymbols::java_lang_IncompatibleClassChangeError(), buf);
    }
  }

  // check if not static
  if (resolved_method->is_static()) {
    ResourceMark rm(THREAD);
    char buf[200];
    jio_snprintf(buf, sizeof(buf), "Expecting non-static method %s", Method::name_and_sig_as_C_string(resolved_klass, resolved_method->name(), resolved_method->signature()));
    THROW_MSG_NULL(vmSymbols::java_lang_IncompatibleClassChangeError(), buf);
  }

  return resolved_method;
}

// throws runtime exceptions
void LinkResolver::runtime_resolve_special_method(CallInfo& result, const LinkInfo& link_info, const methodHandle& resolved_method, Handle recv, TRAPS) {
  Klass* resolved_klass = link_info.resolved_klass();

  // resolved method is selected method unless we have an old-style lookup
  // for a superclass method
  // Invokespecial for a superinterface, resolved method is selected method,
  // no checks for shadowing
  methodHandle sel_method(THREAD, resolved_method());

  if (link_info.check_access() &&
      // check if the method is not <init>
      resolved_method->name() != vmSymbols::object_initializer_name()) {
     // check if this is an old-style super call and do a new lookup if so
     // a) check if ACC_SUPER flag is set for the current class
    Klass* current_klass = link_info.current_klass();
    if ((current_klass->is_super() || !AllowNonVirtualCalls) &&
        // b) check if the class of the resolved_klass is a superclass
        // (not supertype in order to exclude interface classes) of the current class.
        // This check is not performed for super.invoke for interface methods
        // in super interfaces.
        current_klass->is_subclass_of(resolved_klass) &&
        current_klass != resolved_klass) {
      // Lookup super method
      Klass* super_klass = current_klass->super();
      sel_method = lookup_instance_method_in_klasses(super_klass, resolved_method->name(), resolved_method->signature(), Klass::find_private, CHECK);
      // check if found
      if (sel_method.is_null()) {
        ResourceMark rm(THREAD);
        THROW_MSG(vmSymbols::java_lang_AbstractMethodError(), Method::name_and_sig_as_C_string(resolved_klass, resolved_method->name(), resolved_method->signature()));
      // check loader constraints if found a different method
      } else if (sel_method() != resolved_method()) {
        check_method_loader_constraints(link_info, sel_method, "method", CHECK);
      }
    }

    // Check that the class of objectref (the receiver) is the current class or interface,
    // or a subtype of the current class or interface (the sender), otherwise invokespecial
    // throws IllegalAccessError.
    // The verifier checks that the sender is a subtype of the class in the I/MR operand.
    // The verifier also checks that the receiver is a subtype of the sender, if the sender is
    // a class.  If the sender is an interface, the check has to be performed at runtime.
    InstanceKlass* sender = InstanceKlass::cast(current_klass);
    sender = sender->is_anonymous() ? sender->host_klass() : sender;
    if (sender->is_interface() && recv.not_null()) {
      Klass* receiver_klass = recv->klass();
      if (!receiver_klass->is_subtype_of(sender)) {
        ResourceMark rm(THREAD);
        char buf[500];
        jio_snprintf(buf, sizeof(buf), "Receiver class %s must be the current class or a subtype of interface %s", receiver_klass->name()->as_C_string(), sender->name()->as_C_string());
        THROW_MSG(vmSymbols::java_lang_IllegalAccessError(), buf);
      }
    }
  }

  // check if not static
  if (sel_method->is_static()) {
    ResourceMark rm(THREAD);
    char buf[200];
    jio_snprintf(buf, sizeof(buf), "Expecting non-static method %s", Method::name_and_sig_as_C_string(resolved_klass, resolved_method->name(), resolved_method->signature()));
    THROW_MSG(vmSymbols::java_lang_IncompatibleClassChangeError(), buf);
  }

  // check if abstract
  if (sel_method->is_abstract()) {
    ResourceMark rm(THREAD);
    THROW_MSG(vmSymbols::java_lang_AbstractMethodError(), Method::name_and_sig_as_C_string(resolved_klass, sel_method->name(), sel_method->signature()));
  }

  // setup result
  result.set_static(resolved_klass, sel_method, CHECK);
}

void LinkResolver::resolve_virtual_call(CallInfo& result, Handle recv, Klass* receiver_klass, const LinkInfo& link_info, bool check_null_and_abstract, TRAPS) {
  methodHandle resolved_method = linktime_resolve_virtual_method(link_info, CHECK);
  runtime_resolve_virtual_method(result, resolved_method, link_info.resolved_klass(), recv, receiver_klass, check_null_and_abstract, CHECK);
}

// throws linktime exceptions
methodHandle LinkResolver::linktime_resolve_virtual_method(const LinkInfo& link_info, TRAPS) {
  // normal method resolution
  methodHandle resolved_method = resolve_method(link_info, Bytecodes::_invokevirtual, CHECK_NULL);

  // check if private interface method
  Klass* resolved_klass = link_info.resolved_klass();
  Klass* current_klass = link_info.current_klass();

  // This is impossible, if resolve_klass is an interface, we've thrown icce in resolve_method
  if (resolved_klass->is_interface() && resolved_method->is_private()) {
    ResourceMark rm(THREAD);
    char buf[200];
    jio_snprintf(buf, sizeof(buf), "private interface method requires invokespecial, not invokevirtual: method %s, caller-class:%s",
                 Method::name_and_sig_as_C_string(resolved_klass, resolved_method->name(), resolved_method->signature()),
                   (current_klass == NULL ? "<NULL>" : current_klass->internal_name()));
    THROW_MSG_NULL(vmSymbols::java_lang_IncompatibleClassChangeError(), buf);
  }

  // check if not static
  if (resolved_method->is_static()) {
    ResourceMark rm(THREAD);
    char buf[200];
    jio_snprintf(buf, sizeof(buf), "Expecting non-static method %s", Method::name_and_sig_as_C_string(resolved_klass, resolved_method->name(), resolved_method->signature()));
    THROW_MSG_NULL(vmSymbols::java_lang_IncompatibleClassChangeError(), buf);
  }

  return resolved_method;
}

// throws runtime exceptions
void LinkResolver::runtime_resolve_virtual_method(CallInfo& result, const methodHandle& resolved_method, Klass* resolved_klass, Handle recv, Klass* recv_klass, bool check_null_and_abstract, TRAPS) {
  // setup default return values
  int vtable_index = Method::invalid_vtable_index;
  methodHandle selected_method;

  // runtime method resolution
  if (check_null_and_abstract && recv.is_null()) { // check if receiver exists
    THROW(vmSymbols::java_lang_NullPointerException());
  }

  // do lookup based on receiver klass using the vtable index
  if (resolved_method->method_holder()->is_interface()) { // default or miranda method
    vtable_index = vtable_index_of_interface_method(resolved_klass, resolved_method);

    selected_method = methodHandle(THREAD, recv_klass->method_at_vtable(vtable_index));
  } else {
    vtable_index = resolved_method->vtable_index();
    // We could get a negative vtable_index of nonvirtual_vtable_index for private
    // methods, or for final methods. Private methods never appear in the vtable
    // and never override other methods. As an optimization, final methods are
    // never put in the vtable, unless they override an existing method.
    // So if we do get nonvirtual_vtable_index, it means the selected method is the
    // resolved method, and it can never be changed by an override.
    if (vtable_index == Method::nonvirtual_vtable_index) {
      selected_method = resolved_method;
    } else {
      selected_method = methodHandle(THREAD, recv_klass->method_at_vtable(vtable_index));
    }
  }

  // check if method exists
  if (selected_method.is_null()) {
    throw_abstract_method_error(resolved_method, recv_klass, CHECK);
  }

  // check if abstract
  if (check_null_and_abstract && selected_method->is_abstract()) {
    // Pass arguments for generating a verbose error message.
    throw_abstract_method_error(resolved_method, selected_method, recv_klass, CHECK);
  }

  // setup result
  result.set_virtual(resolved_klass, recv_klass, resolved_method, selected_method, vtable_index, CHECK);
}

void LinkResolver::resolve_interface_call(CallInfo& result, Handle recv, Klass* recv_klass, const LinkInfo& link_info, bool check_null_and_abstract, TRAPS) {
  // throws linktime exceptions
  methodHandle resolved_method = linktime_resolve_interface_method(link_info, CHECK);
  runtime_resolve_interface_method(result, resolved_method,link_info.resolved_klass(), recv, recv_klass, check_null_and_abstract, CHECK);
}

methodHandle LinkResolver::linktime_resolve_interface_method(const LinkInfo& link_info, TRAPS) {
  // normal interface method resolution
  methodHandle resolved_method = resolve_interface_method(link_info, Bytecodes::_invokeinterface, CHECK_NULL);

  return resolved_method;
}

// throws runtime exceptions
void LinkResolver::runtime_resolve_interface_method(CallInfo& result, const methodHandle& resolved_method, Klass* resolved_klass, Handle recv, Klass* recv_klass, bool check_null_and_abstract, TRAPS) {
  // check if receiver exists
  if (check_null_and_abstract && recv.is_null()) {
    THROW(vmSymbols::java_lang_NullPointerException());
  }

  // check if receiver klass implements the resolved interface
  if (!recv_klass->is_subtype_of(resolved_klass)) {
    ResourceMark rm(THREAD);
    char buf[200];
    jio_snprintf(buf, sizeof(buf), "Class %s does not implement the requested interface %s", recv_klass->external_name(), resolved_klass->external_name());
    THROW_MSG(vmSymbols::java_lang_IncompatibleClassChangeError(), buf);
  }

  methodHandle selected_method = resolved_method;

  // resolve the method in the receiver class, unless it is private
  if (!resolved_method()->is_private()) {
    // do lookup based on receiver klass
    // This search must match the linktime preparation search for itable initialization
    // to correctly enforce loader constraints for interface method inheritance.
    // Private methods are skipped as the resolved method was not private.
    selected_method = lookup_instance_method_in_klasses(recv_klass, resolved_method->name(), resolved_method->signature(), Klass::skip_private, CHECK);

    if (selected_method.is_null() && !check_null_and_abstract) {
      // In theory this is a harmless placeholder value, but
      // in practice leaving in null affects the nsk default method tests.
      // This needs further study.
      selected_method = resolved_method;
    }
    // check if method exists
    if (selected_method.is_null()) {
      // Pass arguments for generating a verbose error message.
      throw_abstract_method_error(resolved_method, recv_klass, CHECK);
    }
    // check access
    // Throw Illegal Access Error if selected_method is not public.
    if (!selected_method->is_public()) {
      ResourceMark rm(THREAD);
      THROW_MSG(vmSymbols::java_lang_IllegalAccessError(),
                Method::name_and_sig_as_C_string(recv_klass,
                                                 selected_method->name(),
                                                 selected_method->signature()));
    }
    // check if abstract
    if (check_null_and_abstract && selected_method->is_abstract()) {
      throw_abstract_method_error(resolved_method, selected_method, recv_klass, CHECK);
    }
  }

  // setup result
  if (resolved_method->has_vtable_index()) {
    int vtable_index = resolved_method->vtable_index();
    result.set_virtual(resolved_klass, recv_klass, resolved_method, selected_method, vtable_index, CHECK);
  } else if (resolved_method->has_itable_index()) {
    int itable_index = resolved_method()->itable_index();
    result.set_interface(resolved_klass, recv_klass, resolved_method, selected_method, itable_index, CHECK);
  } else {
    int index = resolved_method->vtable_index();
    // This sets up the nonvirtual form of "virtual" call (as needed for final and private methods)
    result.set_virtual(resolved_klass, resolved_klass, resolved_method, resolved_method, index, CHECK);
  }
}

methodHandle LinkResolver::linktime_resolve_interface_method_or_null(const LinkInfo& link_info) {
  EXCEPTION_MARK;
  methodHandle method_result = linktime_resolve_interface_method(link_info, THREAD);
  if (HAS_PENDING_EXCEPTION) {
    CLEAR_PENDING_EXCEPTION;
    return methodHandle();
  } else {
    return method_result;
  }
}

methodHandle LinkResolver::linktime_resolve_virtual_method_or_null(const LinkInfo& link_info) {
  EXCEPTION_MARK;
  methodHandle method_result = linktime_resolve_virtual_method(link_info, THREAD);
  if (HAS_PENDING_EXCEPTION) {
    CLEAR_PENDING_EXCEPTION;
    return methodHandle();
  } else {
    return method_result;
  }
}

methodHandle LinkResolver::resolve_virtual_call_or_null(Klass* receiver_klass, const LinkInfo& link_info) {
  EXCEPTION_MARK;
  CallInfo info;
  resolve_virtual_call(info, Handle(), receiver_klass, link_info, false, THREAD);
  if (HAS_PENDING_EXCEPTION) {
    CLEAR_PENDING_EXCEPTION;
    return methodHandle();
  }
  return info.selected_method();
}

methodHandle LinkResolver::resolve_interface_call_or_null(Klass* receiver_klass, const LinkInfo& link_info) {
  EXCEPTION_MARK;
  CallInfo info;
  resolve_interface_call(info, Handle(), receiver_klass, link_info, false, THREAD);
  if (HAS_PENDING_EXCEPTION) {
    CLEAR_PENDING_EXCEPTION;
    return methodHandle();
  }
  return info.selected_method();
}

int LinkResolver::resolve_virtual_vtable_index(Klass* receiver_klass, const LinkInfo& link_info) {
  EXCEPTION_MARK;
  CallInfo info;
  resolve_virtual_call(info, Handle(), receiver_klass, link_info, /*check_null_or_abstract*/false, THREAD);
  if (HAS_PENDING_EXCEPTION) {
    CLEAR_PENDING_EXCEPTION;
    return Method::invalid_vtable_index;
  }
  return info.vtable_index();
}

methodHandle LinkResolver::resolve_static_call_or_null(const LinkInfo& link_info) {
  EXCEPTION_MARK;
  CallInfo info;
  resolve_static_call(info, link_info, /*initialize_class*/false, THREAD);
  if (HAS_PENDING_EXCEPTION) {
    CLEAR_PENDING_EXCEPTION;
    return methodHandle();
  }
  return info.selected_method();
}

methodHandle LinkResolver::resolve_special_call_or_null(const LinkInfo& link_info) {
  EXCEPTION_MARK;
  CallInfo info;
  resolve_special_call(info, Handle(), link_info, THREAD);
  if (HAS_PENDING_EXCEPTION) {
    CLEAR_PENDING_EXCEPTION;
    return methodHandle();
  }
  return info.selected_method();
}

//------------------------------------------------------------------------------------------------------------------------
// ConstantPool entries

void LinkResolver::resolve_invoke(CallInfo& result, Handle recv, const constantPoolHandle& pool, int index, Bytecodes::Code byte, TRAPS) {
  switch (byte) {
    case Bytecodes::_invokestatic   : resolve_invokestatic   (result,       pool, index, CHECK); break;
    case Bytecodes::_invokespecial  : resolve_invokespecial  (result, recv, pool, index, CHECK); break;
    case Bytecodes::_invokevirtual  : resolve_invokevirtual  (result, recv, pool, index, CHECK); break;
    case Bytecodes::_invokeinterface: resolve_invokeinterface(result, recv, pool, index, CHECK); break;
    default                         :                                                            break;
  }
  return;
}

void LinkResolver::resolve_invoke(CallInfo& result, Handle& recv, const methodHandle& attached_method, Bytecodes::Code byte, TRAPS) {
  Klass* defc = attached_method->method_holder();
  Symbol* name = attached_method->name();
  Symbol* type = attached_method->signature();
  LinkInfo link_info(defc, name, type);
  switch (byte) {
    case Bytecodes::_invokevirtual:
      resolve_virtual_call(result, recv, recv->klass(), link_info, /*check_null_and_abstract=*/true, CHECK);
      break;
    case Bytecodes::_invokeinterface:
      resolve_interface_call(result, recv, recv->klass(), link_info, /*check_null_and_abstract=*/true, CHECK);
      break;
    case Bytecodes::_invokestatic:
      resolve_static_call(result, link_info, /*initialize_class=*/false, CHECK);
      break;
    case Bytecodes::_invokespecial:
      resolve_special_call(result, recv, link_info, CHECK);
      break;
    default:
      fatal("bad call: %s", Bytecodes::name(byte));
      break;
  }
}

void LinkResolver::resolve_invokestatic(CallInfo& result, const constantPoolHandle& pool, int index, TRAPS) {
  LinkInfo link_info(pool, index, CHECK);
  resolve_static_call(result, link_info, /*initialize_class*/true, CHECK);
}

void LinkResolver::resolve_invokespecial(CallInfo& result, Handle recv, const constantPoolHandle& pool, int index, TRAPS) {
  LinkInfo link_info(pool, index, CHECK);
  resolve_special_call(result, recv, link_info, CHECK);
}

void LinkResolver::resolve_invokevirtual(CallInfo& result, Handle recv, const constantPoolHandle& pool, int index, TRAPS) {
  LinkInfo link_info(pool, index, CHECK);
  Klass* recvrKlass = recv.is_null() ? (Klass*)NULL : recv->klass();
  resolve_virtual_call(result, recv, recvrKlass, link_info, /*check_null_or_abstract*/true, CHECK);
}

void LinkResolver::resolve_invokeinterface(CallInfo& result, Handle recv, const constantPoolHandle& pool, int index, TRAPS) {
  LinkInfo link_info(pool, index, CHECK);
  Klass* recvrKlass = recv.is_null() ? (Klass*)NULL : recv->klass();
  resolve_interface_call(result, recv, recvrKlass, link_info, true, CHECK);
}

// Selected method is abstract.
void LinkResolver::throw_abstract_method_error(const methodHandle& resolved_method, const methodHandle& selected_method, Klass *recv_klass, TRAPS) {
  Klass *resolved_klass = resolved_method->method_holder();
  ResourceMark rm(THREAD);
  stringStream ss;

  if (recv_klass != NULL) {
    ss.print("Receiver class %s does not define or inherit an implementation of the", recv_klass->external_name());
  } else {
    ss.print("Missing implementation of");
  }

  ss.print(" resolved method %s%s%s%s of %s %s.",
           resolved_method->is_abstract() ? "abstract " : "",
           resolved_method->is_private()  ? "private "  : "",
           resolved_method->name()->as_C_string(),
           resolved_method->signature()->as_C_string(),
           resolved_klass->external_kind(),
           resolved_klass->external_name());

  if (selected_method.not_null() && !(resolved_method == selected_method)) {
    ss.print(" Selected method is %s%s%s.",
             selected_method->is_abstract() ? "abstract " : "",
             selected_method->is_private()  ? "private "  : "",
             selected_method->name_and_sig_as_C_string());
  }

  THROW_MSG(vmSymbols::java_lang_AbstractMethodError(), ss.as_string());
}
