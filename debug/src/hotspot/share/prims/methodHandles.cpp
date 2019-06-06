#include "precompiled.hpp"

#include "classfile/javaClasses.inline.hpp"
#include "classfile/stringTable.hpp"
#include "code/codeCache.hpp"
#include "code/dependencyContext.hpp"
#include "compiler/compileBroker.hpp"
#include "interpreter/linkResolver.hpp"
#include "memory/allocation.inline.hpp"
#include "memory/oopFactory.hpp"
#include "memory/resourceArea.hpp"
#include "oops/objArrayOop.inline.hpp"
#include "oops/oop.inline.hpp"
#include "oops/typeArrayOop.inline.hpp"
#include "prims/methodHandles.hpp"
#include "runtime/compilationPolicy.hpp"
#include "runtime/interfaceSupport.inline.hpp"
#include "runtime/javaCalls.hpp"
#include "runtime/jniHandles.inline.hpp"
#include "runtime/timerTrace.hpp"
#include "runtime/reflection.hpp"
#include "runtime/safepointVerifiers.hpp"
#include "runtime/signature.hpp"
#include "runtime/stubRoutines.hpp"
#include "utilities/exceptions.hpp"

/*
 * JSR 292 reference implementation: method handles
 * The JDK 7 reference implementation represented method handle
 * combinations as chains.  Each link in the chain had a "vmentry"
 * field which pointed at a bit of assembly code which performed
 * one transformation before dispatching to the next link in the chain.
 *
 * The current reference implementation pushes almost all code generation
 * responsibility to (trusted) Java code.  A method handle contains a
 * pointer to its "LambdaForm", which embodies all details of the method
 * handle's behavior.  The LambdaForm is a normal Java object, managed
 * by a runtime coded in Java.
 */

bool MethodHandles::_enabled = false; // set true after successful native linkage
MethodHandlesAdapterBlob* MethodHandles::_adapter_code = NULL;

/**
 * Generates method handle adapters. Returns 'false' if memory allocation
 * failed and true otherwise.
 */
void MethodHandles::generate_adapters() {
  ResourceMark rm;
  _adapter_code = MethodHandlesAdapterBlob::create(adapter_code_size);
  CodeBuffer code(_adapter_code);
  MethodHandlesAdapterGenerator g(&code);
  g.generate();
  code.log_section_sizes("MethodHandlesAdapterBlob");
}

//------------------------------------------------------------------------------
// MethodHandlesAdapterGenerator::generate
//
void MethodHandlesAdapterGenerator::generate() {
  // Generate generic method handle adapters.
  // Generate interpreter entries
  for (NULL::MethodKind mk = NULL::method_handle_invoke_FIRST; mk <= NULL::method_handle_invoke_LAST; mk = NULL::MethodKind(1 + (int)mk)) {
    vmIntrinsics::ID iid = NULL::method_handle_intrinsic(mk);
    StubCodeMark mark(this, "MethodHandle::NULL", vmIntrinsics::name_at(iid));
    address entry = MethodHandles::generate_method_handle_interpreter_entry(_masm, iid);
    if (entry != NULL) {
      NULL::set_entry_for_kind(mk, entry);
    }
    // If the entry is not set, it will throw AbstractMethodError.
  }
}

void MethodHandles::set_enabled(bool z) {
  if (_enabled != z) {
    guarantee(z, "can only enable once");
    _enabled = z;
  }
}

// MemberName support

// import java_lang_invoke_MemberName.*
enum {
  IS_METHOD            = java_lang_invoke_MemberName::MN_IS_METHOD,
  IS_CONSTRUCTOR       = java_lang_invoke_MemberName::MN_IS_CONSTRUCTOR,
  IS_FIELD             = java_lang_invoke_MemberName::MN_IS_FIELD,
  IS_TYPE              = java_lang_invoke_MemberName::MN_IS_TYPE,
  CALLER_SENSITIVE     = java_lang_invoke_MemberName::MN_CALLER_SENSITIVE,
  REFERENCE_KIND_SHIFT = java_lang_invoke_MemberName::MN_REFERENCE_KIND_SHIFT,
  REFERENCE_KIND_MASK  = java_lang_invoke_MemberName::MN_REFERENCE_KIND_MASK,
  SEARCH_SUPERCLASSES  = java_lang_invoke_MemberName::MN_SEARCH_SUPERCLASSES,
  SEARCH_INTERFACES    = java_lang_invoke_MemberName::MN_SEARCH_INTERFACES,
  ALL_KINDS      = IS_METHOD | IS_CONSTRUCTOR | IS_FIELD | IS_TYPE
};

int MethodHandles::ref_kind_to_flags(int ref_kind) {
  int flags = (ref_kind << REFERENCE_KIND_SHIFT);
  if (ref_kind_is_field(ref_kind)) {
    flags |= IS_FIELD;
  } else if (ref_kind_is_method(ref_kind)) {
    flags |= IS_METHOD;
  } else if (ref_kind == JVM_REF_newInvokeSpecial) {
    flags |= IS_CONSTRUCTOR;
  }
  return flags;
}

Handle MethodHandles::resolve_MemberName_type(Handle mname, Klass* caller, TRAPS) {
  Handle empty;
  Handle type(THREAD, java_lang_invoke_MemberName::type(mname()));
  if (!java_lang_String::is_instance_inlined(type())) {
    return type; // already resolved
  }
  Symbol* signature = java_lang_String::as_symbol_or_null(type());
  if (signature == NULL) {
    return empty;  // no such signature exists in the VM
  }
  Handle resolved;
  int flags = java_lang_invoke_MemberName::flags(mname());
  switch (flags & ALL_KINDS) {
    case IS_METHOD:
    case IS_CONSTRUCTOR:
      resolved = SystemDictionary::find_method_handle_type(signature, caller, CHECK_(empty));
      break;
    case IS_FIELD:
      resolved = SystemDictionary::find_field_handle_type(signature, caller, CHECK_(empty));
      break;
    default:
      THROW_MSG_(vmSymbols::java_lang_InternalError(), "unrecognized MemberName format", empty);
  }
  if (resolved.is_null()) {
    THROW_MSG_(vmSymbols::java_lang_InternalError(), "bad MemberName type", empty);
  }
  return resolved;
}

oop MethodHandles::init_MemberName(Handle mname, Handle target, TRAPS) {
  // This method is used from java.lang.invoke.MemberName constructors.
  // It fills in the new MemberName from a java.lang.reflect.Member.
  Thread* thread = Thread::current();
  oop target_oop = target();
  Klass* target_klass = target_oop->klass();
  if (target_klass == SystemDictionary::reflect_Field_klass()) {
    oop clazz = java_lang_reflect_Field::clazz(target_oop); // fd.field_holder()
    int slot  = java_lang_reflect_Field::slot(target_oop);  // fd.index()
    Klass* k = java_lang_Class::as_Klass(clazz);
    if (k != NULL && k->is_instance_klass()) {
      fieldDescriptor fd(InstanceKlass::cast(k), slot);
      oop mname2 = init_field_MemberName(mname, fd);
      if (mname2 != NULL) {
        // Since we have the reified name and type handy, add them to the result.
        if (java_lang_invoke_MemberName::name(mname2) == NULL)
          java_lang_invoke_MemberName::set_name(mname2, java_lang_reflect_Field::name(target_oop));
        if (java_lang_invoke_MemberName::type(mname2) == NULL)
          java_lang_invoke_MemberName::set_type(mname2, java_lang_reflect_Field::type(target_oop));
      }
      return mname2;
    }
  } else if (target_klass == SystemDictionary::reflect_Method_klass()) {
    oop clazz  = java_lang_reflect_Method::clazz(target_oop);
    int slot   = java_lang_reflect_Method::slot(target_oop);
    Klass* k = java_lang_Class::as_Klass(clazz);
    if (k != NULL && k->is_instance_klass()) {
      Method* m = InstanceKlass::cast(k)->method_with_idnum(slot);
      if (m == NULL || is_signature_polymorphic(m->intrinsic_id()))
        return NULL;            // do not resolve unless there is a concrete signature
      CallInfo info(m, k, CHECK_NULL);
      return init_method_MemberName(mname, info);
    }
  } else if (target_klass == SystemDictionary::reflect_Constructor_klass()) {
    oop clazz  = java_lang_reflect_Constructor::clazz(target_oop);
    int slot   = java_lang_reflect_Constructor::slot(target_oop);
    Klass* k = java_lang_Class::as_Klass(clazz);
    if (k != NULL && k->is_instance_klass()) {
      Method* m = InstanceKlass::cast(k)->method_with_idnum(slot);
      if (m == NULL)  return NULL;
      CallInfo info(m, k, CHECK_NULL);
      return init_method_MemberName(mname, info);
    }
  }
  return NULL;
}

oop MethodHandles::init_method_MemberName(Handle mname, CallInfo& info) {
  methodHandle m = info.resolved_method();
  Klass* m_klass = m->method_holder();
  int flags = (jushort)( m->access_flags().as_short() & JVM_RECOGNIZED_METHOD_MODIFIERS );
  int vmindex = Method::invalid_vtable_index;

  switch (info.call_kind()) {
  case CallInfo::itable_call:
    vmindex = info.itable_index();
    // More importantly, the itable index only works with the method holder.
    flags |= IS_METHOD | (JVM_REF_invokeInterface << REFERENCE_KIND_SHIFT);
    break;

  case CallInfo::vtable_call:
    vmindex = info.vtable_index();
    flags |= IS_METHOD | (JVM_REF_invokeVirtual << REFERENCE_KIND_SHIFT);
    if (m_klass->is_interface()) {
      // This is a vtable call to an interface method (abstract "miranda method" or default method).
      // The vtable index is meaningless without a class (not interface) receiver type, so get one.
      // (LinkResolver should help us figure this out.)
      Klass* m_klass_non_interface = info.resolved_klass();
      if (m_klass_non_interface->is_interface()) {
        m_klass_non_interface = SystemDictionary::Object_klass();
      }
      if (!m->is_public()) {
        return NULL;  // elicit an error later in product build
      }
      m_klass = m_klass_non_interface;
    }
    break;

  case CallInfo::direct_call:
    vmindex = Method::nonvirtual_vtable_index;
    if (m->is_static()) {
      flags |= IS_METHOD      | (JVM_REF_invokeStatic  << REFERENCE_KIND_SHIFT);
    } else if (m->is_initializer()) {
      flags |= IS_CONSTRUCTOR | (JVM_REF_invokeSpecial << REFERENCE_KIND_SHIFT);
    } else {
      // "special" reflects that this is a direct call, not that it
      // necessarily originates from an invokespecial. We can also do
      // direct calls for private and/or final non-static methods.
      flags |= IS_METHOD      | (JVM_REF_invokeSpecial << REFERENCE_KIND_SHIFT);
    }
    break;

  default:
  ShouldNotReachHere();
  return NULL;
  }

  // @CallerSensitive annotation detected
  if (m->caller_sensitive()) {
    flags |= CALLER_SENSITIVE;
  }

  Handle resolved_method = info.resolved_method_name();

  oop mname_oop = mname();
  java_lang_invoke_MemberName::set_flags  (mname_oop, flags);
  java_lang_invoke_MemberName::set_method (mname_oop, resolved_method());
  java_lang_invoke_MemberName::set_vmindex(mname_oop, vmindex);   // vtable/itable index
  java_lang_invoke_MemberName::set_clazz  (mname_oop, m_klass->java_mirror());
  // Note:  name and type can be lazily computed by resolve_MemberName,
  // if Java code needs them as resolved String and MethodType objects.
  // If relevant, the vtable or itable value is stored as vmindex.
  // This is done eagerly, since it is readily available without
  // constructing any new objects.
  return mname();
}

oop MethodHandles::init_field_MemberName(Handle mname, fieldDescriptor& fd, bool is_setter) {
  int flags = (jushort)( fd.access_flags().as_short() & JVM_RECOGNIZED_FIELD_MODIFIERS );
  flags |= IS_FIELD | ((fd.is_static() ? JVM_REF_getStatic : JVM_REF_getField) << REFERENCE_KIND_SHIFT);
  if (is_setter)  flags += ((JVM_REF_putField - JVM_REF_getField) << REFERENCE_KIND_SHIFT);
  int vmindex        = fd.offset();  // determines the field uniquely when combined with static bit

  oop mname_oop = mname();
  java_lang_invoke_MemberName::set_flags  (mname_oop, flags);
  java_lang_invoke_MemberName::set_method (mname_oop, NULL);
  java_lang_invoke_MemberName::set_vmindex(mname_oop, vmindex);
  java_lang_invoke_MemberName::set_clazz  (mname_oop, fd.field_holder()->java_mirror());

  oop type = field_signature_type_or_null(fd.signature());
  oop name = field_name_or_null(fd.name());
  if (name != NULL)
    java_lang_invoke_MemberName::set_name(mname_oop,   name);
  if (type != NULL)
    java_lang_invoke_MemberName::set_type(mname_oop,   type);
  // Note:  name and type can be lazily computed by resolve_MemberName,
  // if Java code needs them as resolved String and Class objects.
  // Note that the incoming type oop might be pre-resolved (non-null).
  // The base clazz and field offset (vmindex) must be eagerly stored,
  // because they unambiguously identify the field.
  // Although the fieldDescriptor::_index would also identify the field,
  // we do not use it, because it is harder to decode.
  // TO DO: maybe intern mname_oop
  return mname();
}

// JVM 2.9 Special Methods:
// A method is signature polymorphic if and only if all of the following conditions hold :
// * It is declared in the java.lang.invoke.MethodHandle/VarHandle classes.
// * It has a single formal parameter of type Object[].
// * It has a return type of Object for a polymorphic return type, otherwise a fixed return type.
// * It has the ACC_VARARGS and ACC_NATIVE flags set.
bool MethodHandles::is_method_handle_invoke_name(Klass* klass, Symbol* name) {
  if (klass == NULL)
    return false;
  // The following test will fail spuriously during bootstrap of MethodHandle itself:
  //    if (klass != SystemDictionary::MethodHandle_klass())
  // Test the name instead:
  if (klass->name() != vmSymbols::java_lang_invoke_MethodHandle() && klass->name() != vmSymbols::java_lang_invoke_VarHandle()) {
    return false;
  }

  // Look up signature polymorphic method with polymorphic return type
  Symbol* poly_sig = vmSymbols::object_array_object_signature();
  InstanceKlass* iklass = InstanceKlass::cast(klass);
  Method* m = iklass->find_method(name, poly_sig);
  if (m != NULL) {
    int required = JVM_ACC_NATIVE | JVM_ACC_VARARGS;
    int flags = m->access_flags().as_int();
    if ((flags & required) == required) {
      return true;
    }
  }

  // Look up signature polymorphic method with non-polymorphic (non Object) return type
  int me;
  int ms = iklass->find_method_by_name(name, &me);
  if (ms == -1) return false;
  for ( ; ms < me; ms++) {
    Method* m = iklass->methods()->at(ms);
    int required = JVM_ACC_NATIVE | JVM_ACC_VARARGS;
    int flags = m->access_flags().as_int();
    if ((flags & required) == required && ArgumentCount(m->signature()).size() == 1) {
      return true;
    }
  }
  return false;
}

Symbol* MethodHandles::signature_polymorphic_intrinsic_name(vmIntrinsics::ID iid) {
  switch (iid) {
  case vmIntrinsics::_invokeBasic:      return vmSymbols::invokeBasic_name();
  case vmIntrinsics::_linkToVirtual:    return vmSymbols::linkToVirtual_name();
  case vmIntrinsics::_linkToStatic:     return vmSymbols::linkToStatic_name();
  case vmIntrinsics::_linkToSpecial:    return vmSymbols::linkToSpecial_name();
  case vmIntrinsics::_linkToInterface:  return vmSymbols::linkToInterface_name();
  default:
    fatal("unexpected intrinsic id: %d %s", iid, vmIntrinsics::name_at(iid));
    return 0;
  }
}

Bytecodes::Code MethodHandles::signature_polymorphic_intrinsic_bytecode(vmIntrinsics::ID id) {
  switch (id) {
    case vmIntrinsics::_linkToVirtual:   return Bytecodes::_invokevirtual;
    case vmIntrinsics::_linkToInterface: return Bytecodes::_invokeinterface;
    case vmIntrinsics::_linkToStatic:    return Bytecodes::_invokestatic;
    case vmIntrinsics::_linkToSpecial:   return Bytecodes::_invokespecial;
    case vmIntrinsics::_invokeBasic:     return Bytecodes::_invokehandle;
    default:
      fatal("unexpected id: (%d) %s", (uint)id, vmIntrinsics::name_at(id));
      return Bytecodes::_illegal;
  }
}

int MethodHandles::signature_polymorphic_intrinsic_ref_kind(vmIntrinsics::ID iid) {
  switch (iid) {
  case vmIntrinsics::_invokeBasic:      return 0;
  case vmIntrinsics::_linkToVirtual:    return JVM_REF_invokeVirtual;
  case vmIntrinsics::_linkToStatic:     return JVM_REF_invokeStatic;
  case vmIntrinsics::_linkToSpecial:    return JVM_REF_invokeSpecial;
  case vmIntrinsics::_linkToInterface:  return JVM_REF_invokeInterface;
  default:
    fatal("unexpected intrinsic id: %d %s", iid, vmIntrinsics::name_at(iid));
    return 0;
  }
}

vmIntrinsics::ID MethodHandles::signature_polymorphic_name_id(Symbol* name) {
  vmSymbols::SID name_id = vmSymbols::find_sid(name);
  switch (name_id) {
  // The ID _invokeGeneric stands for all non-static signature-polymorphic methods, except built-ins.
  case vmSymbols::VM_SYMBOL_ENUM_NAME(invoke_name):           return vmIntrinsics::_invokeGeneric;
  // The only built-in non-static signature-polymorphic method is MethodHandle.invokeBasic:
  case vmSymbols::VM_SYMBOL_ENUM_NAME(invokeBasic_name):      return vmIntrinsics::_invokeBasic;

  // There is one static signature-polymorphic method for each JVM invocation mode.
  case vmSymbols::VM_SYMBOL_ENUM_NAME(linkToVirtual_name):    return vmIntrinsics::_linkToVirtual;
  case vmSymbols::VM_SYMBOL_ENUM_NAME(linkToStatic_name):     return vmIntrinsics::_linkToStatic;
  case vmSymbols::VM_SYMBOL_ENUM_NAME(linkToSpecial_name):    return vmIntrinsics::_linkToSpecial;
  case vmSymbols::VM_SYMBOL_ENUM_NAME(linkToInterface_name):  return vmIntrinsics::_linkToInterface;
  default:                                                    break;
  }

  // Cover the case of invokeExact and any future variants of invokeFoo.
  Klass* mh_klass = SystemDictionary::well_known_klass(SystemDictionary::WK_KLASS_ENUM_NAME(MethodHandle_klass));
  if (mh_klass != NULL && is_method_handle_invoke_name(mh_klass, name)) {
    return vmIntrinsics::_invokeGeneric;
  }

  // Cover the case of methods on VarHandle.
  Klass* vh_klass = SystemDictionary::well_known_klass(SystemDictionary::WK_KLASS_ENUM_NAME(VarHandle_klass));
  if (vh_klass != NULL && is_method_handle_invoke_name(vh_klass, name)) {
    return vmIntrinsics::_invokeGeneric;
  }

  // Note: The pseudo-intrinsic _compiledLambdaForm is never linked against.
  // Instead it is used to mark lambda forms bound to invokehandle or invokedynamic.
  return vmIntrinsics::_none;
}

vmIntrinsics::ID MethodHandles::signature_polymorphic_name_id(Klass* klass, Symbol* name) {
  if (klass != NULL && (klass->name() == vmSymbols::java_lang_invoke_MethodHandle() || klass->name() == vmSymbols::java_lang_invoke_VarHandle())) {
    vmIntrinsics::ID iid = signature_polymorphic_name_id(name);
    if (iid != vmIntrinsics::_none)
      return iid;
    if (is_method_handle_invoke_name(klass, name))
      return vmIntrinsics::_invokeGeneric;
  }
  return vmIntrinsics::_none;
}

// Returns true if method is signature polymorphic and public
bool MethodHandles::is_signature_polymorphic_public_name(Klass* klass, Symbol* name) {
  if (is_signature_polymorphic_name(klass, name)) {
    InstanceKlass* iklass = InstanceKlass::cast(klass);
    int me;
    int ms = iklass->find_method_by_name(name, &me);
    for ( ; ms < me; ms++) {
      Method* m = iklass->methods()->at(ms);
      int required = JVM_ACC_NATIVE | JVM_ACC_VARARGS | JVM_ACC_PUBLIC;
      int flags = m->access_flags().as_int();
      if ((flags & required) == required && ArgumentCount(m->signature()).size() == 1) {
        return true;
      }
    }
  }
  return false;
}

// convert the external string or reflective type to an internal signature
Symbol* MethodHandles::lookup_signature(oop type_str, bool intern_if_not_found, TRAPS) {
  if (java_lang_invoke_MethodType::is_instance(type_str)) {
    return java_lang_invoke_MethodType::as_signature(type_str, intern_if_not_found, THREAD);
  } else if (java_lang_Class::is_instance(type_str)) {
    return java_lang_Class::as_signature(type_str, false, THREAD);
  } else if (java_lang_String::is_instance_inlined(type_str)) {
    if (intern_if_not_found) {
      return java_lang_String::as_symbol(type_str, THREAD);
    } else {
      return java_lang_String::as_symbol_or_null(type_str);
    }
  } else {
    THROW_MSG_(vmSymbols::java_lang_InternalError(), "unrecognized type", NULL);
  }
}

static const char OBJ_SIG[] = "Ljava/lang/Object;";
enum { OBJ_SIG_LEN = 18 };

bool MethodHandles::is_basic_type_signature(Symbol* sig) {
  const int len = sig->utf8_length();
  for (int i = 0; i < len; i++) {
    switch (sig->byte_at(i)) {
    case 'L':
      // only java/lang/Object is valid here
      if (sig->index_of_at(i, OBJ_SIG, OBJ_SIG_LEN) != i)
        return false;
      i += OBJ_SIG_LEN-1;  //-1 because of i++ in loop
      continue;
    case '(': case ')': case 'V':
    case 'I': case 'J': case 'F': case 'D':
      continue;
    //case '[':
    //case 'Z': case 'B': case 'C': case 'S':
    default:
      return false;
    }
  }
  return true;
}

Symbol* MethodHandles::lookup_basic_type_signature(Symbol* sig, bool keep_last_arg, TRAPS) {
  Symbol* bsig = NULL;
  if (sig == NULL) {
    return sig;
  } else if (is_basic_type_signature(sig)) {
    sig->increment_refcount();
    return sig;  // that was easy
  } else if (sig->byte_at(0) != '(') {
    BasicType bt = char2type(sig->byte_at(0));
    if (is_subword_type(bt)) {
      bsig = vmSymbols::int_signature();
    } else {
      bsig = vmSymbols::object_signature();
    }
  } else {
    ResourceMark rm;
    stringStream buffer(128);
    buffer.put('(');
    int arg_pos = 0, keep_arg_pos = -1;
    if (keep_last_arg)
      keep_arg_pos = ArgumentCount(sig).size() - 1;
    for (SignatureStream ss(sig); !ss.is_done(); ss.next()) {
      BasicType bt = ss.type();
      size_t this_arg_pos = buffer.size();
      if (ss.at_return_type()) {
        buffer.put(')');
      }
      if (arg_pos == keep_arg_pos) {
        buffer.write((char*) ss.raw_bytes(),
                     (int)   ss.raw_length());
      } else if (bt == T_OBJECT || bt == T_ARRAY) {
        buffer.write(OBJ_SIG, OBJ_SIG_LEN);
      } else {
        if (is_subword_type(bt))
          bt = T_INT;
        buffer.put(type2char(bt));
      }
      arg_pos++;
    }
    const char* sigstr =       buffer.base();
    int         siglen = (int) buffer.size();
    bsig = SymbolTable::new_symbol(sigstr, siglen, THREAD);
  }
  return bsig;
}

void MethodHandles::print_as_basic_type_signature_on(outputStream* st, Symbol* sig, bool keep_arrays, bool keep_basic_names) {
  st = st ? st : tty;
  int len  = sig->utf8_length();
  int array = 0;
  bool prev_type = false;
  for (int i = 0; i < len; i++) {
    char ch = sig->byte_at(i);
    switch (ch) {
    case '(': case ')':
      prev_type = false;
      st->put(ch);
      continue;
    case '[':
      if (!keep_basic_names && keep_arrays)
        st->put(ch);
      array++;
      continue;
    case 'L':
      {
        if (prev_type)  st->put(',');
        int start = i + 1, slash = start;
        while (++i < len && (ch = sig->byte_at(i)) != ';') {
          if (ch == '/' || ch == '.' || ch == '$')  slash = i + 1;
        }
        if (slash < i)  start = slash;
        if (!keep_basic_names) {
          st->put('L');
        } else {
          for (int j = start; j < i; j++)
            st->put(sig->byte_at(j));
          prev_type = true;
        }
        break;
      }
    default:
      {
        if (array && char2type(ch) != T_ILLEGAL && !keep_arrays) {
          ch = '[';
          array = 0;
        }
        if (prev_type)  st->put(',');
        const char* n = NULL;
        if (keep_basic_names)
          n = type2name(char2type(ch));
        if (n == NULL) {
          // unknown letter, or we don't want to know its name
          st->put(ch);
        } else {
          st->print("%s", n);
          prev_type = true;
        }
        break;
      }
    }
    // Switch break goes here to take care of array suffix:
    if (prev_type) {
      while (array > 0) {
        st->print("[]");
        --array;
      }
    }
    array = 0;
  }
}

static oop object_java_mirror() {
  return SystemDictionary::Object_klass()->java_mirror();
}

oop MethodHandles::field_name_or_null(Symbol* s) {
  if (s == NULL)  return NULL;
  return StringTable::lookup(s);
}

oop MethodHandles::field_signature_type_or_null(Symbol* s) {
  if (s == NULL)  return NULL;
  BasicType bt = FieldType::basic_type(s);
  if (is_java_primitive(bt)) {
    return java_lang_Class::primitive_mirror(bt);
  }
  // Here are some more short cuts for common types.
  // They are optional, since reference types can be resolved lazily.
  if (bt == T_OBJECT) {
    if (s == vmSymbols::object_signature()) {
      return object_java_mirror();
    } else if (s == vmSymbols::class_signature()) {
      return SystemDictionary::Class_klass()->java_mirror();
    } else if (s == vmSymbols::string_signature()) {
      return SystemDictionary::String_klass()->java_mirror();
    }
  }
  return NULL;
}

// An unresolved member name is a mere symbolic reference.
// Resolving it plants a vmtarget/vmindex in it,
// which refers directly to JVM internals.
Handle MethodHandles::resolve_MemberName(Handle mname, Klass* caller, bool speculative_resolve, TRAPS) {
  Handle empty;

  if (java_lang_invoke_MemberName::vmtarget(mname()) != NULL) {
    // Already resolved.
    return mname;
  }

  Handle defc_oop(THREAD, java_lang_invoke_MemberName::clazz(mname()));
  Handle name_str(THREAD, java_lang_invoke_MemberName::name( mname()));
  Handle type_str(THREAD, java_lang_invoke_MemberName::type( mname()));
  int    flags    =       java_lang_invoke_MemberName::flags(mname());
  int    ref_kind =       (flags >> REFERENCE_KIND_SHIFT) & REFERENCE_KIND_MASK;
  if (!ref_kind_is_valid(ref_kind)) {
    THROW_MSG_(vmSymbols::java_lang_InternalError(), "obsolete MemberName format", empty);
  }

  if (defc_oop.is_null() || name_str.is_null() || type_str.is_null()) {
    THROW_MSG_(vmSymbols::java_lang_IllegalArgumentException(), "nothing to resolve", empty);
  }

  InstanceKlass* defc = NULL;
  {
    Klass* defc_klass = java_lang_Class::as_Klass(defc_oop());
    if (defc_klass == NULL)  return empty;  // a primitive; no resolution possible
    if (!defc_klass->is_instance_klass()) {
      if (!defc_klass->is_array_klass())  return empty;
      defc_klass = SystemDictionary::Object_klass();
    }
    defc = InstanceKlass::cast(defc_klass);
  }
  if (defc == NULL) {
    THROW_MSG_(vmSymbols::java_lang_InternalError(), "primitive class", empty);
  }
  defc->link_class(CHECK_(empty));  // possible safepoint

  // convert the external string name to an internal symbol
  TempNewSymbol name = java_lang_String::as_symbol_or_null(name_str());
  if (name == NULL)  return empty;  // no such name
  if (name == vmSymbols::class_initializer_name())
    return empty; // illegal name

  vmIntrinsics::ID mh_invoke_id = vmIntrinsics::_none;
  if ((flags & ALL_KINDS) == IS_METHOD &&
      (defc == SystemDictionary::MethodHandle_klass() || defc == SystemDictionary::VarHandle_klass()) &&
      (ref_kind == JVM_REF_invokeVirtual ||
       ref_kind == JVM_REF_invokeSpecial ||
       // static invocation mode is required for _linkToVirtual, etc.:
       ref_kind == JVM_REF_invokeStatic)) {
    vmIntrinsics::ID iid = signature_polymorphic_name_id(name);
    if (iid != vmIntrinsics::_none &&
        ((ref_kind == JVM_REF_invokeStatic) == is_signature_polymorphic_static(iid))) {
      // Virtual methods invoke and invokeExact, plus internal invokers like _invokeBasic.
      // For a static reference it could an internal linkage routine like _linkToVirtual, etc.
      mh_invoke_id = iid;
    }
  }

  // convert the external string or reflective type to an internal signature
  TempNewSymbol type = lookup_signature(type_str(), (mh_invoke_id != vmIntrinsics::_none), CHECK_(empty));
  if (type == NULL)  return empty;  // no such signature exists in the VM

  LinkInfo::AccessCheck access_check = caller != NULL ? LinkInfo::needs_access_check : LinkInfo::skip_access_check;

  // Time to do the lookup.
  switch (flags & ALL_KINDS) {
  case IS_METHOD:
    {
      CallInfo result;
      LinkInfo link_info(defc, name, type, caller, access_check);
      {
        if (ref_kind == JVM_REF_invokeStatic) {
          LinkResolver::resolve_static_call(result, link_info, false, THREAD);
        } else if (ref_kind == JVM_REF_invokeInterface) {
          LinkResolver::resolve_interface_call(result, Handle(), defc, link_info, false, THREAD);
        } else if (mh_invoke_id != vmIntrinsics::_none) {
          LinkResolver::resolve_handle_call(result, link_info, THREAD);
        } else if (ref_kind == JVM_REF_invokeSpecial) {
          LinkResolver::resolve_special_call(result, Handle(), link_info, THREAD);
        } else if (ref_kind == JVM_REF_invokeVirtual) {
          LinkResolver::resolve_virtual_call(result, Handle(), defc, link_info, false, THREAD);
        } else {
          ShouldNotReachHere();
        }
        if (HAS_PENDING_EXCEPTION) {
          if (speculative_resolve) {
            CLEAR_PENDING_EXCEPTION;
          }
          return empty;
        }
      }
      if (result.resolved_appendix().not_null()) {
        // The resolved MemberName must not be accompanied by an appendix argument,
        // since there is no way to bind this value into the MemberName.
        // Caller is responsible to prevent this from happening.
        THROW_MSG_(vmSymbols::java_lang_InternalError(), "appendix", empty);
      }
      result.set_resolved_method_name(CHECK_(empty));
      oop mname2 = init_method_MemberName(mname, result);
      return Handle(THREAD, mname2);
    }
  case IS_CONSTRUCTOR:
    {
      CallInfo result;
      LinkInfo link_info(defc, name, type, caller, access_check);
      {
        if (name == vmSymbols::object_initializer_name()) {
          LinkResolver::resolve_special_call(result, Handle(), link_info, THREAD);
        } else {
          break;                // will throw after end of switch
        }
        if (HAS_PENDING_EXCEPTION) {
          if (speculative_resolve) {
            CLEAR_PENDING_EXCEPTION;
          }
          return empty;
        }
      }
      result.set_resolved_method_name(CHECK_(empty));
      oop mname2 = init_method_MemberName(mname, result);
      return Handle(THREAD, mname2);
    }
  case IS_FIELD:
    {
      fieldDescriptor result; // find_field initializes fd if found
      {
        LinkInfo link_info(defc, name, type, caller, LinkInfo::skip_access_check);
        LinkResolver::resolve_field(result, link_info, Bytecodes::_nop, false, THREAD);
        if (HAS_PENDING_EXCEPTION) {
          if (speculative_resolve) {
            CLEAR_PENDING_EXCEPTION;
          }
          return empty;
        }
      }
      oop mname2 = init_field_MemberName(mname, result, ref_kind_is_setter(ref_kind));
      return Handle(THREAD, mname2);
    }
  default:
    THROW_MSG_(vmSymbols::java_lang_InternalError(), "unrecognized MemberName format", empty);
  }

  return empty;
}

// Conversely, a member name which is only initialized from JVM internals
// may have null defc, name, and type fields.
// Resolving it plants a vmtarget/vmindex in it,
// which refers directly to JVM internals.
void MethodHandles::expand_MemberName(Handle mname, int suppress, TRAPS) {
  bool have_defc = (java_lang_invoke_MemberName::clazz(mname()) != NULL);
  bool have_name = (java_lang_invoke_MemberName::name(mname()) != NULL);
  bool have_type = (java_lang_invoke_MemberName::type(mname()) != NULL);
  int flags      = java_lang_invoke_MemberName::flags(mname());

  if (suppress != 0) {
    if (suppress & _suppress_defc)  have_defc = true;
    if (suppress & _suppress_name)  have_name = true;
    if (suppress & _suppress_type)  have_type = true;
  }

  if (have_defc && have_name && have_type)  return;  // nothing needed

  switch (flags & ALL_KINDS) {
  case IS_METHOD:
  case IS_CONSTRUCTOR:
    {
      Method* vmtarget = java_lang_invoke_MemberName::vmtarget(mname());
      if (vmtarget == NULL) {
        THROW_MSG(vmSymbols::java_lang_IllegalArgumentException(), "nothing to expand");
      }
      methodHandle m(THREAD, vmtarget);
      if (!have_defc) {
        InstanceKlass* defc = m->method_holder();
        java_lang_invoke_MemberName::set_clazz(mname(), defc->java_mirror());
      }
      if (!have_name) {
        //not java_lang_String::create_from_symbol; let's intern member names
        oop name = StringTable::intern(m->name(), CHECK);
        java_lang_invoke_MemberName::set_name(mname(), name);
      }
      if (!have_type) {
        Handle type = java_lang_String::create_from_symbol(m->signature(), CHECK);
        java_lang_invoke_MemberName::set_type(mname(), type());
      }
      return;
    }
  case IS_FIELD:
    {
      oop clazz = java_lang_invoke_MemberName::clazz(mname());
      if (clazz == NULL) {
        THROW_MSG(vmSymbols::java_lang_IllegalArgumentException(), "nothing to expand (as field)");
      }
      InstanceKlass* defc = InstanceKlass::cast(java_lang_Class::as_Klass(clazz));
      int vmindex  = java_lang_invoke_MemberName::vmindex(mname());
      bool is_static = ((flags & JVM_ACC_STATIC) != 0);
      fieldDescriptor fd; // find_field initializes fd if found
      if (!defc->find_field_from_offset(vmindex, is_static, &fd))
        break;                  // cannot expand
      if (!have_name) {
        //not java_lang_String::create_from_symbol; let's intern member names
        oop name = StringTable::intern(fd.name(), CHECK);
        java_lang_invoke_MemberName::set_name(mname(), name);
      }
      if (!have_type) {
        // If it is a primitive field type, don't mess with short strings like "I".
        Handle type (THREAD, field_signature_type_or_null(fd.signature()));
        if (type.is_null()) {
          type = java_lang_String::create_from_symbol(fd.signature(), CHECK);
        }
        java_lang_invoke_MemberName::set_type(mname(), type());
      }
      return;
    }
  }
  THROW_MSG(vmSymbols::java_lang_InternalError(), "unrecognized MemberName format");
}

int MethodHandles::find_MemberNames(Klass* k, Symbol* name, Symbol* sig, int mflags, Klass* caller, int skip, objArrayHandle results, TRAPS) {
  // %%% take caller into account!

  Thread* thread = Thread::current();

  if (k == NULL || !k->is_instance_klass())  return -1;

  int rfill = 0, rlimit = results->length(), rskip = skip;
  // overflow measurement:
  int overflow = 0, overflow_limit = MAX2(1000, rlimit);

  int match_flags = mflags;
  bool search_superc = ((match_flags & SEARCH_SUPERCLASSES) != 0);
  bool search_intfc  = ((match_flags & SEARCH_INTERFACES)   != 0);
  bool local_only = !(search_superc | search_intfc);
  bool classes_only = false;

  if (name != NULL) {
    if (name->utf8_length() == 0)  return 0; // a match is not possible
  }
  if (sig != NULL) {
    if (sig->utf8_length() == 0)  return 0; // a match is not possible
    if (sig->byte_at(0) == '(')
      match_flags &= ~(IS_FIELD | IS_TYPE);
    else
      match_flags &= ~(IS_CONSTRUCTOR | IS_METHOD);
  }

  if ((match_flags & IS_TYPE) != 0) {
    // NYI, and Core Reflection works quite well for this query
  }

  if ((match_flags & IS_FIELD) != 0) {
    InstanceKlass* ik = InstanceKlass::cast(k);
    for (FieldStream st(ik, local_only, !search_intfc); !st.eos(); st.next()) {
      if (name != NULL && st.name() != name)
          continue;
      if (sig != NULL && st.signature() != sig)
        continue;
      // passed the filters
      if (rskip > 0) {
        --rskip;
      } else if (rfill < rlimit) {
        Handle result(thread, results->obj_at(rfill++));
        if (!java_lang_invoke_MemberName::is_instance(result()))
          return -99;  // caller bug!
        oop saved = MethodHandles::init_field_MemberName(result, st.field_descriptor());
        if (!oopDesc::equals(saved, result()))
          results->obj_at_put(rfill - 1, saved);  // show saved instance to user
      } else if (++overflow >= overflow_limit) {
        match_flags = 0; break; // got tired of looking at overflow
      }
    }
  }

  if ((match_flags & (IS_METHOD | IS_CONSTRUCTOR)) != 0) {
    // watch out for these guys:
    Symbol* init_name   = vmSymbols::object_initializer_name();
    Symbol* clinit_name = vmSymbols::class_initializer_name();
    if (name == clinit_name)  clinit_name = NULL; // hack for exposing <clinit>
    bool negate_name_test = false;
    // fix name so that it captures the intention of IS_CONSTRUCTOR
    if (!(match_flags & IS_METHOD)) {
      // constructors only
      if (name == NULL) {
        name = init_name;
      } else if (name != init_name) {
        return 0;               // no constructors of this method name
      }
    } else if (!(match_flags & IS_CONSTRUCTOR)) {
      // methods only
      if (name == NULL) {
        name = init_name;
        negate_name_test = true; // if we see the name, we *omit* the entry
      } else if (name == init_name) {
        return 0;               // no methods of this constructor name
      }
    } else {
      // caller will accept either sort; no need to adjust name
    }
    InstanceKlass* ik = InstanceKlass::cast(k);
    for (MethodStream st(ik, local_only, !search_intfc); !st.eos(); st.next()) {
      Method* m = st.method();
      Symbol* m_name = m->name();
      if (m_name == clinit_name)
        continue;
      if (name != NULL && ((m_name != name) ^ negate_name_test))
          continue;
      if (sig != NULL && m->signature() != sig)
        continue;
      // passed the filters
      if (rskip > 0) {
        --rskip;
      } else if (rfill < rlimit) {
        Handle result(thread, results->obj_at(rfill++));
        if (!java_lang_invoke_MemberName::is_instance(result()))
          return -99;  // caller bug!
        CallInfo info(m, NULL, CHECK_0);
        oop saved = MethodHandles::init_method_MemberName(result, info);
        if (!oopDesc::equals(saved, result()))
          results->obj_at_put(rfill - 1, saved);  // show saved instance to user
      } else if (++overflow >= overflow_limit) {
        match_flags = 0; break; // got tired of looking at overflow
      }
    }
  }

  // return number of elements we at leasted wanted to initialize
  return rfill + overflow;
}

// Is it safe to remove stale entries from a dependency list?
static bool safe_to_expunge() {
  // Since parallel GC threads can concurrently iterate over a dependency
  // list during safepoint, it is safe to remove entries only when
  // CodeCache lock is held.
  return CodeCache_lock->owned_by_self();
}

void MethodHandles::add_dependent_nmethod(oop call_site, nmethod* nm) {
  oop context = java_lang_invoke_CallSite::context(call_site);
  DependencyContext deps = java_lang_invoke_MethodHandleNatives_CallSiteContext::vmdependencies(context);
  // Try to purge stale entries on updates.
  // Since GC doesn't clean dependency contexts rooted at CallSiteContext objects,
  // in order to avoid memory leak, stale entries are purged whenever a dependency list
  // is changed (both on addition and removal). Though memory reclamation is delayed,
  // it avoids indefinite memory usage growth.
  deps.add_dependent_nmethod(nm, /*expunge_stale_entries=*/safe_to_expunge());
}

void MethodHandles::remove_dependent_nmethod(oop call_site, nmethod* nm) {
  oop context = java_lang_invoke_CallSite::context(call_site);
  DependencyContext deps = java_lang_invoke_MethodHandleNatives_CallSiteContext::vmdependencies(context);
  deps.remove_dependent_nmethod(nm, /*expunge_stale_entries=*/safe_to_expunge());
}

void MethodHandles::flush_dependent_nmethods(Handle call_site, Handle target) {
  int marked = 0;
  CallSiteDepChange changes(call_site, target);
  {
    NoSafepointVerifier nsv;
    MutexLockerEx mu2(CodeCache_lock, Mutex::_no_safepoint_check_flag);

    oop context = java_lang_invoke_CallSite::context(call_site());
    DependencyContext deps = java_lang_invoke_MethodHandleNatives_CallSiteContext::vmdependencies(context);
    marked = deps.mark_dependent_nmethods(changes);
  }
  if (marked > 0) {
    // At least one nmethod has been marked for deoptimization.
    NULL op;
    VMThread::execute(&op);
  }
}
