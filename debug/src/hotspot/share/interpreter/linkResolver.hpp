#ifndef SHARE_VM_INTERPRETER_LINKRESOLVER_HPP
#define SHARE_VM_INTERPRETER_LINKRESOLVER_HPP

#include "oops/method.hpp"

// All the necessary definitions for run-time link resolution.

// CallInfo provides all the information gathered for a particular
// linked call site after resolving it. A link is any reference
// made from within the bytecodes of a method to an object outside of
// that method. If the info is invalid, the link has not been resolved
// successfully.

class CallInfo : public StackObj {
 public:
  // Ways that a method call might be selected (or not) based on receiver type.
  // Note that an invokevirtual instruction might be linked with no_dispatch,
  // and an invokeinterface instruction might be linked with any of the three options
  enum CallKind {
    direct_call,                        // jump into resolved_method (must be concrete)
    vtable_call,                        // select recv.klass.method_at_vtable(index)
    itable_call,                        // select recv.klass.method_at_itable(resolved_method.holder, index)
    unknown_kind = -1
  };
 private:
  Klass*       _resolved_klass;         // static receiver klass, resolved from a symbolic reference
  Klass*       _selected_klass;         // dynamic receiver class (same as static, or subklass)
  methodHandle _resolved_method;        // static target method
  methodHandle _selected_method;        // dynamic (actual) target method
  CallKind     _call_kind;              // kind of call (static(=bytecode static/special +
                                        //               others inferred), vtable, itable)
  int          _call_index;             // vtable or itable index of selected class method (if any)
  Handle       _resolved_appendix;      // extra argument in constant pool (if CPCE::has_appendix)
  Handle       _resolved_method_type;   // MethodType (for invokedynamic and invokehandle call sites)
  Handle       _resolved_method_name;   // Object holding the ResolvedMethodName

  void set_static(Klass* resolved_klass, const methodHandle& resolved_method, TRAPS);
  void set_interface(Klass* resolved_klass, Klass* selected_klass,
                     const methodHandle& resolved_method,
                     const methodHandle& selected_method,
                     int itable_index, TRAPS);
  void set_virtual(Klass* resolved_klass, Klass* selected_klass,
                   const methodHandle& resolved_method,
                   const methodHandle& selected_method,
                   int vtable_index, TRAPS);
  void set_handle(const methodHandle& resolved_method,
                  Handle resolved_appendix, Handle resolved_method_type, TRAPS);
  void set_handle(Klass* resolved_klass,
                  const methodHandle& resolved_method,
                  Handle resolved_appendix, Handle resolved_method_type, TRAPS);
  void set_common(Klass* resolved_klass, Klass* selected_klass,
                  const methodHandle& resolved_method,
                  const methodHandle& selected_method,
                  CallKind kind,
                  int index, TRAPS);

  friend class LinkResolver;

 public:
  CallInfo() { }

  // utility to extract an effective CallInfo from a method and an optional receiver limit
  // does not queue the method for compilation.  This also creates a ResolvedMethodName
  // object for the resolved_method.
  CallInfo(Method* resolved_method, Klass* resolved_klass, TRAPS);

  Klass*  resolved_klass() const                 { return _resolved_klass; }
  Klass*  selected_klass() const                 { return _selected_klass; }
  methodHandle resolved_method() const           { return _resolved_method; }
  methodHandle selected_method() const           { return _selected_method; }
  Handle       resolved_appendix() const         { return _resolved_appendix; }
  Handle       resolved_method_type() const      { return _resolved_method_type; }
  Handle       resolved_method_name() const      { return _resolved_method_name; }
  // Materialize a java.lang.invoke.ResolvedMethodName for this resolved_method
  void     set_resolved_method_name(TRAPS);

  BasicType    result_type() const               { return selected_method()->result_type(); }
  CallKind     call_kind() const                 { return _call_kind; }
  int          call_index() const                { return _call_index; }
  int          vtable_index() const {
    // The returned value is < 0 if the call is statically bound.
    // But, the returned value may be >= 0 even if the kind is direct_call.
    // It is up to the caller to decide which way to go.
    return _call_index;
  }
  int          itable_index() const {
    // The returned value is always >= 0, a valid itable index.
    return _call_index;
  }

  // debugging
  void         verify() { };
  void         print()  { };
};

// Condensed information from constant pool to use to resolve the method or field.
//   resolved_klass = specified class (i.e., static receiver class)
//   current_klass  = sending method holder (i.e., class containing the method
//                    containing the call being resolved)
//   current_method = sending method (relevant for field resolution)
class LinkInfo : public StackObj {
  Symbol*     _name;            // extracted from JVM_CONSTANT_NameAndType
  Symbol*     _signature;
  Klass*      _resolved_klass;  // class that the constant pool entry points to
  Klass*      _current_klass;   // class that owns the constant pool
  methodHandle _current_method;  // sending method
  bool        _check_access;
  constantTag _tag;

 public:
  enum AccessCheck {
    needs_access_check,
    skip_access_check
  };

  LinkInfo(const constantPoolHandle& pool, int index, const methodHandle& current_method, TRAPS);
  LinkInfo(const constantPoolHandle& pool, int index, TRAPS);

  // Condensed information from other call sites within the vm.
  LinkInfo(Klass* resolved_klass, Symbol* name, Symbol* signature, Klass* current_klass,
           AccessCheck check_access = needs_access_check,
           constantTag tag = JVM_CONSTANT_Invalid) :
    _resolved_klass(resolved_klass),
    _name(name), _signature(signature), _current_klass(current_klass), _current_method(methodHandle()),
    _check_access(check_access == needs_access_check), _tag(tag) { }

  LinkInfo(Klass* resolved_klass, Symbol* name, Symbol* signature, const methodHandle& current_method,
           AccessCheck check_access = needs_access_check,
           constantTag tag = JVM_CONSTANT_Invalid) :
    _resolved_klass(resolved_klass),
    _name(name), _signature(signature), _current_klass(current_method->method_holder()), _current_method(current_method),
    _check_access(check_access == needs_access_check), _tag(tag) { }

  // Case where we just find the method and don't check access against the current class
  LinkInfo(Klass* resolved_klass, Symbol*name, Symbol* signature) :
    _resolved_klass(resolved_klass),
    _name(name), _signature(signature), _current_klass(NULL), _current_method(methodHandle()),
    _check_access(false), _tag(JVM_CONSTANT_Invalid) { }

  // accessors
  Symbol* name() const               { return _name; }
  Symbol* signature() const          { return _signature; }
  Klass* resolved_klass() const      { return _resolved_klass; }
  Klass* current_klass() const       { return _current_klass; }
  methodHandle current_method() const { return _current_method; }
  constantTag tag() const            { return _tag; }
  bool check_access() const          { return _check_access; }
  char* method_string() const;

  void         print()  { };
};

// Link information for getfield/putfield & getstatic/putstatic bytecodes
// is represented using a fieldDescriptor.

// The LinkResolver is used to resolve constant-pool references at run-time.
// It does all necessary link-time checks & throws exceptions if necessary.

class LinkResolver: AllStatic {
  friend class klassVtable;
  friend class klassItable;

 private:

  static Method* lookup_method_in_klasses(const LinkInfo& link_info, bool checkpolymorphism, bool in_imethod_resolve);
  static Method* lookup_method_in_interfaces(const LinkInfo& link_info);

  static methodHandle lookup_polymorphic_method(const LinkInfo& link_info, Handle *appendix_result_or_null, Handle *method_type_result, TRAPS);
 public: // Needed for CompilerToVM.resolveMethod()
  // Not Linktime so doesn't take LinkInfo
  static methodHandle lookup_instance_method_in_klasses (Klass* klass, Symbol* name, Symbol* signature, Klass::PrivateLookupMode private_mode, TRAPS);
 private:

  // Similar loader constraint checking functions that throw
  // LinkageError with descriptive message.
  static void check_method_loader_constraints(const LinkInfo& link_info, const methodHandle& resolved_method, const char* method_type, TRAPS);
  static void check_field_loader_constraints(Symbol* field, Symbol* sig, Klass* current_klass, Klass* sel_klass, TRAPS);

  static methodHandle resolve_interface_method(const LinkInfo& link_info, Bytecodes::Code code, TRAPS);
  static methodHandle resolve_method          (const LinkInfo& link_info, Bytecodes::Code code, TRAPS);

  static methodHandle linktime_resolve_static_method    (const LinkInfo& link_info, TRAPS);
  static methodHandle linktime_resolve_special_method   (const LinkInfo& link_info, TRAPS);
  static methodHandle linktime_resolve_virtual_method   (const LinkInfo& link_info, TRAPS);
  static methodHandle linktime_resolve_interface_method (const LinkInfo& link_info, TRAPS);

  static void runtime_resolve_special_method    (CallInfo& result,
                                                 const LinkInfo& link_info,
                                                 const methodHandle& resolved_method,
                                                 Handle recv, TRAPS);

  static void runtime_resolve_virtual_method    (CallInfo& result,
                                                 const methodHandle& resolved_method,
                                                 Klass* resolved_klass,
                                                 Handle recv,
                                                 Klass* recv_klass,
                                                 bool check_null_and_abstract, TRAPS);
  static void runtime_resolve_interface_method  (CallInfo& result,
                                                 const methodHandle& resolved_method,
                                                 Klass* resolved_klass,
                                                 Handle recv,
                                                 Klass* recv_klass,
                                                 bool check_null_and_abstract, TRAPS);

  static void check_field_accessability(Klass* ref_klass, Klass* resolved_klass, Klass* sel_klass, const fieldDescriptor& fd, TRAPS);
  static void check_method_accessability(Klass* ref_klass, Klass* resolved_klass, Klass* sel_klass, const methodHandle& sel_method, TRAPS);

  // runtime resolving from constant pool
  static void resolve_invokestatic   (CallInfo& result, const constantPoolHandle& pool, int index, TRAPS);
  static void resolve_invokespecial  (CallInfo& result, Handle recv, const constantPoolHandle& pool, int index, TRAPS);
  static void resolve_invokevirtual  (CallInfo& result, Handle recv, const constantPoolHandle& pool, int index, TRAPS);
  static void resolve_invokeinterface(CallInfo& result, Handle recv, const constantPoolHandle& pool, int index, TRAPS);
  static void resolve_invokedynamic  (CallInfo& result, const constantPoolHandle& pool, int index, TRAPS);
  static void resolve_invokehandle   (CallInfo& result, const constantPoolHandle& pool, int index, TRAPS);
 public:
  // constant pool resolving
  static void check_klass_accessability(Klass* ref_klass, Klass* sel_klass, bool fold_type_to_class, TRAPS);
  // The optional 'fold_type_to_class' means that a derived type (array)
  // is first converted to the class it is derived from (element type).
  // If this element type is not a class, then the check passes quietly.
  // This is usually what is needed, but a few existing uses might break
  // if this flag were always turned on.  FIXME: See if it can be, always.
  static void check_klass_accessability(Klass* ref_klass, Klass* sel_klass, TRAPS) {
    return check_klass_accessability(ref_klass, sel_klass, false, THREAD);
  }

  // static resolving calls (will not run any Java code);
  // used only from Bytecode_invoke::static_target
  static methodHandle resolve_method_statically(Bytecodes::Code code, const constantPoolHandle& pool, int index, TRAPS);

  static void resolve_field_access(fieldDescriptor& result, const constantPoolHandle& pool, int index, const methodHandle& method, Bytecodes::Code byte, TRAPS);
  static void resolve_field(fieldDescriptor& result, const LinkInfo& link_info, Bytecodes::Code access_kind, bool initialize_class, TRAPS);

  static void resolve_static_call   (CallInfo& result, const LinkInfo& link_info, bool initialize_klass, TRAPS);
  static void resolve_special_call  (CallInfo& result, Handle recv, const LinkInfo& link_info, TRAPS);
  static void resolve_virtual_call  (CallInfo& result, Handle recv, Klass* recv_klass, const LinkInfo& link_info, bool check_null_and_abstract, TRAPS);
  static void resolve_interface_call(CallInfo& result, Handle recv, Klass* recv_klass, const LinkInfo& link_info, bool check_null_and_abstract, TRAPS);
  static void resolve_handle_call   (CallInfo& result, const LinkInfo& link_info, TRAPS);
  static void resolve_dynamic_call  (CallInfo& result, int pool_index, Handle bootstrap_specifier, Symbol* method_name, Symbol* method_signature, Klass* current_klass, TRAPS);

  // same as above for compile-time resolution; but returns null handle instead of throwing
  // an exception on error also, does not initialize klass (i.e., no side effects)
  static methodHandle resolve_virtual_call_or_null  (Klass* receiver_klass, const LinkInfo& link_info);
  static methodHandle resolve_interface_call_or_null(Klass* receiver_klass, const LinkInfo& link_info);
  static methodHandle resolve_static_call_or_null   (const LinkInfo& link_info);
  static methodHandle resolve_special_call_or_null  (const LinkInfo& link_info);

  static int vtable_index_of_interface_method(Klass* klass, const methodHandle& resolved_method);

  // same as above for compile-time resolution; returns vtable_index if current_klass if linked
  static int resolve_virtual_vtable_index  (Klass* receiver_klass, const LinkInfo& link_info);

  // static resolving for compiler (does not throw exceptions, returns null handle if unsuccessful)
  static methodHandle linktime_resolve_virtual_method_or_null  (const LinkInfo& link_info);
  static methodHandle linktime_resolve_interface_method_or_null(const LinkInfo& link_info);

  // runtime resolving from constant pool
  static void resolve_invoke(CallInfo& result, Handle recv, const constantPoolHandle& pool, int index, Bytecodes::Code byte, TRAPS);

  // runtime resolving from attached method
  static void resolve_invoke(CallInfo& result, Handle& recv, const methodHandle& attached_method, Bytecodes::Code byte, TRAPS);

 public:
  // Only resolved method known.
  static void throw_abstract_method_error(const methodHandle& resolved_method, TRAPS) {
    throw_abstract_method_error(resolved_method, NULL, NULL, CHECK);
  }
  // Resolved method and receiver klass know.
  static void throw_abstract_method_error(const methodHandle& resolved_method, Klass *recv_klass, TRAPS) {
    throw_abstract_method_error(resolved_method, NULL, recv_klass, CHECK);
  }
  // Selected method is abstract.
  static void throw_abstract_method_error(const methodHandle& resolved_method, const methodHandle& selected_method, Klass *recv_klass, TRAPS);
};
#endif
