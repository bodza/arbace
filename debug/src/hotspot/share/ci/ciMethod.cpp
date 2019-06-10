#include "precompiled.hpp"

#include "ci/ciExceptionHandler.hpp"
#include "ci/ciInstanceKlass.hpp"
#include "ci/ciMethod.hpp"
#include "ci/ciMethodBlocks.hpp"
#include "ci/ciMethodData.hpp"
#include "ci/ciStreams.hpp"
#include "ci/ciSymbol.hpp"
#include "ci/ciUtilities.inline.hpp"
#include "classfile/systemDictionary.hpp"
#include "compiler/abstractCompiler.hpp"
#include "compiler/methodLiveness.hpp"
#include "interpreter/linkResolver.hpp"
#include "memory/allocation.inline.hpp"
#include "memory/resourceArea.hpp"
#include "oops/generateOopMap.hpp"
#include "oops/method.inline.hpp"
#include "oops/oop.inline.hpp"
#include "prims/nativeLookup.hpp"
#include "utilities/bitMap.inline.hpp"
#include "utilities/xmlstream.hpp"

// ciMethod
//
// This class represents a Method* in the HotSpot virtual
// machine.

// Loaded method.
ciMethod::ciMethod(const methodHandle& h_m, ciInstanceKlass* holder) : ciMetadata(h_m()), _holder(holder) {
  // These fields are always filled in in loaded methods.
  _flags = ciFlags(h_m()->access_flags());

  // Easy to compute, so fill them in now.
  _max_stack          = h_m()->max_stack();
  _max_locals         = h_m()->max_locals();
  _code_size          = h_m()->code_size();
  _intrinsic_id       = h_m()->intrinsic_id();
  _handler_count      = h_m()->exception_table_length();
  _size_of_parameters = h_m()->size_of_parameters();
  _uses_monitors      = h_m()->access_flags().has_monitor_bytecodes();
  _balanced_monitors  = !_uses_monitors || h_m()->access_flags().is_monitor_matching();
  _is_c1_compilable   = !h_m()->is_not_c1_compilable();
  _is_c2_compilable   = !h_m()->is_not_c2_compilable();
  _can_be_parsed      = true;
  // Lazy fields, filled in on demand.  Require allocation.
  _code               = NULL;
  _exception_handlers = NULL;
  _liveness           = NULL;
  _method_blocks = NULL;

  ciEnv *env = ciEnv::current();

  if (h_m()->method_holder()->is_linked()) {
    _can_be_statically_bound = h_m()->can_be_statically_bound();
  } else {
    // Have to use a conservative value in this case.
    _can_be_statically_bound = false;
  }

  // Adjust the definition of this condition to be more useful:
  // %%% take these conditions into account in vtable generation
  if (!_can_be_statically_bound && h_m()->is_private())
    _can_be_statically_bound = true;
  if (_can_be_statically_bound && h_m()->is_abstract())
    _can_be_statically_bound = false;

  // generating _signature may allow GC and therefore move m.
  // These fields are always filled in.
  _name = env->get_symbol(h_m()->name());
  ciSymbol* sig_symbol = env->get_symbol(h_m()->signature());
  constantPoolHandle cpool = h_m()->constants();
  _signature = new (env->arena()) ciSignature(_holder, cpool, sig_symbol);
  _method_data = NULL;
  _nmethod_age = h_m()->nmethod_age();
  // Take a snapshot of these values, so they will be commensurate with the MDO.
  {
    _interpreter_invocation_count = 0;
    _interpreter_throwout_count = 0;
  }
  if (_interpreter_invocation_count == 0)
    _interpreter_invocation_count = 1;
  _instructions_size = -1;
}

// Unloaded method.
ciMethod::ciMethod(ciInstanceKlass* holder, ciSymbol* name, ciSymbol* signature, ciInstanceKlass* accessor) : ciMetadata((Metadata*)NULL),
  _name(                   name),
  _holder(                 holder),
  _intrinsic_id(           vmIntrinsics::_none),
  _liveness(               NULL),
  _can_be_statically_bound(false),
  _method_blocks(          NULL),
  _method_data(            NULL)
{
  // Usually holder and accessor are the same type but in some cases
  // the holder has the wrong class loader (e.g. invokedynamic call
  // sites) so we pass the accessor.
  _signature = new (ciEnv::current()->arena()) ciSignature(accessor, constantPoolHandle(), signature);
}

// Load the bytecodes and exception handler table for this method.
void ciMethod::load_code() {
  VM_ENTRY_MARK;

  Method* me = get_Method();
  Arena* arena = ciEnv::current(thread)->arena();

  // Load the bytecodes.
  _code = (address)arena->Amalloc(code_size());
  memcpy(_code, me->code_base(), code_size());

  // And load the exception table.
  ExceptionTable exc_table(me);

  // Allocate one extra spot in our list of exceptions.  This
  // last entry will be used to represent the possibility that
  // an exception escapes the method.  See ciExceptionHandlerStream
  // for details.
  _exception_handlers = (ciExceptionHandler**)arena->Amalloc(sizeof(ciExceptionHandler*) * (_handler_count + 1));
  if (_handler_count > 0) {
    for (int i = 0; i<_handler_count; i++) {
      _exception_handlers[i] = new (arena) ciExceptionHandler(
                                holder(),
            /* start    */      exc_table.start_pc(i),
            /* limit    */      exc_table.end_pc(i),
            /* goto pc  */      exc_table.handler_pc(i),
            /* cp index */      exc_table.catch_type_index(i));
    }
  }

  // Put an entry at the end of our list to represent the possibility
  // of exceptional exit.
  _exception_handlers[_handler_count] = new (arena) ciExceptionHandler(holder(), 0, code_size(), -1, 0);
}

// length unknown until decompression
bool ciMethod::has_linenumber_table() const {
  check_is_loaded();
  VM_ENTRY_MARK;
  return get_Method()->has_linenumber_table();
}

u_char* ciMethod::compressed_linenumber_table() const {
  check_is_loaded();
  VM_ENTRY_MARK;
  return get_Method()->compressed_linenumber_table();
}

int ciMethod::line_number_from_bci(int bci) const {
  check_is_loaded();
  VM_ENTRY_MARK;
  return get_Method()->line_number_from_bci(bci);
}

// Get the position of this method's entry in the vtable, if any.
int ciMethod::vtable_index() {
  check_is_loaded();
  VM_ENTRY_MARK;
  return get_Method()->vtable_index();
}

// Get the address of this method's native code, if any.
address ciMethod::native_entry() {
  check_is_loaded();
  VM_ENTRY_MARK;
  Method* method = get_Method();
  address entry = method->native_function();
  return entry;
}

// Does this method use monitors in a strict stack-disciplined manner?
bool ciMethod::has_balanced_monitors() {
  check_is_loaded();
  if (_balanced_monitors) return true;

  // Analyze the method to see if monitors are used properly.
  VM_ENTRY_MARK;
  methodHandle method(THREAD, get_Method());

  // Check to see if a previous compilation computed the
  // monitor-matching analysis.
  if (method->guaranteed_monitor_matching()) {
    _balanced_monitors = true;
    return true;
  }

  {
    EXCEPTION_MARK;
    ResourceMark rm(THREAD);
    GeneratePairingInfo gpi(method);
    gpi.compute_map(CATCH);
    if (!gpi.monitor_safe()) {
      return false;
    }
    method->set_guaranteed_monitor_matching();
    _balanced_monitors = true;
  }
  return true;
}

ciTypeFlow* ciMethod::get_flow_analysis() {
  ShouldNotReachHere();
  return NULL;
}

// Which local variables are live at a specific bci?
MethodLivenessResult ciMethod::raw_liveness_at_bci(int bci) {
  check_is_loaded();
  if (_liveness == NULL) {
    // Create the liveness analyzer.
    Arena* arena = ciEnv::current()->arena();
    _liveness = new (arena) MethodLiveness(arena, this);
    _liveness->compute_liveness();
  }
  return _liveness->get_liveness_at(bci);
}

// Which local variables are live at a specific bci?  When debugging
// will return true for all locals in some cases to improve debug
// information.
MethodLivenessResult ciMethod::liveness_at_bci(int bci) {
  return raw_liveness_at_bci(bci);
}

// Marks all bcis where a new basic block starts
const BitMap& ciMethod::bci_block_start() {
  check_is_loaded();
  if (_liveness == NULL) {
    // Create the liveness analyzer.
    Arena* arena = ciEnv::current()->arena();
    _liveness = new (arena) MethodLiveness(arena, this);
    _liveness->compute_liveness();
  }

  return _liveness->get_bci_block_start();
}

// Given a certain calling environment, find the monomorphic target
// for the call.  Return NULL if the call is not monomorphic in
// its calling environment, or if there are only abstract methods.
// The returned method is never abstract.
// Note: If caller uses a non-null result, it must inform dependencies
// via assert_unique_concrete_method or assert_leaf_type.
ciMethod* ciMethod::find_monomorphic_target(ciInstanceKlass* caller, ciInstanceKlass* callee_holder, ciInstanceKlass* actual_recv, bool check_access) {
  check_is_loaded();

  if (actual_recv->is_interface()) {
    // %%% We cannot trust interface types, yet.  See bug 6312651.
    return NULL;
  }

  ciMethod* root_m = resolve_invoke(caller, actual_recv, check_access);
  if (root_m == NULL) {
    // Something went wrong looking up the actual receiver method.
    return NULL;
  }

  // Is it private or final?
  if (root_m->can_be_statically_bound()) {
    return root_m;
  }

  if (actual_recv->is_leaf_type() && actual_recv == root_m->holder()) {
    // Easy case.  There is no other place to put a method, so don't bother
    // to go through the VM_ENTRY_MARK and all the rest.
    return root_m;
  }

  // Array methods (clone, hashCode, etc.) are always statically bound.
  // If we were to see an array type here, we'd return root_m.
  // However, this method processes only ciInstanceKlasses.  (See 4962591.)
  // The inline_native_clone intrinsic narrows Object to T[] properly,
  // so there is no need to do the same job here.

  return NULL;
}

// Given a known receiver klass, find the target for the call.
// Return NULL if the call has no target or the target is abstract.
ciMethod* ciMethod::resolve_invoke(ciKlass* caller, ciKlass* exact_receiver, bool check_access) {
   check_is_loaded();
   VM_ENTRY_MARK;

   Klass* caller_klass = caller->get_Klass();
   Klass* recv         = exact_receiver->get_Klass();
   Klass* resolved     = holder()->get_Klass();
   Symbol* h_name      = name()->get_symbol();
   Symbol* h_signature = signature()->get_symbol();

   LinkInfo link_info(resolved, h_name, h_signature, caller_klass, check_access ? LinkInfo::needs_access_check : LinkInfo::skip_access_check);
   methodHandle m;
   // Only do exact lookup if receiver klass has been linked.  Otherwise,
   // the vtable has not been setup, and the LinkResolver will fail.
   if (recv->is_array_klass() || (InstanceKlass::cast(recv)->is_linked() && !exact_receiver->is_interface())) {
     if (holder()->is_interface()) {
       m = LinkResolver::resolve_interface_call_or_null(recv, link_info);
     } else {
       m = LinkResolver::resolve_virtual_call_or_null(recv, link_info);
     }
   }

   if (m.is_null()) {
     // Return NULL only if there was a problem with lookup (uninitialized class, etc.)
     return NULL;
   }

   ciMethod* result = this;
   if (m() != get_Method()) {
     result = ciEnv::current(thread)->get_method(m());
   }

   // Don't return abstract methods because they aren't
   // optimizable or interesting.
   if (result->is_abstract()) {
     return NULL;
   } else {
     return result;
   }
}

// Given a known receiver klass, find the vtable index for the call.
// Return Method::invalid_vtable_index if the vtable_index is unknown.
int ciMethod::resolve_vtable_index(ciKlass* caller, ciKlass* receiver) {
   check_is_loaded();

   int vtable_index = Method::invalid_vtable_index;
   // Only do lookup if receiver klass has been linked.  Otherwise,
   // the vtable has not been setup, and the LinkResolver will fail.
   if (!receiver->is_interface() && (!receiver->is_instance_klass() || receiver->as_instance_klass()->is_linked())) {
     VM_ENTRY_MARK;

     Klass* caller_klass = caller->get_Klass();
     Klass* recv         = receiver->get_Klass();
     Symbol* h_name = name()->get_symbol();
     Symbol* h_signature = signature()->get_symbol();

     LinkInfo link_info(recv, h_name, h_signature, caller_klass);
     vtable_index = LinkResolver::resolve_virtual_vtable_index(recv, link_info);
     if (vtable_index == Method::nonvirtual_vtable_index) {
       // A statically bound method.  Return "no such index".
       vtable_index = Method::invalid_vtable_index;
     }
   }

   return vtable_index;
}

ciField* ciMethod::get_field_at_bci(int bci, bool &will_link) {
  ciBytecodeStream iter(this);
  iter.reset_to_bci(bci);
  iter.next();
  return iter.get_field(will_link);
}

ciMethod* ciMethod::get_method_at_bci(int bci, bool &will_link, ciSignature* *declared_signature) {
  ciBytecodeStream iter(this);
  iter.reset_to_bci(bci);
  iter.next();
  return iter.get_method(will_link, declared_signature);
}

// ------------------------------------------------------------------
// Adjust a CounterData count to be commensurate with
// interpreter_invocation_count.  If the MDO exists for
// only 25% of the time the method exists, then the
// counts in the MDO should be scaled by 4X, so that
// they can be usefully and stably compared against the
// invocation counts in methods.
int ciMethod::scale_count(int count, float prof_factor) {
  if (count > 0 && method_data() != NULL) {
    int counter_life;
    int method_life = interpreter_invocation_count();
    {
      int current_mileage = method_data()->current_mileage();
      int creation_mileage = method_data()->creation_mileage();
      counter_life = current_mileage - creation_mileage;
    }

    // counter_life due to backedge_counter could be > method_life
    if (counter_life > method_life)
      counter_life = method_life;
    if (0 < counter_life && counter_life <= method_life) {
      count = (int)((double)count * prof_factor * method_life / counter_life + 0.5);
      count = (count > 0) ? count : 1;
    }
  }
  return count;
}

bool ciMethod::is_ignored_by_security_stack_walk() const {
  check_is_loaded();
  VM_ENTRY_MARK;
  return get_Method()->is_ignored_by_security_stack_walk();
}

// ------------------------------------------------------------------
// invokedynamic support

bool ciMethod::is_object_initializer() const {
   return name() == ciSymbol::object_initializer_name();
}

// Generate new MethodData* objects at compile time.
// Return true if allocation was successful or no MDO is required.
bool ciMethod::ensure_method_data(const methodHandle& h_m) {
  EXCEPTION_CONTEXT;
  if (is_native() || is_abstract() || h_m()->is_accessor()) {
    return true;
  }
  if (h_m()->method_data() == NULL) {
    Method::build_interpreter_method_data(h_m, THREAD);
    if (HAS_PENDING_EXCEPTION) {
      CLEAR_PENDING_EXCEPTION;
    }
  }
  if (h_m()->method_data() != NULL) {
    _method_data = ciEnv::current()->get_method_data(h_m()->method_data());
    _method_data->load_data();
    return true;
  } else {
    _method_data = ciEnv::current()->get_empty_methodData();
    return false;
  }
}

// public, retroactive version
bool ciMethod::ensure_method_data() {
  bool result = true;
  if (_method_data == NULL || _method_data->is_empty()) {
    GUARDED_VM_ENTRY({
      result = ensure_method_data(get_Method());
    });
  }
  return result;
}

ciMethodData* ciMethod::method_data() {
  if (_method_data != NULL) {
    return _method_data;
  }
  VM_ENTRY_MARK;
  ciEnv* env = ciEnv::current();
  Thread* my_thread = JavaThread::current();
  methodHandle h_m(my_thread, get_Method());

  if (h_m()->method_data() != NULL) {
    _method_data = ciEnv::current()->get_method_data(h_m()->method_data());
    _method_data->load_data();
  } else {
    _method_data = ciEnv::current()->get_empty_methodData();
  }
  return _method_data;
}

MethodCounters* ciMethod::ensure_method_counters() {
  check_is_loaded();
  VM_ENTRY_MARK;
  methodHandle mh(THREAD, get_Method());
  MethodCounters* method_counters = mh->get_method_counters(CHECK_NULL);
  return method_counters;
}

bool ciMethod::has_option(const char* option) {
  check_is_loaded();
  VM_ENTRY_MARK;
  methodHandle mh(THREAD, get_Method());
  return CompilerOracle::has_option_string(mh, option);
}

bool ciMethod::has_option_value(const char* option, double& value) {
  check_is_loaded();
  VM_ENTRY_MARK;
  methodHandle mh(THREAD, get_Method());
  return CompilerOracle::has_option_value(mh, option, value);
}

// Have previous compilations of this method succeeded?
bool ciMethod::can_be_compiled() {
  check_is_loaded();
  ciEnv* env = ciEnv::current();
  if (is_c1_compile(env->comp_level())) {
    return _is_c1_compilable;
  }
  return _is_c2_compilable;
}

// Tell the VM that this method cannot be compiled at all.
void ciMethod::set_not_compilable(const char* reason) {
  check_is_loaded();
  VM_ENTRY_MARK;
  ciEnv* env = ciEnv::current();
  if (is_c1_compile(env->comp_level())) {
    _is_c1_compilable = false;
  } else {
    _is_c2_compilable = false;
  }
  get_Method()->set_not_compilable(env->comp_level(), true, reason);
}

bool ciMethod::has_compiled_code() {
  return instructions_size() > 0;
}

int ciMethod::comp_level() {
  check_is_loaded();
  VM_ENTRY_MARK;
  CompiledMethod* nm = get_Method()->code();
  if (nm != NULL) return nm->comp_level();
  return 0;
}

// Code size for inlining decisions.  This method returns a code
// size of 1 for methods which has the ForceInline annotation.
int ciMethod::code_size_for_inlining() {
  check_is_loaded();
  if (get_Method()->force_inline()) {
    return 1;
  }
  return code_size();
}

// This is a rough metric for "fat" methods, compared before inlining
// with InlineSmallCode.  The CodeBlob::code_size accessor includes
// junk like exception handler, stubs, and constant table, which are
// not highly relevant to an inlined method.  So we use the more
// specific accessor nmethod::insts_size.
int ciMethod::instructions_size() {
  if (_instructions_size == -1) {
    GUARDED_VM_ENTRY(
        CompiledMethod* code = get_Method()->code();
        if (code != NULL && (code->comp_level() == CompLevel_full_optimization)) {
          _instructions_size = code->insts_end() - code->verified_entry_point();
        } else {
          _instructions_size = 0;
        }
    );
  }
  return _instructions_size;
}

bool ciMethod::was_executed_more_than(int times) {
  VM_ENTRY_MARK;
  return get_Method()->was_executed_more_than(times);
}

bool ciMethod::has_unloaded_classes_in_signature() {
  VM_ENTRY_MARK;
  {
    EXCEPTION_MARK;
    methodHandle m(THREAD, get_Method());
    bool has_unloaded = Method::has_unloaded_classes_in_signature(m, (JavaThread *)THREAD);
    if (HAS_PENDING_EXCEPTION) {
      CLEAR_PENDING_EXCEPTION;
      return true;     // Declare that we may have unloaded classes
    }
    return has_unloaded;
  }
}

bool ciMethod::is_klass_loaded(int refinfo_index, bool must_be_resolved) const {
  VM_ENTRY_MARK;
  return get_Method()->is_klass_loaded(refinfo_index, must_be_resolved);
}

bool ciMethod::check_call(int refinfo_index, bool is_static) const {
  // This method is used only in C2 from InlineTree::ok_to_inline,
  // and is only used under -Xcomp.
  // It appears to fail when applied to an invokeinterface call site.
  // FIXME: Remove this method and resolve_method_statically; refactor to use the other LinkResolver entry points.
  VM_ENTRY_MARK;
  {
    EXCEPTION_MARK;
    HandleMark hm(THREAD);
    constantPoolHandle pool (THREAD, get_Method()->constants());
    Bytecodes::Code code = (is_static ? Bytecodes::_invokestatic : Bytecodes::_invokevirtual);
    methodHandle spec_method = LinkResolver::resolve_method_statically(code, pool, refinfo_index, THREAD);
    if (HAS_PENDING_EXCEPTION) {
      CLEAR_PENDING_EXCEPTION;
      return false;
    } else {
      return (spec_method->is_static() == is_static);
    }
  }
  return false;
}

#define FETCH_FLAG_FROM_VM(flag_accessor) { \
  check_is_loaded(); \
  VM_ENTRY_MARK; \
  return get_Method()->flag_accessor(); \
}

bool ciMethod::is_empty_method() const { FETCH_FLAG_FROM_VM(is_empty_method); }
bool ciMethod::is_vanilla_constructor() const { FETCH_FLAG_FROM_VM(is_vanilla_constructor); }
bool ciMethod::has_loops () const { FETCH_FLAG_FROM_VM(has_loops); }
bool ciMethod::has_jsrs () const { FETCH_FLAG_FROM_VM(has_jsrs); }
bool ciMethod::is_getter () const { FETCH_FLAG_FROM_VM(is_getter); }
bool ciMethod::is_setter () const { FETCH_FLAG_FROM_VM(is_setter); }
bool ciMethod::is_accessor () const { FETCH_FLAG_FROM_VM(is_accessor); }
bool ciMethod::is_initializer () const { FETCH_FLAG_FROM_VM(is_initializer); }

bool ciMethod::is_boxing_method() const {
  if (holder()->is_box_klass()) {
    switch (intrinsic_id()) {
      case vmIntrinsics::_Boolean_valueOf:
      case vmIntrinsics::_Byte_valueOf:
      case vmIntrinsics::_Character_valueOf:
      case vmIntrinsics::_Short_valueOf:
      case vmIntrinsics::_Integer_valueOf:
      case vmIntrinsics::_Long_valueOf:
      case vmIntrinsics::_Float_valueOf:
      case vmIntrinsics::_Double_valueOf:
        return true;
      default:
        return false;
    }
  }
  return false;
}

bool ciMethod::is_unboxing_method() const {
  if (holder()->is_box_klass()) {
    switch (intrinsic_id()) {
      case vmIntrinsics::_booleanValue:
      case vmIntrinsics::_byteValue:
      case vmIntrinsics::_charValue:
      case vmIntrinsics::_shortValue:
      case vmIntrinsics::_intValue:
      case vmIntrinsics::_longValue:
      case vmIntrinsics::_floatValue:
      case vmIntrinsics::_doubleValue:
        return true;
      default:
        return false;
    }
  }
  return false;
}

ciMethodBlocks *ciMethod::get_method_blocks() {
  Arena *arena = ciEnv::current()->arena();
  if (_method_blocks == NULL) {
    _method_blocks = new (arena) ciMethodBlocks(arena, this);
  }
  return _method_blocks;
}

#undef FETCH_FLAG_FROM_VM

void ciMethod::dump_name_as_ascii(outputStream* st) {
  Method* method = get_Method();
  st->print("%s %s %s",
            method->klass_name()->as_quoted_ascii(),
            method->name()->as_quoted_ascii(),
            method->signature()->as_quoted_ascii());
}

// Print the name of this method, including signature and some flags.
void ciMethod::print_name(outputStream* st) {
  check_is_loaded();
  GUARDED_VM_ENTRY(get_Method()->print_name(st);)
}

// Print the name of this method, without signature.
void ciMethod::print_short_name(outputStream* st) {
  if (is_loaded()) {
    GUARDED_VM_ENTRY(get_Method()->print_short_name(st););
  } else {
    // Fall back if method is not loaded.
    holder()->print_name_on(st);
    st->print("::");
    name()->print_symbol_on(st);
  }
}

// Implementation of the print method.
void ciMethod::print_impl(outputStream* st) {
  ciMetadata::print_impl(st);
  st->print(" name=");
  name()->print_symbol_on(st);
  st->print(" holder=");
  holder()->print_name_on(st);
  st->print(" signature=");
  signature()->as_symbol()->print_symbol_on(st);
  if (is_loaded()) {
    st->print(" loaded=true");
    st->print(" arg_size=%d", arg_size());
    st->print(" flags=");
    flags().print_member_flags(st);
  } else {
    st->print(" loaded=false");
  }
}

// ------------------------------------------------------------------

static BasicType erase_to_word_type(BasicType bt) {
  if (is_subword_type(bt)) return T_INT;
  if (bt == T_ARRAY)       return T_OBJECT;
  return bt;
}

static bool basic_types_match(ciType* t1, ciType* t2) {
  if (t1 == t2)  return true;
  return erase_to_word_type(t1->basic_type()) == erase_to_word_type(t2->basic_type());
}

bool ciMethod::is_consistent_info(ciMethod* declared_method, ciMethod* resolved_method) {
  {
    // Method name & descriptor should stay the same.
    // Signatures may reference unloaded types and thus they may be not strictly equal.
    ciSymbol* declared_signature = declared_method->signature()->as_symbol();
    ciSymbol* resolved_signature = resolved_method->signature()->as_symbol();

    return (declared_method->name()->equals(resolved_method->name())) && (declared_signature->equals(resolved_signature));
  }
}
