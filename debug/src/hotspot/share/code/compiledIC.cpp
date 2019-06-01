#include "precompiled.hpp"

#include "classfile/systemDictionary.hpp"
#include "code/codeCache.hpp"
#include "code/compiledIC.hpp"
#include "code/icBuffer.hpp"
#include "code/nmethod.hpp"
#include "code/vtableStubs.hpp"
#include "interpreter/interpreter.hpp"
#include "interpreter/linkResolver.hpp"
#include "memory/metadataFactory.hpp"
#include "memory/oopFactory.hpp"
#include "memory/resourceArea.hpp"
#include "oops/method.inline.hpp"
#include "oops/oop.inline.hpp"
#include "oops/symbol.hpp"
#include "runtime/handles.inline.hpp"
#include "runtime/icache.hpp"
#include "runtime/sharedRuntime.hpp"
#include "runtime/stubRoutines.hpp"
#include "utilities/events.hpp"

// Every time a compiled IC is changed or its type is being accessed,
// either the CompiledIC_lock must be set or we must be at a safe point.

//-----------------------------------------------------------------------------
// Low-level access to an inline cache. Private, since they might not be
// MT-safe to use.

void* CompiledIC::cached_value() const {

  if (!is_in_transition_state()) {
    void* data = get_data();
    // If we let the metadata value here be initialized to zero...
    return (data == (void*)Universe::non_oop_word()) ? NULL : data;
  } else {
    return InlineCacheBuffer::cached_value_for((CompiledIC *)this);
  }
}

void CompiledIC::internal_set_ic_destination(address entry_point, bool is_icstub, void* cache, bool is_icholder) {

  // Don't use ic_destination for this test since that forwards
  // through ICBuffer instead of returning the actual current state of
  // the CompiledIC.
  if (is_icholder_entry(_call->destination())) {
    // When patching for the ICStub case the cached value isn't
    // overwritten until the ICStub copied into the CompiledIC during
    // the next safepoint.  Make sure that the CompiledICHolder* is
    // marked for release at this point since it won't be identifiable
    // once the entry point is overwritten.
    InlineCacheBuffer::queue_for_release((CompiledICHolder*)get_data());
  }

  if (TraceCompiledIC) {
    tty->print("  ");
    print_compiled_ic();
    tty->print(" changing destination to " INTPTR_FORMAT, p2i(entry_point));
    if (!is_optimized()) {
      tty->print(" changing cached %s to " INTPTR_FORMAT, is_icholder ? "icholder" : "metadata", p2i((address)cache));
    }
    if (is_icstub) {
      tty->print(" (icstub)");
    }
    tty->cr();
  }

  {
    MutexLockerEx pl(SafepointSynchronize::is_at_safepoint() ? NULL : Patching_lock, Mutex::_no_safepoint_check_flag);
    _call->set_destination_mt_safe(entry_point);
  }

  if (is_optimized() || is_icstub) {
    // Optimized call sites don't have a cache value and ICStub call
    // sites only change the entry point.  Changing the value in that
    // case could lead to MT safety issues.
    return;
  }

  if (cache == NULL)  cache = (void*)Universe::non_oop_word();

  set_data((intptr_t)cache);
}

void CompiledIC::set_ic_destination(ICStub* stub) {
  internal_set_ic_destination(stub->code_begin(), true, NULL, false);
}

address CompiledIC::ic_destination() const {
 if (!is_in_transition_state()) {
   return _call->destination();
 } else {
   return InlineCacheBuffer::ic_destination_for((CompiledIC *)this);
 }
}

bool CompiledIC::is_in_transition_state() const {
  return InlineCacheBuffer::contains(_call->destination());
}

bool CompiledIC::is_icholder_call() const {
  return !_is_optimized && is_icholder_entry(ic_destination());
}

// Returns native address of 'call' instruction in inline-cache. Used by
// the InlineCacheBuffer when it needs to find the stub.
address CompiledIC::stub_address() const {
  return _call->destination();
}

// Clears the IC stub if the compiled IC is in transition state
void CompiledIC::clear_ic_stub() {
  if (is_in_transition_state()) {
    ICStub* stub = ICStub_from_destination_address(stub_address());
    stub->clear();
  }
}

//-----------------------------------------------------------------------------
// High-level access to an inline cache. Guaranteed to be MT-safe.

void CompiledIC::initialize_from_iter(RelocIterator* iter) {

  if (iter->type() == relocInfo::virtual_call_type) {
    virtual_call_Relocation* r = iter->virtual_call_reloc();
    _is_optimized = false;
    _value = _call->get_load_instruction(r);
  } else {
    _is_optimized = true;
    _value = NULL;
  }
}

CompiledIC::CompiledIC(CompiledMethod* cm, NativeCall* call)
  : _method(cm)
{
  _call = _method->call_wrapper_at((address) call);
  address ic_call = _call->instruction_address();

  // Search for the ic_call at the given address.
  RelocIterator iter(cm, ic_call, ic_call+1);
  bool ret = iter.next();

  initialize_from_iter(&iter);
}

CompiledIC::CompiledIC(RelocIterator* iter)
  : _method(iter->code())
{
  _call = _method->call_wrapper_at(iter->addr());
  address ic_call = _call->instruction_address();

  CompiledMethod* nm = iter->code();

  initialize_from_iter(iter);
}

bool CompiledIC::set_to_megamorphic(CallInfo* call_info, Bytecodes::Code bytecode, TRAPS) {

  address entry;
  if (call_info->call_kind() == CallInfo::itable_call) {
    int itable_index = call_info->itable_index();
    entry = VtableStubs::find_itable_stub(itable_index);
    if (entry == NULL) {
      return false;
    }
    CompiledICHolder* holder = new CompiledICHolder(call_info->resolved_method()->method_holder(),
                                                    call_info->resolved_klass(), false);
    holder->claim();
    InlineCacheBuffer::create_transition_stub(this, holder, entry);
  } else {
    // Can be different than selected_method->vtable_index(), due to package-private etc.
    int vtable_index = call_info->vtable_index();
    entry = VtableStubs::find_vtable_stub(vtable_index);
    if (entry == NULL) {
      return false;
    }
    InlineCacheBuffer::create_transition_stub(this, NULL, entry);
  }

  if (TraceICs) {
    ResourceMark rm;
    tty->print_cr("IC@" INTPTR_FORMAT ": to megamorphic %s entry: " INTPTR_FORMAT, p2i(instruction_address()), call_info->selected_method()->print_value_string(), p2i(entry));
  }

  return true;
}

// true if destination is megamorphic stub
bool CompiledIC::is_megamorphic() const {

  // Cannot rely on cached_value. It is either an interface or a method.
  return VtableStubs::entry_point(ic_destination()) != NULL;
}

bool CompiledIC::is_call_to_compiled() const {

  // Use unsafe, since an inline cache might point to a zombie method. However, the zombie
  // method is guaranteed to still exist, since we only remove methods after all inline caches
  // has been cleaned up
  CodeBlob* cb = CodeCache::find_blob_unsafe(ic_destination());
  bool is_monomorphic = (cb != NULL && cb->is_compiled());
  // Check that the cached_value is a klass for non-optimized monomorphic calls
  // This assertion is invalid for compiler1: a call that does not look optimized (no static stub) can be used
  // for calling directly to vep without using the inline cache (i.e., cached_value == NULL).
  // For JVMCI this occurs because CHA is only used to improve inlining so call sites which could be optimized
  // virtuals because there are no currently loaded subclasses of a type are left as virtual call sites.
  return is_monomorphic;
}

bool CompiledIC::is_call_to_interpreted() const {
  // Call to interpreter if destination is either calling to a stub (if it
  // is optimized), or calling to an I2C blob
  bool is_call_to_interpreted = false;
  if (!is_optimized()) {
    // must use unsafe because the destination can be a zombie (and we're cleaning)
    // and the print_compiled_ic code wants to know if site (in the non-zombie)
    // is to the interpreter.
    CodeBlob* cb = CodeCache::find_blob_unsafe(ic_destination());
    is_call_to_interpreted = (cb != NULL && cb->is_adapter_blob());
  } else {
    // Check if we are calling into our own codeblob (i.e., to a stub)
    address dest = ic_destination();
    is_call_to_interpreted = _call->is_call_to_interpreted(dest);
  }
  return is_call_to_interpreted;
}

void CompiledIC::set_to_clean(bool in_use) {
  if (TraceInlineCacheClearing || TraceICs) {
    tty->print_cr("IC@" INTPTR_FORMAT ": set to clean", p2i(instruction_address()));
    print();
  }

  address entry = _call->get_resolve_call_stub(is_optimized());

  // A zombie transition will always be safe, since the metadata has already been set to NULL, so
  // we only need to patch the destination
  bool safe_transition = _call->is_safe_for_patching() || !in_use || is_optimized() || SafepointSynchronize::is_at_safepoint();

  if (safe_transition) {
    // Kill any leftover stub we might have too
    clear_ic_stub();
    if (is_optimized()) {
      set_ic_destination(entry);
    } else {
      set_ic_destination_and_value(entry, (void*)NULL);
    }
  } else {
    // Unsafe transition - create stub.
    InlineCacheBuffer::create_transition_stub(this, NULL, entry);
  }
}

bool CompiledIC::is_clean() const {
  bool is_clean = false;
  address dest = ic_destination();
  is_clean = dest == _call->get_resolve_call_stub(is_optimized());
  return is_clean;
}

void CompiledIC::set_to_monomorphic(CompiledICInfo& info) {
  // Updating a cache to the wrong entry can cause bugs that are very hard
  // to track down - if cache entry gets invalid - we just clean it. In
  // this way it is always the same code path that is responsible for
  // updating and resolving an inline cache
  //
  // The above is no longer true. SharedRuntime::fixup_callers_callsite will change optimized
  // callsites. In addition ic_miss code will update a site to monomorphic if it determines
  // that an monomorphic call to the interpreter can now be monomorphic to compiled code.
  //
  // In both of these cases the only thing being modifed is the jump/call target and these
  // transitions are mt_safe

  Thread *thread = Thread::current();
  if (info.to_interpreter() || info.to_aot()) {
    // Call to interpreter
    if (info.is_optimized() && is_optimized()) {
       MutexLockerEx pl(Patching_lock, Mutex::_no_safepoint_check_flag);
      // the call analysis (callee structure) specifies that the call is optimized
      // (either because of CHA or the static target is final)
      // At code generation time, this call has been emitted as static call
      // Call via stub
      methodHandle method (thread, (Method*)info.cached_metadata());
      _call->set_to_interpreted(method, info);

      if (TraceICs) {
         ResourceMark rm(thread);
         tty->print_cr("IC@" INTPTR_FORMAT ": monomorphic to %s: %s",
           p2i(instruction_address()),
           (info.to_aot() ? "aot" : "interpreter"),
           method->print_value_string());
      }
    } else {
      // Call via method-klass-holder
      InlineCacheBuffer::create_transition_stub(this, info.claim_cached_icholder(), info.entry());
      if (TraceICs) {
         ResourceMark rm(thread);
         tty->print_cr("IC@" INTPTR_FORMAT ": monomorphic to interpreter via icholder ", p2i(instruction_address()));
      }
    }
  } else {
    // Call to compiled code
    bool static_bound = info.is_optimized() || (info.cached_metadata() == NULL);

    // This is MT safe if we come from a clean-cache and go through a
    // non-verified entry point
    bool safe = SafepointSynchronize::is_at_safepoint() || (!is_in_transition_state() && (info.is_optimized() || static_bound || is_clean()));

    if (!safe) {
      InlineCacheBuffer::create_transition_stub(this, info.cached_metadata(), info.entry());
    } else {
      if (is_optimized()) {
        set_ic_destination(info.entry());
      } else {
        set_ic_destination_and_value(info.entry(), info.cached_metadata());
      }
    }

    if (TraceICs) {
      ResourceMark rm(thread);
      tty->print_cr("IC@" INTPTR_FORMAT ": monomorphic to compiled (rcvr klass) %s: %s",
        p2i(instruction_address()),
        ((Klass*)info.cached_metadata())->print_value_string(),
        (safe) ? "" : "via stub");
    }
  }
}

// is_optimized: Compiler has generated an optimized call (i.e. fixed, no inline cache)
// static_bound: The call can be static bound. If it isn't also optimized, the property
// wasn't provable at time of compilation. An optimized call will have any necessary
// null check, while a static_bound won't. A static_bound (but not optimized) must
// therefore use the unverified entry point.
void CompiledIC::compute_monomorphic_entry(const methodHandle& method, Klass* receiver_klass, bool is_optimized, bool static_bound, bool caller_is_nmethod, CompiledICInfo& info, TRAPS) {
  CompiledMethod* method_code = method->code();

  address entry = NULL;
  if (method_code != NULL && method_code->is_in_use()) {
    // Call to compiled code
    //
    // Note: the following problem exists with Compiler1:
    //   - at compile time we may or may not know if the destination is final
    //   - if we know that the destination is final (is_optimized), we will emit
    //     an optimized virtual call (no inline cache), and need a Method* to make
    //     a call to the interpreter
    //   - if we don't know if the destination is final, we emit a standard
    //     virtual call, and use CompiledICHolder to call interpreted code
    //     (no static call stub has been generated)
    //   - In the case that we here notice the call is static bound we
    //     convert the call into what looks to be an optimized virtual call,
    //     but we must use the unverified entry point (since there will be no
    //     null check on a call when the target isn't loaded).
    //     This causes problems when verifying the IC because
    //     it looks vanilla but is optimized. Code in is_call_to_interpreted
    //     is aware of this and weakens its asserts.
    if (is_optimized) {
      entry      = method_code->verified_entry_point();
    } else {
      entry      = method_code->entry_point();
    }
  }
  bool far_c2a = entry != NULL && caller_is_nmethod && method_code->is_far_code();
  if (entry != NULL && !far_c2a) {
    // Call to near compiled code (nmethod or aot).
    info.set_compiled_entry(entry, is_optimized ? NULL : receiver_klass, is_optimized);
  } else {
    if (is_optimized) {
      if (far_c2a) {
        // Call to aot code from nmethod.
        info.set_aot_entry(entry, method());
      } else {
        // Use stub entry
        info.set_interpreter_entry(method()->get_c2i_entry(), method());
      }
    } else {
      // Use icholder entry
      CompiledICHolder* holder = new CompiledICHolder(method(), receiver_klass);
      info.set_icholder_entry(method()->get_c2i_unverified_entry(), holder);
    }
  }
}

bool CompiledIC::is_icholder_entry(address entry) {
  CodeBlob* cb = CodeCache::find_blob_unsafe(entry);
  if (cb != NULL && cb->is_adapter_blob()) {
    return true;
  }
  // itable stubs also use CompiledICHolder
  if (cb != NULL && cb->is_vtable_blob()) {
    VtableStub* s = VtableStubs::entry_point(entry);
    return (s != NULL) && s->is_itable_stub();
  }

  return false;
}

bool CompiledIC::is_icholder_call_site(virtual_call_Relocation* call_site, const CompiledMethod* cm) {
  // This call site might have become stale so inspect it carefully.
  address dest = cm->call_wrapper_at(call_site->addr())->destination();
  return is_icholder_entry(dest);
}

// Release the CompiledICHolder* associated with this call site is there is one.
void CompiledIC::cleanup_call_site(virtual_call_Relocation* call_site, const CompiledMethod* cm) {
  // This call site might have become stale so inspect it carefully.
  NativeCall* call = nativeCall_at(call_site->addr());
  if (is_icholder_entry(call->destination())) {
    NativeMovConstReg* value = nativeMovConstReg_at(call_site->cached_value());
    InlineCacheBuffer::queue_for_release((CompiledICHolder*)value->data());
  }
}

// ----------------------------------------------------------------------------

void CompiledStaticCall::set_to_clean(bool in_use) {
  // in_use is unused but needed to match template function in CompiledMethod
  // Reset call site
  MutexLockerEx pl(SafepointSynchronize::is_at_safepoint() ? NULL : Patching_lock, Mutex::_no_safepoint_check_flag);

  set_destination_mt_safe(resolve_call_stub());

  // Do not reset stub here:  It is too expensive to call find_stub.
  // Instead, rely on caller (nmethod::clear_inline_caches) to clear
  // both the call and its stub.
}

bool CompiledStaticCall::is_clean() const {
  return destination() == resolve_call_stub();
}

bool CompiledStaticCall::is_call_to_compiled() const {
  return CodeCache::contains(destination());
}

bool CompiledDirectStaticCall::is_call_to_interpreted() const {
  // It is a call to interpreted, if it calls to a stub. Hence, the destination
  // must be in the stub part of the nmethod that contains the call
  CompiledMethod* cm = CodeCache::find_compiled(instruction_address());
  return cm->stub_contains(destination());
}

bool CompiledDirectStaticCall::is_call_to_far() const {
  // It is a call to aot method, if it calls to a stub. Hence, the destination
  // must be in the stub part of the nmethod that contains the call
  CodeBlob* desc = CodeCache::find_blob(instruction_address());
  return desc->as_compiled_method()->stub_contains(destination());
}

void CompiledStaticCall::set_to_compiled(address entry) {
  if (TraceICs) {
    ResourceMark rm;
    tty->print_cr("%s@" INTPTR_FORMAT ": set_to_compiled " INTPTR_FORMAT,
        name(),
        p2i(instruction_address()),
        p2i(entry));
  }
  // Call to compiled code
  set_destination_mt_safe(entry);
}

void CompiledStaticCall::set(const StaticCallInfo& info) {
  MutexLockerEx pl(Patching_lock, Mutex::_no_safepoint_check_flag);
  // Updating a cache to the wrong entry can cause bugs that are very hard
  // to track down - if cache entry gets invalid - we just clean it. In
  // this way it is always the same code path that is responsible for
  // updating and resolving an inline cache

  if (info._to_interpreter) {
    // Call to interpreted code
    set_to_interpreted(info.callee(), info.entry());
  } else {
    set_to_compiled(info.entry());
  }
}

// Compute settings for a CompiledStaticCall. Since we might have to set
// the stub when calling to the interpreter, we need to return arguments.
void CompiledStaticCall::compute_entry(const methodHandle& m, bool caller_is_nmethod, StaticCallInfo& info) {
  CompiledMethod* m_code = m->code();
  info._callee = m;
  if (m_code != NULL && m_code->is_in_use()) {
    if (caller_is_nmethod && m_code->is_far_code()) {
      // Call to far aot code from nmethod.
      info._to_aot = true;
    } else {
      info._to_aot = false;
    }
    info._to_interpreter = false;
    info._entry  = m_code->verified_entry_point();
  } else {
    // Callee is interpreted code.  In any case entering the interpreter
    // puts a converter-frame on the stack to save arguments.
    info._to_interpreter = true;
    info._entry      = m()->get_c2i_entry();
  }
}

address CompiledDirectStaticCall::find_stub_for(address instruction, bool is_aot) {
  // Find reloc. information containing this call-site
  RelocIterator iter((nmethod*)NULL, instruction);
  while (iter.next()) {
    if (iter.addr() == instruction) {
      switch(iter.type()) {
        case relocInfo::static_call_type:
          return iter.static_call_reloc()->static_stub(is_aot);
        // We check here for opt_virtual_call_type, since we reuse the code
        // from the CompiledIC implementation
        case relocInfo::opt_virtual_call_type:
          return iter.opt_virtual_call_reloc()->static_stub(is_aot);
        case relocInfo::poll_type:
        case relocInfo::poll_return_type: // A safepoint can't overlap a call.
        default:
          ShouldNotReachHere();
      }
    }
  }
  return NULL;
}

address CompiledDirectStaticCall::find_stub(bool is_aot) {
  return CompiledDirectStaticCall::find_stub_for(instruction_address(), is_aot);
}

address CompiledDirectStaticCall::resolve_call_stub() const {
  return SharedRuntime::get_resolve_static_call_stub();
}
