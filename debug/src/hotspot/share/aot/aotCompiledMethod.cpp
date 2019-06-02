#include "precompiled.hpp"

#include "aot/aotCodeHeap.hpp"
#include "aot/aotLoader.hpp"
#include "aot/compiledIC_aot.hpp"
#include "code/codeCache.hpp"
#include "code/compiledIC.hpp"
#include "code/nativeInst.hpp"
#include "compiler/compilerOracle.hpp"
#include "gc/shared/cardTableBarrierSet.hpp"
#include "gc/shared/collectedHeap.hpp"
#include "jvmci/compilerRuntime.hpp"
#include "jvmci/jvmciRuntime.hpp"
#include "oops/method.inline.hpp"
#include "runtime/frame.inline.hpp"
#include "runtime/handles.inline.hpp"
#include "runtime/java.hpp"
#include "runtime/os.hpp"
#include "runtime/safepointVerifiers.hpp"
#include "runtime/sharedRuntime.hpp"
#include "utilities/xmlstream.hpp"

#include <stdio.h>

address* AOTCompiledMethod::orig_pc_addr(const frame* fr) {
  return (address*) ((address)fr->unextended_sp() + _meta->orig_pc_offset());
}

bool AOTCompiledMethod::do_unloading_oops(address low_boundary, BoolObjectClosure* is_alive) {
  return false;
}

oop AOTCompiledMethod::oop_at(int index) const {
  if (index == 0) { // 0 is reserved
    return NULL;
  }
  Metadata** entry = _metadata_got + (index - 1);
  intptr_t meta = (intptr_t)*entry;
  if ((meta & 1) == 1) {
    // already resolved
    Klass* k = (Klass*)(meta & ~1);
    return k->java_mirror();
  }
  // The entry is string which we need to resolve.
  const char* meta_name = _heap->get_name_at((int)meta);
  int klass_len = build_u2_from((address)meta_name);
  const char* klass_name = meta_name + 2;
  // Quick check the current method's holder.
  Klass* k = _method->method_holder();

  ResourceMark rm; // for signature_name()
  if (strncmp(k->signature_name(), klass_name, klass_len) != 0) { // Does not match?
    // Search klass in got cells in DSO which have this compiled method.
    k = _heap->get_klass_from_got(klass_name, klass_len, _method);
  }
  int method_name_len = build_u2_from((address)klass_name + klass_len);
  guarantee(method_name_len == 0, "only klass is expected here");
  meta = ((intptr_t)k) | 1;
  *entry = (Metadata*)meta; // Should be atomic on x64
  return k->java_mirror();
}

Metadata* AOTCompiledMethod::metadata_at(int index) const {
  if (index == 0) { // 0 is reserved
    return NULL;
  }
  {
    Metadata** entry = _metadata_got + (index - 1);
    intptr_t meta = (intptr_t)*entry;
    if ((meta & 1) == 1) {
      // already resolved
      Metadata *m = (Metadata*)(meta & ~1);
      return m;
    }
    // The entry is string which we need to resolve.
    const char* meta_name = _heap->get_name_at((int)meta);
    int klass_len = build_u2_from((address)meta_name);
    const char* klass_name = meta_name + 2;
    // Quick check the current method's holder.
    Klass* k = _method->method_holder();
    bool klass_matched = true;

    ResourceMark rm; // for signature_name() and find_method()
    if (strncmp(k->signature_name(), klass_name, klass_len) != 0) { // Does not match?
      // Search klass in got cells in DSO which have this compiled method.
      k = _heap->get_klass_from_got(klass_name, klass_len, _method);
      klass_matched = false;
    }
    int method_name_len = build_u2_from((address)klass_name + klass_len);
    if (method_name_len == 0) { // Array or Klass name only?
      meta = ((intptr_t)k) | 1;
      *entry = (Metadata*)meta; // Should be atomic on x64
      return (Metadata*)k;
    } else { // Method
      // Quick check the current method's name.
      Method* m = _method;
      int signature_len = build_u2_from((address)klass_name + klass_len + 2 + method_name_len);
      int full_len = 2 + klass_len + 2 + method_name_len + 2 + signature_len;
      if (!klass_matched || memcmp(_name, meta_name, full_len) != 0) { // Does not match?
        Thread* thread = Thread::current();
        const char* method_name = klass_name + klass_len;
        m = AOTCodeHeap::find_method(k, thread, method_name);
      }
      meta = ((intptr_t)m) | 1;
      *entry = (Metadata*)meta; // Should be atomic on x64
      return (Metadata*)m;
    }
  }
  ShouldNotReachHere(); return NULL;
}

bool AOTCompiledMethod::make_not_entrant_helper(int new_state) {
  // Make sure the method is not flushed in case of a safepoint in code below.
  methodHandle the_method(method());
  NoSafepointVerifier nsv;

  {
    // Enter critical section.  Does not block for safepoint.
    MutexLockerEx pl(Patching_lock, Mutex::_no_safepoint_check_flag);

    if (*_state_adr == new_state) {
      // another thread already performed this transition so nothing
      // to do, but return false to indicate this.
      return false;
    }

    // Change state
    OrderAccess::storestore();
    *_state_adr = new_state;

    // Log the transition once
    log_state_change();

    // Remove AOTCompiledMethod from method.
    if (method() != NULL && (method()->code() == this || method()->from_compiled_entry() == verified_entry_point())) {
      HandleMark hm;
      method()->clear_code(false /* already owns Patching_lock */);
    }
  }

  return true;
}

bool AOTCompiledMethod::make_entrant() {

  // Make sure the method is not flushed in case of a safepoint in code below.
  methodHandle the_method(method());
  NoSafepointVerifier nsv;

  {
    // Enter critical section.  Does not block for safepoint.
    MutexLockerEx pl(Patching_lock, Mutex::_no_safepoint_check_flag);

    if (*_state_adr == in_use) {
      // another thread already performed this transition so nothing
      // to do, but return false to indicate this.
      return false;
    }

    // Change state
    OrderAccess::storestore();
    *_state_adr = in_use;

    // Log the transition once
    log_state_change();
  }

  return true;
}

// Iterate over metadata calling this function.   Used by RedefineClasses
// Copied from nmethod::metadata_do
void AOTCompiledMethod::metadata_do(void f(Metadata*)) {
  address low_boundary = verified_entry_point();
  {
    // Visit all immediate references that are embedded in the instruction stream.
    RelocIterator iter(this, low_boundary);
    while (iter.next()) {
      if (iter.type() == relocInfo::metadata_type ) {
        metadata_Relocation* r = iter.metadata_reloc();
        // In this metadata, we must only follow those metadatas directly embedded in
        // the code.  Other metadatas (oop_index>0) are seen as part of
        // the metadata section below.
        if (r->metadata_is_immediate() && r->metadata_value() != NULL) {
          Metadata* md = r->metadata_value();
          if (md != _method) f(md);
        }
      } else if (iter.type() == relocInfo::virtual_call_type) {
        ResourceMark rm;
        // Check compiledIC holders associated with this nmethod
        CompiledIC *ic = CompiledIC_at(&iter);
        if (ic->is_icholder_call()) {
          CompiledICHolder* cichk = ic->cached_icholder();
          f(cichk->holder_metadata());
          f(cichk->holder_klass());
        } else {
          // Get Klass* or NULL (if value is -1) from GOT cell of virtual call PLT stub.
          Metadata* ic_oop = ic->cached_metadata();
          if (ic_oop != NULL) {
            f(ic_oop);
          }
        }
      } else if (iter.type() == relocInfo::static_call_type || iter.type() == relocInfo::opt_virtual_call_type) {
        // Check Method* in AOT c2i stub for other calls.
        Metadata* meta = (Metadata*)nativeLoadGot_at(nativePltCall_at(iter.addr())->plt_c2i_stub())->data();
        if (meta != NULL) {
          f(meta);
        }
      }
    }
  }

  // Visit the metadata section
  for (Metadata** p = metadata_begin(); p < metadata_end(); p++) {
    Metadata* m = *p;

    intptr_t meta = (intptr_t)m;
    if ((meta & 1) == 1) {
      // already resolved
      m = (Metadata*)(meta & ~1);
    } else {
      continue;
    }
    f(m);
  }

  // Visit metadata not embedded in the other places.
  if (_method != NULL) f(_method);
}

void AOTCompiledMethod::print() const {
  print_on(tty, "AOTCompiledMethod");
}

void AOTCompiledMethod::print_on(outputStream* st) const {
  print_on(st, "AOTCompiledMethod");
}

// Print out more verbose output usually for a newly created aot method.
void AOTCompiledMethod::print_on(outputStream* st, const char* msg) const {
  if (st != NULL) {
    ttyLocker ttyl;
    st->print("%7d ", (int) st->time_stamp().milliseconds());
    st->print("%4d ", _aot_id);    // print compilation number
    st->print("    aot[%2d]", _heap->dso_id());
    // Stubs have _method == NULL
    if (_method == NULL) {
      st->print("   %s", _name);
    } else {
      ResourceMark m;
      st->print("   %s", _method->name_and_sig_as_C_string());
    }
    if (Verbose) {
      st->print(" entry at " INTPTR_FORMAT, p2i(_code));
    }
    if (msg != NULL) {
      st->print("   %s", msg);
    }
    st->cr();
  }
}

void AOTCompiledMethod::print_value_on(outputStream* st) const {
  st->print("AOTCompiledMethod ");
  print_on(st, NULL);
}

// Print a short set of xml attributes to identify this aot method.  The
// output should be embedded in some other element.
void AOTCompiledMethod::log_identity(xmlStream* log) const {
  log->print(" aot_id='%d'", _aot_id);
  log->print(" aot='%2d'", _heap->dso_id());
}

void AOTCompiledMethod::log_state_change() const { }

NativeInstruction* PltNativeCallWrapper::get_load_instruction(virtual_call_Relocation* r) const {
  return nativeLoadGot_at(_call->plt_load_got());
}

void PltNativeCallWrapper::set_to_interpreted(const methodHandle& method, CompiledICInfo& info) {
  CompiledPltStaticCall* csc = CompiledPltStaticCall::at(instruction_address());
  csc->set_to_interpreted(method, info.entry());
}

NativeCallWrapper* AOTCompiledMethod::call_wrapper_at(address call) const {
  return new PltNativeCallWrapper((NativePltCall*) call);
}

NativeCallWrapper* AOTCompiledMethod::call_wrapper_before(address return_pc) const {
  return new PltNativeCallWrapper(nativePltCall_before(return_pc));
}

CompiledStaticCall* AOTCompiledMethod::compiledStaticCall_at(Relocation* call_site) const {
  return CompiledPltStaticCall::at(call_site);
}

CompiledStaticCall* AOTCompiledMethod::compiledStaticCall_at(address call_site) const {
  return CompiledPltStaticCall::at(call_site);
}

CompiledStaticCall* AOTCompiledMethod::compiledStaticCall_before(address return_addr) const {
  return CompiledPltStaticCall::before(return_addr);
}

address AOTCompiledMethod::call_instruction_address(address pc) const {
  NativePltCall* pltcall = nativePltCall_before(pc);
  return pltcall->instruction_address();
}

bool AOTCompiledMethod::is_evol_dependent_on(Klass* dependee) {
  return !is_aot_runtime_stub() && _heap->is_dependent_method(dependee, this);
}

void AOTCompiledMethod::clear_inline_caches() {
  if (is_zombie()) {
    return;
  }

  ResourceMark rm;
  RelocIterator iter(this);
  while (iter.next()) {
    iter.reloc()->clear_inline_cache();
    if (iter.type() == relocInfo::opt_virtual_call_type) {
      CompiledIC* cic = CompiledIC_at(&iter);
      nativePltCall_at(iter.addr())->set_stub_to_clean();
    }
  }
}
