#ifndef SHARE_VM_AOT_COMPILEDIC_AOT_HPP
#define SHARE_VM_AOT_COMPILEDIC_AOT_HPP

#include "code/compiledIC.hpp"
#include "code/nativeInst.hpp"
#include "interpreter/linkResolver.hpp"
#include "oops/compiledICHolder.hpp"

class CompiledPltStaticCall: public CompiledStaticCall {
  friend class CompiledIC;
  friend class PltNativeCallWrapper;

  // Also used by CompiledIC
  void set_to_interpreted(const methodHandle& callee, address entry);

  address instruction_address() const { return _call->instruction_address(); }
  void set_destination_mt_safe(address dest) { _call->set_destination_mt_safe(dest); }

  NativePltCall* _call;

  CompiledPltStaticCall(NativePltCall* call) : _call(call) { }

 public:

  inline static CompiledPltStaticCall* before(address return_addr) {
    CompiledPltStaticCall* st = new CompiledPltStaticCall(nativePltCall_before(return_addr));
    st->verify();
    return st;
  }

  static inline CompiledPltStaticCall* at(address native_call) {
    CompiledPltStaticCall* st = new CompiledPltStaticCall(nativePltCall_at(native_call));
    st->verify();
    return st;
  }

  static inline CompiledPltStaticCall* at(Relocation* call_site) {
    return at(call_site->addr());
  }

  // Delegation
  address destination() const { return _call->destination(); }

  virtual bool is_call_to_interpreted() const;

  // Stub support
  address find_stub();
  static void set_stub_to_clean(static_stub_Relocation* static_stub);

  // Misc.
  void print()  { };
  void verify() { };

 protected:
  virtual address resolve_call_stub() const { return _call->plt_resolve_call(); }
  virtual void set_to_far(const methodHandle& callee, address entry) { set_to_compiled(entry); }
  virtual const char* name() const { return "CompiledPltStaticCall"; }
};

#endif
