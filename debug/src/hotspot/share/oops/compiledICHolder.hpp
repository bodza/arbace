#ifndef SHARE_VM_OOPS_COMPILEDICHOLDEROOP_HPP
#define SHARE_VM_OOPS_COMPILEDICHOLDEROOP_HPP

#include "oops/oop.hpp"
#include "utilities/macros.hpp"
#include "oops/klass.hpp"
#include "oops/method.hpp"

// A CompiledICHolder* is a helper object for the inline cache implementation.
// It holds:
//   (1) (method+klass pair) when converting from compiled to an interpreted call
//   (2) (klass+klass pair) when calling itable stub from megamorphic compiled call
//
// These are always allocated in the C heap and are freed during a
// safepoint by the ICBuffer logic.  It's unsafe to free them earlier
// since they might be in use.
//

class CompiledICHolder : public CHeapObj<mtCompiler> {
  friend class VMStructs;
 private:
  static volatile int _live_count; // allocated
  static volatile int _live_not_claimed_count; // allocated but not yet in use so not
                                               // reachable by iterating over nmethods

  Metadata* _holder_metadata;
  Klass*    _holder_klass;    // to avoid name conflict with oopDesc::_klass
  CompiledICHolder* _next;
  bool _is_metadata_method;

 public:
  // Constructor
  CompiledICHolder(Metadata* metadata, Klass* klass, bool is_method = true);
  ~CompiledICHolder() { };

  static int live_count() { return _live_count; }
  static int live_not_claimed_count() { return _live_not_claimed_count; }

  // accessors
  Klass*    holder_klass()  const     { return _holder_klass; }
  Metadata* holder_metadata() const   { return _holder_metadata; }

  void set_holder_metadata(Metadata* m) { _holder_metadata = m; }
  void set_holder_klass(Klass* k)     { _holder_klass = k; }

  static int holder_metadata_offset() { return offset_of(CompiledICHolder, _holder_metadata); }
  static int holder_klass_offset()    { return offset_of(CompiledICHolder, _holder_klass); }

  CompiledICHolder* next()     { return _next; }
  void set_next(CompiledICHolder* n) { _next = n; }

  inline bool is_loader_alive() {
    Klass* k = _is_metadata_method ? ((Method*)_holder_metadata)->method_holder() : (Klass*)_holder_metadata;
    if (!k->is_loader_alive()) {
      return false;
    }
    if (!_holder_klass->is_loader_alive()) {
      return false;
    }
    return true;
  }

  // Verify
  void verify_on(outputStream* st);

  // Printing
  void print_on(outputStream* st) const;
  void print_value_on(outputStream* st) const;

  const char* internal_name() const { return "{compiledICHolder}"; }

  void claim() { };
};

#endif
