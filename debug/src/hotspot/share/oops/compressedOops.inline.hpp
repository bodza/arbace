#ifndef SHARE_OOPS_COMPRESSEDOOPS_INLINE_HPP
#define SHARE_OOPS_COMPRESSEDOOPS_INLINE_HPP

#include "gc/shared/collectedHeap.hpp"
#include "memory/universe.hpp"
#include "oops/oop.hpp"

// Functions for encoding and decoding compressed oops.
// If the oops are compressed, the type passed to these overloaded functions
// is narrowOop.  All functions are overloaded so they can be called by
// template functions without conditionals (the compiler instantiates via
// the right type and inlines the appropriate code).

// Algorithm for encoding and decoding oops from 64 bit pointers to 32 bit
// offset from the heap base.  Saving the check for null can save instructions
// in inner GC loops so these are separated.

namespace CompressedOops {
  inline bool is_null(oop obj)       { return obj == NULL; }
  inline bool is_null(narrowOop obj) { return obj == 0; }

  inline oop decode_not_null(narrowOop v) {
    assert(!is_null(v), "narrow oop value can never be zero");
    address base = Universe::narrow_oop_base();
    int    shift = Universe::narrow_oop_shift();
    oop result = (oop)(void*)((uintptr_t)base + ((uintptr_t)v << shift));
    assert(check_obj_alignment(result), "address not aligned: " INTPTR_FORMAT, p2i((void*) result));
    return result;
  }

  inline oop decode(narrowOop v) {
    return is_null(v) ? (oop)NULL : decode_not_null(v);
  }

  inline narrowOop encode_not_null(oop v) {
    assert(!is_null(v), "oop value can never be zero");
    assert(check_obj_alignment(v), "Address not aligned");
    assert(Universe::heap()->is_in_reserved(v), "Address not in heap");
    address base = Universe::narrow_oop_base();
    int    shift = Universe::narrow_oop_shift();
    uint64_t  pd = (uint64_t)(pointer_delta((void*)v, (void*)base, 1));
    assert(OopEncodingHeapMax > pd, "change encoding max if new encoding");
    uint64_t result = pd >> shift;
    assert((result & CONST64(0xffffffff00000000)) == 0, "narrow oop overflow");
    assert(decode(result) == v, "reversibility");
    return (narrowOop)result;
  }

  inline narrowOop encode(oop v) {
    return is_null(v) ? (narrowOop)0 : encode_not_null(v);
  }

  // No conversions needed for these overloads
  inline oop decode_not_null(oop v)             { return v; }
  inline oop decode(oop v)                      { return v; }
  inline narrowOop encode_not_null(narrowOop v) { return v; }
  inline narrowOop encode(narrowOop v)          { return v; }
}

#endif
