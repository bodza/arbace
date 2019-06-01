#include "precompiled.hpp"

#include "asm/codeBuffer.hpp"
#include "memory/resourceArea.hpp"
#include "oops/access.inline.hpp"
#include "oops/oop.inline.hpp"
#include "runtime/interfaceSupport.inline.hpp"
#include "runtime/timerTrace.hpp"
#include "runtime/sharedRuntime.hpp"
#include "runtime/stubRoutines.hpp"
#include "utilities/align.hpp"
#include "utilities/copy.hpp"
#include "utilities/vmError.hpp"

// Implementation of StubRoutines - for a description
// of how to extend it, see the header file.

// Class Variables

BufferBlob* StubRoutines::_code1                                = NULL;
BufferBlob* StubRoutines::_code2                                = NULL;

address StubRoutines::_call_stub_return_address                 = NULL;
address StubRoutines::_call_stub_entry                          = NULL;

address StubRoutines::_catch_exception_entry                    = NULL;
address StubRoutines::_forward_exception_entry                  = NULL;
address StubRoutines::_throw_AbstractMethodError_entry          = NULL;
address StubRoutines::_throw_IncompatibleClassChangeError_entry = NULL;
address StubRoutines::_throw_NullPointerException_at_call_entry = NULL;
address StubRoutines::_throw_StackOverflowError_entry           = NULL;
address StubRoutines::_throw_delayed_StackOverflowError_entry   = NULL;
jint    StubRoutines::_verify_oop_count                         = 0;
address StubRoutines::_verify_oop_subroutine_entry              = NULL;
address StubRoutines::_atomic_xchg_entry                        = NULL;
address StubRoutines::_atomic_xchg_long_entry                   = NULL;
address StubRoutines::_atomic_store_entry                       = NULL;
address StubRoutines::_atomic_cmpxchg_entry                     = NULL;
address StubRoutines::_atomic_cmpxchg_byte_entry                = NULL;
address StubRoutines::_atomic_cmpxchg_long_entry                = NULL;
address StubRoutines::_atomic_add_entry                         = NULL;
address StubRoutines::_atomic_add_long_entry                    = NULL;
address StubRoutines::_fence_entry                              = NULL;
address StubRoutines::_d2i_wrapper                              = NULL;
address StubRoutines::_d2l_wrapper                              = NULL;

jint    StubRoutines::_fpu_cntrl_wrd_std                        = 0;
jint    StubRoutines::_fpu_cntrl_wrd_24                         = 0;
jint    StubRoutines::_fpu_cntrl_wrd_64                         = 0;
jint    StubRoutines::_fpu_cntrl_wrd_trunc                      = 0;
jint    StubRoutines::_mxcsr_std                                = 0;
jint    StubRoutines::_fpu_subnormal_bias1[3]                   = { 0, 0, 0 };
jint    StubRoutines::_fpu_subnormal_bias2[3]                   = { 0, 0, 0 };

// Compiled code entry points default values
// The default functions don't have separate disjoint versions.
address StubRoutines::_jbyte_arraycopy          = CAST_FROM_FN_PTR(address, StubRoutines::jbyte_copy);
address StubRoutines::_jshort_arraycopy         = CAST_FROM_FN_PTR(address, StubRoutines::jshort_copy);
address StubRoutines::_jint_arraycopy           = CAST_FROM_FN_PTR(address, StubRoutines::jint_copy);
address StubRoutines::_jlong_arraycopy          = CAST_FROM_FN_PTR(address, StubRoutines::jlong_copy);
address StubRoutines::_oop_arraycopy            = CAST_FROM_FN_PTR(address, StubRoutines::oop_copy);
address StubRoutines::_oop_arraycopy_uninit     = CAST_FROM_FN_PTR(address, StubRoutines::oop_copy_uninit);
address StubRoutines::_jbyte_disjoint_arraycopy          = CAST_FROM_FN_PTR(address, StubRoutines::jbyte_copy);
address StubRoutines::_jshort_disjoint_arraycopy         = CAST_FROM_FN_PTR(address, StubRoutines::jshort_copy);
address StubRoutines::_jint_disjoint_arraycopy           = CAST_FROM_FN_PTR(address, StubRoutines::jint_copy);
address StubRoutines::_jlong_disjoint_arraycopy          = CAST_FROM_FN_PTR(address, StubRoutines::jlong_copy);
address StubRoutines::_oop_disjoint_arraycopy            = CAST_FROM_FN_PTR(address, StubRoutines::oop_copy);
address StubRoutines::_oop_disjoint_arraycopy_uninit     = CAST_FROM_FN_PTR(address, StubRoutines::oop_copy_uninit);

address StubRoutines::_arrayof_jbyte_arraycopy  = CAST_FROM_FN_PTR(address, StubRoutines::arrayof_jbyte_copy);
address StubRoutines::_arrayof_jshort_arraycopy = CAST_FROM_FN_PTR(address, StubRoutines::arrayof_jshort_copy);
address StubRoutines::_arrayof_jint_arraycopy   = CAST_FROM_FN_PTR(address, StubRoutines::arrayof_jint_copy);
address StubRoutines::_arrayof_jlong_arraycopy  = CAST_FROM_FN_PTR(address, StubRoutines::arrayof_jlong_copy);
address StubRoutines::_arrayof_oop_arraycopy    = CAST_FROM_FN_PTR(address, StubRoutines::arrayof_oop_copy);
address StubRoutines::_arrayof_oop_arraycopy_uninit      = CAST_FROM_FN_PTR(address, StubRoutines::arrayof_oop_copy_uninit);
address StubRoutines::_arrayof_jbyte_disjoint_arraycopy  = CAST_FROM_FN_PTR(address, StubRoutines::arrayof_jbyte_copy);
address StubRoutines::_arrayof_jshort_disjoint_arraycopy = CAST_FROM_FN_PTR(address, StubRoutines::arrayof_jshort_copy);
address StubRoutines::_arrayof_jint_disjoint_arraycopy   = CAST_FROM_FN_PTR(address, StubRoutines::arrayof_jint_copy);
address StubRoutines::_arrayof_jlong_disjoint_arraycopy  = CAST_FROM_FN_PTR(address, StubRoutines::arrayof_jlong_copy);
address StubRoutines::_arrayof_oop_disjoint_arraycopy    = CAST_FROM_FN_PTR(address, StubRoutines::arrayof_oop_copy);
address StubRoutines::_arrayof_oop_disjoint_arraycopy_uninit  = CAST_FROM_FN_PTR(address, StubRoutines::arrayof_oop_copy_uninit);

address StubRoutines::_zero_aligned_words = CAST_FROM_FN_PTR(address, Copy::zero_to_words);

address StubRoutines::_checkcast_arraycopy               = NULL;
address StubRoutines::_checkcast_arraycopy_uninit        = NULL;
address StubRoutines::_unsafe_arraycopy                  = NULL;
address StubRoutines::_generic_arraycopy                 = NULL;

address StubRoutines::_jbyte_fill;
address StubRoutines::_jshort_fill;
address StubRoutines::_jint_fill;
address StubRoutines::_arrayof_jbyte_fill;
address StubRoutines::_arrayof_jshort_fill;
address StubRoutines::_arrayof_jint_fill;

address StubRoutines::_aescrypt_encryptBlock               = NULL;
address StubRoutines::_aescrypt_decryptBlock               = NULL;
address StubRoutines::_cipherBlockChaining_encryptAESCrypt = NULL;
address StubRoutines::_cipherBlockChaining_decryptAESCrypt = NULL;
address StubRoutines::_counterMode_AESCrypt                = NULL;
address StubRoutines::_ghash_processBlocks                 = NULL;
address StubRoutines::_base64_encodeBlock                  = NULL;

address StubRoutines::_sha1_implCompress     = NULL;
address StubRoutines::_sha1_implCompressMB   = NULL;
address StubRoutines::_sha256_implCompress   = NULL;
address StubRoutines::_sha256_implCompressMB = NULL;
address StubRoutines::_sha512_implCompress   = NULL;
address StubRoutines::_sha512_implCompressMB = NULL;

address StubRoutines::_updateBytesCRC32 = NULL;
address StubRoutines::_crc_table_adr =    NULL;

address StubRoutines::_crc32c_table_addr = NULL;
address StubRoutines::_updateBytesCRC32C = NULL;
address StubRoutines::_updateBytesAdler32 = NULL;

address StubRoutines::_multiplyToLen = NULL;
address StubRoutines::_squareToLen = NULL;
address StubRoutines::_mulAdd = NULL;
address StubRoutines::_montgomeryMultiply = NULL;
address StubRoutines::_montgomerySquare = NULL;

address StubRoutines::_vectorizedMismatch = NULL;

address StubRoutines::_dexp = NULL;
address StubRoutines::_dlog = NULL;
address StubRoutines::_dlog10 = NULL;
address StubRoutines::_dpow = NULL;
address StubRoutines::_dsin = NULL;
address StubRoutines::_dcos = NULL;
address StubRoutines::_dlibm_sin_cos_huge = NULL;
address StubRoutines::_dlibm_reduce_pi04l = NULL;
address StubRoutines::_dlibm_tan_cot_huge = NULL;
address StubRoutines::_dtan = NULL;

double (* StubRoutines::_intrinsic_log10 )(double) = NULL;
double (* StubRoutines::_intrinsic_sin   )(double) = NULL;
double (* StubRoutines::_intrinsic_cos   )(double) = NULL;
double (* StubRoutines::_intrinsic_tan   )(double) = NULL;

address StubRoutines::_safefetch32_entry                 = NULL;
address StubRoutines::_safefetch32_fault_pc              = NULL;
address StubRoutines::_safefetch32_continuation_pc       = NULL;
address StubRoutines::_safefetchN_entry                  = NULL;
address StubRoutines::_safefetchN_fault_pc               = NULL;
address StubRoutines::_safefetchN_continuation_pc        = NULL;

// Initialization
//
// Note: to break cycle with universe initialization, stubs are generated in two phases.
// The first one generates stubs needed during universe init (e.g., _handle_must_compile_first_entry).
// The second phase includes all other stubs (which may depend on universe being initialized.)

extern void StubGenerator_generate(CodeBuffer* code, bool all); // only interface to generators

void StubRoutines::initialize1() {
  if (_code1 == NULL) {
    ResourceMark rm;
    _code1 = BufferBlob::create("StubRoutines (1)", code_size1);
    if (_code1 == NULL) {
      vm_exit_out_of_memory(code_size1, OOM_MALLOC_ERROR, "CodeCache: no room for StubRoutines (1)");
    }
    CodeBuffer buffer(_code1);
    StubGenerator_generate(&buffer, false);
    // When new stubs added we need to make sure there is some space left
    // to catch situation when we should increase size again.
  }
}

void StubRoutines::initialize2() {
  if (_code2 == NULL) {
    ResourceMark rm;
    _code2 = BufferBlob::create("StubRoutines (2)", code_size2);
    if (_code2 == NULL) {
      vm_exit_out_of_memory(code_size2, OOM_MALLOC_ERROR, "CodeCache: no room for StubRoutines (2)");
    }
    CodeBuffer buffer(_code2);
    StubGenerator_generate(&buffer, true);
    // When new stubs added we need to make sure there is some space left
    // to catch situation when we should increase size again.
  }
}

void stubRoutines_init1() { StubRoutines::initialize1(); }
void stubRoutines_init2() { StubRoutines::initialize2(); }

//
// Default versions of arraycopy functions
//

JRT_LEAF(void, StubRoutines::jbyte_copy(jbyte* src, jbyte* dest, size_t count))
  Copy::conjoint_jbytes_atomic(src, dest, count);
JRT_END

JRT_LEAF(void, StubRoutines::jshort_copy(jshort* src, jshort* dest, size_t count))
  Copy::conjoint_jshorts_atomic(src, dest, count);
JRT_END

JRT_LEAF(void, StubRoutines::jint_copy(jint* src, jint* dest, size_t count))
  Copy::conjoint_jints_atomic(src, dest, count);
JRT_END

JRT_LEAF(void, StubRoutines::jlong_copy(jlong* src, jlong* dest, size_t count))
  Copy::conjoint_jlongs_atomic(src, dest, count);
JRT_END

JRT_LEAF(void, StubRoutines::oop_copy(oop* src, oop* dest, size_t count))
  ArrayAccess<>::oop_arraycopy_raw((HeapWord*)src, (HeapWord*)dest, count);
JRT_END

JRT_LEAF(void, StubRoutines::oop_copy_uninit(oop* src, oop* dest, size_t count))
  ArrayAccess<IS_DEST_UNINITIALIZED>::oop_arraycopy_raw((HeapWord*)src, (HeapWord*)dest, count);
JRT_END

JRT_LEAF(void, StubRoutines::arrayof_jbyte_copy(HeapWord* src, HeapWord* dest, size_t count))
  Copy::arrayof_conjoint_jbytes(src, dest, count);
JRT_END

JRT_LEAF(void, StubRoutines::arrayof_jshort_copy(HeapWord* src, HeapWord* dest, size_t count))
  Copy::arrayof_conjoint_jshorts(src, dest, count);
JRT_END

JRT_LEAF(void, StubRoutines::arrayof_jint_copy(HeapWord* src, HeapWord* dest, size_t count))
  Copy::arrayof_conjoint_jints(src, dest, count);
JRT_END

JRT_LEAF(void, StubRoutines::arrayof_jlong_copy(HeapWord* src, HeapWord* dest, size_t count))
  Copy::arrayof_conjoint_jlongs(src, dest, count);
JRT_END

JRT_LEAF(void, StubRoutines::arrayof_oop_copy(HeapWord* src, HeapWord* dest, size_t count))
  ArrayAccess<ARRAYCOPY_ARRAYOF>::oop_arraycopy_raw(src, dest, count);
JRT_END

JRT_LEAF(void, StubRoutines::arrayof_oop_copy_uninit(HeapWord* src, HeapWord* dest, size_t count))
  ArrayAccess<ARRAYCOPY_ARRAYOF | IS_DEST_UNINITIALIZED>::oop_arraycopy_raw(src, dest, count);
JRT_END

address StubRoutines::select_fill_function(BasicType t, bool aligned, const char* &name) {
#define RETURN_STUB(xxx_fill) { \
  name = #xxx_fill; \
  return StubRoutines::xxx_fill(); }

  switch (t) {
  case T_BYTE:
  case T_BOOLEAN:
    if (!aligned) RETURN_STUB(jbyte_fill);
    RETURN_STUB(arrayof_jbyte_fill);
  case T_CHAR:
  case T_SHORT:
    if (!aligned) RETURN_STUB(jshort_fill);
    RETURN_STUB(arrayof_jshort_fill);
  case T_INT:
  case T_FLOAT:
    if (!aligned) RETURN_STUB(jint_fill);
    RETURN_STUB(arrayof_jint_fill);
  case T_DOUBLE:
  case T_LONG:
  case T_ARRAY:
  case T_OBJECT:
  case T_NARROWOOP:
  case T_NARROWKLASS:
  case T_ADDRESS:
    // Currently unsupported
    return NULL;

  default:
    ShouldNotReachHere();
    return NULL;
  }

#undef RETURN_STUB
}

// constants for computing the copy function
enum {
  COPYFUNC_UNALIGNED = 0,
  COPYFUNC_ALIGNED = 1,                 // src, dest aligned to HeapWordSize
  COPYFUNC_CONJOINT = 0,
  COPYFUNC_DISJOINT = 2                 // src != dest, or transfer can descend
};

// Note:  The condition "disjoint" applies also for overlapping copies
// where an descending copy is permitted (i.e., dest_offset <= src_offset).
address
StubRoutines::select_arraycopy_function(BasicType t, bool aligned, bool disjoint, const char* &name, bool dest_uninitialized) {
  int selector = (aligned  ? COPYFUNC_ALIGNED  : COPYFUNC_UNALIGNED) + (disjoint ? COPYFUNC_DISJOINT : COPYFUNC_CONJOINT);

#define RETURN_STUB(xxx_arraycopy) { \
  name = #xxx_arraycopy; \
  return StubRoutines::xxx_arraycopy(); }

#define RETURN_STUB_PARM(xxx_arraycopy, parm) { \
  name = #xxx_arraycopy; \
  return StubRoutines::xxx_arraycopy(parm); }

  switch (t) {
  case T_BYTE:
  case T_BOOLEAN:
    switch (selector) {
    case COPYFUNC_CONJOINT | COPYFUNC_UNALIGNED:  RETURN_STUB(jbyte_arraycopy);
    case COPYFUNC_CONJOINT | COPYFUNC_ALIGNED:    RETURN_STUB(arrayof_jbyte_arraycopy);
    case COPYFUNC_DISJOINT | COPYFUNC_UNALIGNED:  RETURN_STUB(jbyte_disjoint_arraycopy);
    case COPYFUNC_DISJOINT | COPYFUNC_ALIGNED:    RETURN_STUB(arrayof_jbyte_disjoint_arraycopy);
    }
  case T_CHAR:
  case T_SHORT:
    switch (selector) {
    case COPYFUNC_CONJOINT | COPYFUNC_UNALIGNED:  RETURN_STUB(jshort_arraycopy);
    case COPYFUNC_CONJOINT | COPYFUNC_ALIGNED:    RETURN_STUB(arrayof_jshort_arraycopy);
    case COPYFUNC_DISJOINT | COPYFUNC_UNALIGNED:  RETURN_STUB(jshort_disjoint_arraycopy);
    case COPYFUNC_DISJOINT | COPYFUNC_ALIGNED:    RETURN_STUB(arrayof_jshort_disjoint_arraycopy);
    }
  case T_INT:
  case T_FLOAT:
    switch (selector) {
    case COPYFUNC_CONJOINT | COPYFUNC_UNALIGNED:  RETURN_STUB(jint_arraycopy);
    case COPYFUNC_CONJOINT | COPYFUNC_ALIGNED:    RETURN_STUB(arrayof_jint_arraycopy);
    case COPYFUNC_DISJOINT | COPYFUNC_UNALIGNED:  RETURN_STUB(jint_disjoint_arraycopy);
    case COPYFUNC_DISJOINT | COPYFUNC_ALIGNED:    RETURN_STUB(arrayof_jint_disjoint_arraycopy);
    }
  case T_DOUBLE:
  case T_LONG:
    switch (selector) {
    case COPYFUNC_CONJOINT | COPYFUNC_UNALIGNED:  RETURN_STUB(jlong_arraycopy);
    case COPYFUNC_CONJOINT | COPYFUNC_ALIGNED:    RETURN_STUB(arrayof_jlong_arraycopy);
    case COPYFUNC_DISJOINT | COPYFUNC_UNALIGNED:  RETURN_STUB(jlong_disjoint_arraycopy);
    case COPYFUNC_DISJOINT | COPYFUNC_ALIGNED:    RETURN_STUB(arrayof_jlong_disjoint_arraycopy);
    }
  case T_ARRAY:
  case T_OBJECT:
    switch (selector) {
    case COPYFUNC_CONJOINT | COPYFUNC_UNALIGNED:  RETURN_STUB_PARM(oop_arraycopy, dest_uninitialized);
    case COPYFUNC_CONJOINT | COPYFUNC_ALIGNED:    RETURN_STUB_PARM(arrayof_oop_arraycopy, dest_uninitialized);
    case COPYFUNC_DISJOINT | COPYFUNC_UNALIGNED:  RETURN_STUB_PARM(oop_disjoint_arraycopy, dest_uninitialized);
    case COPYFUNC_DISJOINT | COPYFUNC_ALIGNED:    RETURN_STUB_PARM(arrayof_oop_disjoint_arraycopy, dest_uninitialized);
    }
  default:
    ShouldNotReachHere();
    return NULL;
  }

#undef RETURN_STUB
#undef RETURN_STUB_PARM
}
