#include "precompiled.hpp"

#include "jvm.h"
#include "classfile/vmSymbols.hpp"
#include "compiler/compilerDirectives.hpp"
#include "memory/allocation.inline.hpp"
#include "memory/oopFactory.hpp"
#include "memory/metaspaceClosure.hpp"
#include "oops/oop.inline.hpp"
#include "runtime/handles.inline.hpp"
#include "utilities/xmlstream.hpp"

Symbol* vmSymbols::_symbols[vmSymbols::SID_LIMIT];

Symbol* vmSymbols::_type_signatures[T_VOID+1] = { NULL /*, NULL...*/ };

inline int compare_symbol(const Symbol* a, const Symbol* b) {
  if (a == b)  return 0;
  // follow the natural address order:
  return (address)a > (address)b ? +1 : -1;
}

static vmSymbols::SID vm_symbol_index[vmSymbols::SID_LIMIT];
extern "C" {
  static int compare_vmsymbol_sid(const void* void_a, const void* void_b) {
    const Symbol* a = vmSymbols::symbol_at(*((vmSymbols::SID*) void_a));
    const Symbol* b = vmSymbols::symbol_at(*((vmSymbols::SID*) void_b));
    return compare_symbol(a, b);
  }
}

// Put all the VM symbol strings in one place.
// Makes for a more compact libjvm.
#define VM_SYMBOL_BODY(name, string) string "\0"
static const char* vm_symbol_bodies = VM_SYMBOLS_DO(VM_SYMBOL_BODY, VM_ALIAS_IGNORE);

void vmSymbols::initialize(TRAPS) {
  {
    const char* string = &vm_symbol_bodies[0];
    for (int index = (int)FIRST_SID; index < (int)SID_LIMIT; index++) {
      Symbol* sym = SymbolTable::new_permanent_symbol(string, CHECK);
      _symbols[index] = sym;
      string += strlen(string); // skip string body
      string += 1;              // skip trailing null
    }

    _type_signatures[T_BYTE]    = byte_signature();
    _type_signatures[T_CHAR]    = char_signature();
    _type_signatures[T_DOUBLE]  = double_signature();
    _type_signatures[T_FLOAT]   = float_signature();
    _type_signatures[T_INT]     = int_signature();
    _type_signatures[T_LONG]    = long_signature();
    _type_signatures[T_SHORT]   = short_signature();
    _type_signatures[T_BOOLEAN] = bool_signature();
    _type_signatures[T_VOID]    = void_signature();
    // no single signatures for T_OBJECT or T_ARRAY
  }

  // Create an index for find_id:
  {
    for (int index = (int)FIRST_SID; index < (int)SID_LIMIT; index++) {
      vm_symbol_index[index] = (SID)index;
    }
    int num_sids = SID_LIMIT-FIRST_SID;
    qsort(&vm_symbol_index[FIRST_SID], num_sids, sizeof(vm_symbol_index[0]),
          compare_vmsymbol_sid);
  }
}

void vmSymbols::symbols_do(SymbolClosure* f) {
  for (int index = (int)FIRST_SID; index < (int)SID_LIMIT; index++) {
    f->do_symbol(&_symbols[index]);
  }
  for (int i = 0; i < T_VOID+1; i++) {
    f->do_symbol(&_type_signatures[i]);
  }
}

void vmSymbols::metaspace_pointers_do(MetaspaceClosure *it) {
  for (int index = (int)FIRST_SID; index < (int)SID_LIMIT; index++) {
    it->push(&_symbols[index]);
  }
  for (int i = 0; i < T_VOID+1; i++) {
    it->push(&_type_signatures[i]);
  }
}

void vmSymbols::serialize(SerializeClosure* soc) {
  soc->do_region((u_char*)&_symbols[FIRST_SID],
                 (SID_LIMIT - FIRST_SID) * sizeof(_symbols[0]));
  soc->do_region((u_char*)_type_signatures, sizeof(_type_signatures));
}

BasicType vmSymbols::signature_type(const Symbol* s) {
  if (s->utf8_length() == 1) {
    BasicType result = char2type(s->byte_at(0));
    if (is_java_primitive(result) || result == T_VOID) {
      return result;
    }
  }
  return T_OBJECT;
}

static int mid_hint = (int)vmSymbols::FIRST_SID+1;

vmSymbols::SID vmSymbols::find_sid(const Symbol* symbol) {
  // Handle the majority of misses by a bounds check.
  // Then, use a binary search over the index.
  // Expected trip count is less than log2_SID_LIMIT, about eight.
  // This is slow but acceptable, given that calls are not
  // dynamically common.  (Method*::intrinsic_id has a cache.)
  int min = (int)FIRST_SID, max = (int)SID_LIMIT - 1;
  SID sid = NO_SID, sid1;
  int cmp1;
  sid1 = vm_symbol_index[min];
  cmp1 = compare_symbol(symbol, symbol_at(sid1));
  if (cmp1 <= 0) {              // before the first
    if (cmp1 == 0)  sid = sid1;
  } else {
    sid1 = vm_symbol_index[max];
    cmp1 = compare_symbol(symbol, symbol_at(sid1));
    if (cmp1 >= 0) {            // after the last
      if (cmp1 == 0)  sid = sid1;
    } else {
      // After checking the extremes, do a binary search.
      ++min; --max;             // endpoints are done
      int mid = mid_hint;       // start at previous success
      while (max >= min) {
        sid1 = vm_symbol_index[mid];
        cmp1 = compare_symbol(symbol, symbol_at(sid1));
        if (cmp1 == 0) {
          mid_hint = mid;
          sid = sid1;
          break;
        }
        if (cmp1 < 0)
          max = mid - 1;        // symbol < symbol_at(sid)
        else
          min = mid + 1;

        // Pick a new probe point:
        mid = (max + min) / 2;
      }
    }
  }

  return sid;
}

vmSymbols::SID vmSymbols::find_sid(const char* symbol_name) {
  Symbol* symbol = SymbolTable::probe(symbol_name, (int) strlen(symbol_name));
  if (symbol == NULL)  return NO_SID;
  return find_sid(symbol);
}

static vmIntrinsics::ID wrapper_intrinsic(BasicType type, bool unboxing) {
#define TYPE2(type, unboxing) ((int)(type)*2 + ((unboxing) ? 1 : 0))
  switch (TYPE2(type, unboxing)) {
#define BASIC_TYPE_CASE(type, box, unbox) \
    case TYPE2(type, false):  return vmIntrinsics::box; \
    case TYPE2(type, true):   return vmIntrinsics::unbox
    BASIC_TYPE_CASE(T_BOOLEAN, _Boolean_valueOf,   _booleanValue);
    BASIC_TYPE_CASE(T_BYTE,    _Byte_valueOf,      _byteValue);
    BASIC_TYPE_CASE(T_CHAR,    _Character_valueOf, _charValue);
    BASIC_TYPE_CASE(T_SHORT,   _Short_valueOf,     _shortValue);
    BASIC_TYPE_CASE(T_INT,     _Integer_valueOf,   _intValue);
    BASIC_TYPE_CASE(T_LONG,    _Long_valueOf,      _longValue);
    BASIC_TYPE_CASE(T_FLOAT,   _Float_valueOf,     _floatValue);
    BASIC_TYPE_CASE(T_DOUBLE,  _Double_valueOf,    _doubleValue);
#undef BASIC_TYPE_CASE
  }
#undef TYPE2
  return vmIntrinsics::_none;
}

vmIntrinsics::ID vmIntrinsics::for_boxing(BasicType type) {
  return wrapper_intrinsic(type, false);
}
vmIntrinsics::ID vmIntrinsics::for_unboxing(BasicType type) {
  return wrapper_intrinsic(type, true);
}

vmIntrinsics::ID vmIntrinsics::for_raw_conversion(BasicType src, BasicType dest) {
#define SRC_DEST(s,d) (((int)(s) << 4) + (int)(d))
  switch (SRC_DEST(src, dest)) {
  case SRC_DEST(T_INT, T_FLOAT):   return vmIntrinsics::_intBitsToFloat;
  case SRC_DEST(T_FLOAT, T_INT):   return vmIntrinsics::_floatToRawIntBits;

  case SRC_DEST(T_LONG, T_DOUBLE): return vmIntrinsics::_longBitsToDouble;
  case SRC_DEST(T_DOUBLE, T_LONG): return vmIntrinsics::_doubleToRawLongBits;
  }
#undef SRC_DEST

  return vmIntrinsics::_none;
}

bool vmIntrinsics::preserves_state(vmIntrinsics::ID id) {
  switch(id) {
  case vmIntrinsics::_currentTimeMillis:
  case vmIntrinsics::_nanoTime:
  case vmIntrinsics::_floatToRawIntBits:
  case vmIntrinsics::_intBitsToFloat:
  case vmIntrinsics::_doubleToRawLongBits:
  case vmIntrinsics::_longBitsToDouble:
  case vmIntrinsics::_getClass:
  case vmIntrinsics::_isInstance:
  case vmIntrinsics::_currentThread:
  case vmIntrinsics::_dabs:
  case vmIntrinsics::_dsqrt:
  case vmIntrinsics::_dsin:
  case vmIntrinsics::_dcos:
  case vmIntrinsics::_dtan:
  case vmIntrinsics::_dlog:
  case vmIntrinsics::_dlog10:
  case vmIntrinsics::_dexp:
  case vmIntrinsics::_dpow:
  case vmIntrinsics::_checkIndex:
  case vmIntrinsics::_Reference_get:
  case vmIntrinsics::_updateCRC32:
  case vmIntrinsics::_updateBytesCRC32:
  case vmIntrinsics::_updateByteBufferCRC32:
  case vmIntrinsics::_vectorizedMismatch:
  case vmIntrinsics::_fmaD:
  case vmIntrinsics::_fmaF:
  case vmIntrinsics::_isDigit:
  case vmIntrinsics::_isLowerCase:
  case vmIntrinsics::_isUpperCase:
  case vmIntrinsics::_isWhitespace:
    return true;
  default:
    return false;
  }
}

bool vmIntrinsics::can_trap(vmIntrinsics::ID id) {
  switch(id) {
  case vmIntrinsics::_currentTimeMillis:
  case vmIntrinsics::_nanoTime:
  case vmIntrinsics::_floatToRawIntBits:
  case vmIntrinsics::_intBitsToFloat:
  case vmIntrinsics::_doubleToRawLongBits:
  case vmIntrinsics::_longBitsToDouble:
  case vmIntrinsics::_currentThread:
  case vmIntrinsics::_dabs:
  case vmIntrinsics::_dsqrt:
  case vmIntrinsics::_dsin:
  case vmIntrinsics::_dcos:
  case vmIntrinsics::_dtan:
  case vmIntrinsics::_dlog:
  case vmIntrinsics::_dlog10:
  case vmIntrinsics::_dexp:
  case vmIntrinsics::_dpow:
  case vmIntrinsics::_updateCRC32:
  case vmIntrinsics::_updateBytesCRC32:
  case vmIntrinsics::_updateByteBufferCRC32:
  case vmIntrinsics::_vectorizedMismatch:
  case vmIntrinsics::_fmaD:
  case vmIntrinsics::_fmaF:
    return false;
  default:
    return true;
  }
}

// Some intrinsics produce different results if they are not pinned
bool vmIntrinsics::should_be_pinned(vmIntrinsics::ID id) {
  switch(id) {
  case vmIntrinsics::_currentTimeMillis:
  case vmIntrinsics::_nanoTime:
    return true;
  default:
    return false;
  }
}

bool vmIntrinsics::does_virtual_dispatch(vmIntrinsics::ID id) {
  switch(id) {
  case vmIntrinsics::_hashCode:
  case vmIntrinsics::_clone:
    return true;
    break;
  default:
    return false;
  }
}

int vmIntrinsics::predicates_needed(vmIntrinsics::ID id) {
  switch (id) {
  case vmIntrinsics::_cipherBlockChaining_encryptAESCrypt:
  case vmIntrinsics::_cipherBlockChaining_decryptAESCrypt:
  case vmIntrinsics::_counterMode_AESCrypt:
    return 1;
  case vmIntrinsics::_digestBase_implCompressMB:
    return 3;
  default:
    return 0;
  }
}

bool vmIntrinsics::is_intrinsic_available(vmIntrinsics::ID id) {
  return !vmIntrinsics::is_intrinsic_disabled(id) && !vmIntrinsics::is_disabled_by_flags(id);
}

bool vmIntrinsics::is_intrinsic_disabled(vmIntrinsics::ID id) {
  // Canonicalize DisableIntrinsic to contain only ',' as a separator.
  // Note, DirectiveSet may not be created at this point yet since this code
  // is called from initial stub geenration code.
  char* local_list = (char*)DirectiveSet::canonicalize_disableintrinsic(DisableIntrinsic);

  bool found = false;
  char* token = strtok(local_list, ",");
  while (token != NULL) {
    if (strcmp(token, vmIntrinsics::name_at(id)) == 0) {
      found = true;
      break;
    } else {
      token = strtok(NULL, ",");
    }
  }

  FREE_C_HEAP_ARRAY(char, local_list);
  return found;
}

bool vmIntrinsics::is_disabled_by_flags(const methodHandle& method) {
  vmIntrinsics::ID id = method->intrinsic_id();
  return is_disabled_by_flags(id);
}

bool vmIntrinsics::is_disabled_by_flags(vmIntrinsics::ID id) {
  // -XX:-InlineNatives disables nearly all intrinsics except the ones listed in
  // the following switch statement.
  if (!InlineNatives) {
    switch (id) {
    case vmIntrinsics::_indexOfL:
    case vmIntrinsics::_indexOfU:
    case vmIntrinsics::_indexOfUL:
    case vmIntrinsics::_indexOfIL:
    case vmIntrinsics::_indexOfIU:
    case vmIntrinsics::_indexOfIUL:
    case vmIntrinsics::_indexOfU_char:
    case vmIntrinsics::_compareToL:
    case vmIntrinsics::_compareToU:
    case vmIntrinsics::_compareToLU:
    case vmIntrinsics::_compareToUL:
    case vmIntrinsics::_equalsL:
    case vmIntrinsics::_equalsU:
    case vmIntrinsics::_equalsC:
    case vmIntrinsics::_getCharStringU:
    case vmIntrinsics::_putCharStringU:
    case vmIntrinsics::_compressStringC:
    case vmIntrinsics::_compressStringB:
    case vmIntrinsics::_inflateStringC:
    case vmIntrinsics::_inflateStringB:
    case vmIntrinsics::_getAndAddInt:
    case vmIntrinsics::_getAndAddLong:
    case vmIntrinsics::_getAndSetInt:
    case vmIntrinsics::_getAndSetLong:
    case vmIntrinsics::_getAndSetObject:
    case vmIntrinsics::_loadFence:
    case vmIntrinsics::_storeFence:
    case vmIntrinsics::_fullFence:
    case vmIntrinsics::_hasNegatives:
    case vmIntrinsics::_Reference_get:
      break;
    default:
      return true;
    }
  }

  switch (id) {
  case vmIntrinsics::_isInstance:
  case vmIntrinsics::_isAssignableFrom:
  case vmIntrinsics::_getModifiers:
  case vmIntrinsics::_isInterface:
  case vmIntrinsics::_isArray:
  case vmIntrinsics::_isPrimitive:
  case vmIntrinsics::_getSuperclass:
  case vmIntrinsics::_Class_cast:
  case vmIntrinsics::_getLength:
  case vmIntrinsics::_newArray:
  case vmIntrinsics::_getClass:
    if (!InlineClassNatives) return true;
    break;
  case vmIntrinsics::_currentThread:
  case vmIntrinsics::_isInterrupted:
    if (!InlineThreadNatives) return true;
    break;
  case vmIntrinsics::_floatToRawIntBits:
  case vmIntrinsics::_intBitsToFloat:
  case vmIntrinsics::_doubleToRawLongBits:
  case vmIntrinsics::_longBitsToDouble:
  case vmIntrinsics::_dabs:
  case vmIntrinsics::_dsqrt:
  case vmIntrinsics::_dsin:
  case vmIntrinsics::_dcos:
  case vmIntrinsics::_dtan:
  case vmIntrinsics::_dlog:
  case vmIntrinsics::_dexp:
  case vmIntrinsics::_dpow:
  case vmIntrinsics::_dlog10:
  case vmIntrinsics::_datan2:
  case vmIntrinsics::_min:
  case vmIntrinsics::_max:
  case vmIntrinsics::_floatToIntBits:
  case vmIntrinsics::_doubleToLongBits:
    if (!InlineMathNatives) return true;
    break;
  case vmIntrinsics::_fmaD:
  case vmIntrinsics::_fmaF:
    if (!InlineMathNatives || !UseFMA) return true;
    break;
  case vmIntrinsics::_arraycopy:
    if (!InlineArrayCopy) return true;
    break;
  case vmIntrinsics::_updateCRC32:
  case vmIntrinsics::_updateBytesCRC32:
  case vmIntrinsics::_updateByteBufferCRC32:
    if (!UseCRC32Intrinsics) return true;
    break;
  case vmIntrinsics::_getObject:
  case vmIntrinsics::_getBoolean:
  case vmIntrinsics::_getByte:
  case vmIntrinsics::_getShort:
  case vmIntrinsics::_getChar:
  case vmIntrinsics::_getInt:
  case vmIntrinsics::_getLong:
  case vmIntrinsics::_getFloat:
  case vmIntrinsics::_getDouble:
  case vmIntrinsics::_putObject:
  case vmIntrinsics::_putBoolean:
  case vmIntrinsics::_putByte:
  case vmIntrinsics::_putShort:
  case vmIntrinsics::_putChar:
  case vmIntrinsics::_putInt:
  case vmIntrinsics::_putLong:
  case vmIntrinsics::_putFloat:
  case vmIntrinsics::_putDouble:
  case vmIntrinsics::_getObjectVolatile:
  case vmIntrinsics::_getBooleanVolatile:
  case vmIntrinsics::_getByteVolatile:
  case vmIntrinsics::_getShortVolatile:
  case vmIntrinsics::_getCharVolatile:
  case vmIntrinsics::_getIntVolatile:
  case vmIntrinsics::_getLongVolatile:
  case vmIntrinsics::_getFloatVolatile:
  case vmIntrinsics::_getDoubleVolatile:
  case vmIntrinsics::_putObjectVolatile:
  case vmIntrinsics::_putBooleanVolatile:
  case vmIntrinsics::_putByteVolatile:
  case vmIntrinsics::_putShortVolatile:
  case vmIntrinsics::_putCharVolatile:
  case vmIntrinsics::_putIntVolatile:
  case vmIntrinsics::_putLongVolatile:
  case vmIntrinsics::_putFloatVolatile:
  case vmIntrinsics::_putDoubleVolatile:
  case vmIntrinsics::_getObjectAcquire:
  case vmIntrinsics::_getBooleanAcquire:
  case vmIntrinsics::_getByteAcquire:
  case vmIntrinsics::_getShortAcquire:
  case vmIntrinsics::_getCharAcquire:
  case vmIntrinsics::_getIntAcquire:
  case vmIntrinsics::_getLongAcquire:
  case vmIntrinsics::_getFloatAcquire:
  case vmIntrinsics::_getDoubleAcquire:
  case vmIntrinsics::_putObjectRelease:
  case vmIntrinsics::_putBooleanRelease:
  case vmIntrinsics::_putByteRelease:
  case vmIntrinsics::_putShortRelease:
  case vmIntrinsics::_putCharRelease:
  case vmIntrinsics::_putIntRelease:
  case vmIntrinsics::_putLongRelease:
  case vmIntrinsics::_putFloatRelease:
  case vmIntrinsics::_putDoubleRelease:
  case vmIntrinsics::_getObjectOpaque:
  case vmIntrinsics::_getBooleanOpaque:
  case vmIntrinsics::_getByteOpaque:
  case vmIntrinsics::_getShortOpaque:
  case vmIntrinsics::_getCharOpaque:
  case vmIntrinsics::_getIntOpaque:
  case vmIntrinsics::_getLongOpaque:
  case vmIntrinsics::_getFloatOpaque:
  case vmIntrinsics::_getDoubleOpaque:
  case vmIntrinsics::_putObjectOpaque:
  case vmIntrinsics::_putBooleanOpaque:
  case vmIntrinsics::_putByteOpaque:
  case vmIntrinsics::_putShortOpaque:
  case vmIntrinsics::_putCharOpaque:
  case vmIntrinsics::_putIntOpaque:
  case vmIntrinsics::_putLongOpaque:
  case vmIntrinsics::_putFloatOpaque:
  case vmIntrinsics::_putDoubleOpaque:
  case vmIntrinsics::_getAndAddInt:
  case vmIntrinsics::_getAndAddLong:
  case vmIntrinsics::_getAndSetInt:
  case vmIntrinsics::_getAndSetLong:
  case vmIntrinsics::_getAndSetObject:
  case vmIntrinsics::_loadFence:
  case vmIntrinsics::_storeFence:
  case vmIntrinsics::_fullFence:
  case vmIntrinsics::_compareAndSetLong:
  case vmIntrinsics::_weakCompareAndSetLong:
  case vmIntrinsics::_weakCompareAndSetLongPlain:
  case vmIntrinsics::_weakCompareAndSetLongAcquire:
  case vmIntrinsics::_weakCompareAndSetLongRelease:
  case vmIntrinsics::_compareAndSetInt:
  case vmIntrinsics::_weakCompareAndSetInt:
  case vmIntrinsics::_weakCompareAndSetIntPlain:
  case vmIntrinsics::_weakCompareAndSetIntAcquire:
  case vmIntrinsics::_weakCompareAndSetIntRelease:
  case vmIntrinsics::_compareAndSetObject:
  case vmIntrinsics::_weakCompareAndSetObject:
  case vmIntrinsics::_weakCompareAndSetObjectPlain:
  case vmIntrinsics::_weakCompareAndSetObjectAcquire:
  case vmIntrinsics::_weakCompareAndSetObjectRelease:
  case vmIntrinsics::_compareAndExchangeInt:
  case vmIntrinsics::_compareAndExchangeIntAcquire:
  case vmIntrinsics::_compareAndExchangeIntRelease:
  case vmIntrinsics::_compareAndExchangeLong:
  case vmIntrinsics::_compareAndExchangeLongAcquire:
  case vmIntrinsics::_compareAndExchangeLongRelease:
  case vmIntrinsics::_compareAndExchangeObject:
  case vmIntrinsics::_compareAndExchangeObjectAcquire:
  case vmIntrinsics::_compareAndExchangeObjectRelease:
    if (!InlineUnsafeOps) return true;
    break;
  case vmIntrinsics::_getShortUnaligned:
  case vmIntrinsics::_getCharUnaligned:
  case vmIntrinsics::_getIntUnaligned:
  case vmIntrinsics::_getLongUnaligned:
  case vmIntrinsics::_putShortUnaligned:
  case vmIntrinsics::_putCharUnaligned:
  case vmIntrinsics::_putIntUnaligned:
  case vmIntrinsics::_putLongUnaligned:
  case vmIntrinsics::_allocateInstance:
    if (!InlineUnsafeOps || !UseUnalignedAccesses) return true;
    break;
  case vmIntrinsics::_hashCode:
    if (!InlineObjectHash) return true;
    break;
  case vmIntrinsics::_aescrypt_encryptBlock:
  case vmIntrinsics::_aescrypt_decryptBlock:
    if (!UseAESIntrinsics) return true;
    break;
  case vmIntrinsics::_cipherBlockChaining_encryptAESCrypt:
  case vmIntrinsics::_cipherBlockChaining_decryptAESCrypt:
    if (!UseAESIntrinsics) return true;
    break;
  case vmIntrinsics::_counterMode_AESCrypt:
    if (!UseAESCTRIntrinsics) return true;
    break;
  case vmIntrinsics::_sha_implCompress:
    if (!UseSHA1Intrinsics) return true;
    break;
  case vmIntrinsics::_sha2_implCompress:
    if (!UseSHA256Intrinsics) return true;
    break;
  case vmIntrinsics::_sha5_implCompress:
    if (!UseSHA512Intrinsics) return true;
    break;
  case vmIntrinsics::_digestBase_implCompressMB:
    if (!(UseSHA1Intrinsics || UseSHA256Intrinsics || UseSHA512Intrinsics)) return true;
    break;
  case vmIntrinsics::_ghash_processBlocks:
    if (!UseGHASHIntrinsics) return true;
    break;
  case vmIntrinsics::_base64_encodeBlock:
    if (!UseBASE64Intrinsics) return true;
    break;
  case vmIntrinsics::_updateBytesCRC32C:
  case vmIntrinsics::_updateDirectByteBufferCRC32C:
    if (!UseCRC32CIntrinsics) return true;
    break;
  case vmIntrinsics::_vectorizedMismatch:
    if (!UseVectorizedMismatchIntrinsic) return true;
    break;
  case vmIntrinsics::_updateBytesAdler32:
  case vmIntrinsics::_updateByteBufferAdler32:
    if (!UseAdler32Intrinsics) return true;
    break;
  case vmIntrinsics::_copyMemory:
    if (!InlineArrayCopy || !InlineUnsafeOps) return true;
    break;
  case vmIntrinsics::_checkIndex:
    if (!InlineNIOCheckIndex) return true;
    break;
  default:
    return false;
  }

  return false;
}

#define VM_INTRINSIC_INITIALIZE(id, klass, name, sig, flags) #id "\0"
static const char* vm_intrinsic_name_bodies = VM_INTRINSICS_DO(VM_INTRINSIC_INITIALIZE, VM_SYMBOL_IGNORE, VM_SYMBOL_IGNORE, VM_SYMBOL_IGNORE, VM_ALIAS_IGNORE);

static const char* vm_intrinsic_name_table[vmIntrinsics::ID_LIMIT];

const char* vmIntrinsics::name_at(vmIntrinsics::ID id) {
  const char** nt = &vm_intrinsic_name_table[0];
  if (nt[_none] == NULL) {
    char* string = (char*) &vm_intrinsic_name_bodies[0];
    for (int index = FIRST_ID; index < ID_LIMIT; index++) {
      nt[index] = string;
      string += strlen(string); // skip string body
      string += 1;              // skip trailing null
    }
    nt[_none] = "_none";
  }
  if ((uint)id < (uint)ID_LIMIT)
    return vm_intrinsic_name_table[(uint)id];
  else
    return "(unknown intrinsic)";
}

// These are flag-matching functions:
inline bool match_F_R(jshort flags) {
  const int req = 0;
  const int neg = JVM_ACC_STATIC | JVM_ACC_SYNCHRONIZED;
  return (flags & (req | neg)) == req;
}
inline bool match_F_Y(jshort flags) {
  const int req = JVM_ACC_SYNCHRONIZED;
  const int neg = JVM_ACC_STATIC;
  return (flags & (req | neg)) == req;
}
inline bool match_F_RN(jshort flags) {
  const int req = JVM_ACC_NATIVE;
  const int neg = JVM_ACC_STATIC | JVM_ACC_SYNCHRONIZED;
  return (flags & (req | neg)) == req;
}
inline bool match_F_S(jshort flags) {
  const int req = JVM_ACC_STATIC;
  const int neg = JVM_ACC_SYNCHRONIZED;
  return (flags & (req | neg)) == req;
}
inline bool match_F_SN(jshort flags) {
  const int req = JVM_ACC_STATIC | JVM_ACC_NATIVE;
  const int neg = JVM_ACC_SYNCHRONIZED;
  return (flags & (req | neg)) == req;
}
inline bool match_F_RNY(jshort flags) {
  const int req = JVM_ACC_NATIVE | JVM_ACC_SYNCHRONIZED;
  const int neg = JVM_ACC_STATIC;
  return (flags & (req | neg)) == req;
}

// These are for forming case labels:
#define ID3(x, y, z) (( jlong)(z) + \
                      ((jlong)(y) <<    vmSymbols::log2_SID_LIMIT) + \
                      ((jlong)(x) << (2*vmSymbols::log2_SID_LIMIT))  )
#define SID_ENUM(n) vmSymbols::VM_SYMBOL_ENUM_NAME(n)

vmIntrinsics::ID vmIntrinsics::find_id_impl(vmSymbols::SID holder,
                                            vmSymbols::SID name,
                                            vmSymbols::SID sig,
                                            jshort flags) {
  // Let the C compiler build the decision tree.

#define VM_INTRINSIC_CASE(id, klass, name, sig, fcode) \
  case ID3(SID_ENUM(klass), SID_ENUM(name), SID_ENUM(sig)): \
    if (!match_##fcode(flags))  break; \
    return id;

  switch (ID3(holder, name, sig)) {
    VM_INTRINSICS_DO(VM_INTRINSIC_CASE,
                     VM_SYMBOL_IGNORE, VM_SYMBOL_IGNORE, VM_SYMBOL_IGNORE, VM_ALIAS_IGNORE);
  }
  return vmIntrinsics::_none;

#undef VM_INTRINSIC_CASE
}

const char* vmIntrinsics::short_name_as_C_string(vmIntrinsics::ID id, char* buf, int buflen) {
  const char* str = name_at(id);
  return str;
}

// These are to get information about intrinsics.

#define ID4(x, y, z, f) ((ID3(x, y, z) << vmIntrinsics::log2_FLAG_LIMIT) | (jlong) (f))

static const jlong intrinsic_info_array[vmIntrinsics::ID_LIMIT+1] = {
#define VM_INTRINSIC_INFO(ignore_id, klass, name, sig, fcode) \
  ID4(SID_ENUM(klass), SID_ENUM(name), SID_ENUM(sig), vmIntrinsics::fcode),

  0, VM_INTRINSICS_DO(VM_INTRINSIC_INFO,
                     VM_SYMBOL_IGNORE, VM_SYMBOL_IGNORE, VM_SYMBOL_IGNORE, VM_ALIAS_IGNORE)
    0
#undef VM_INTRINSIC_INFO
};

inline jlong intrinsic_info(vmIntrinsics::ID id) {
  return intrinsic_info_array[vmIntrinsics::ID_from((int)id)];
}

vmSymbols::SID vmIntrinsics::class_for(vmIntrinsics::ID id) {
  jlong info = intrinsic_info(id);
  int shift = 2*vmSymbols::log2_SID_LIMIT + log2_FLAG_LIMIT, mask = right_n_bits(vmSymbols::log2_SID_LIMIT);
  return vmSymbols::SID( (info >> shift) & mask );
}

vmSymbols::SID vmIntrinsics::name_for(vmIntrinsics::ID id) {
  jlong info = intrinsic_info(id);
  int shift = vmSymbols::log2_SID_LIMIT + log2_FLAG_LIMIT, mask = right_n_bits(vmSymbols::log2_SID_LIMIT);
  return vmSymbols::SID( (info >> shift) & mask );
}

vmSymbols::SID vmIntrinsics::signature_for(vmIntrinsics::ID id) {
  jlong info = intrinsic_info(id);
  int shift = log2_FLAG_LIMIT, mask = right_n_bits(vmSymbols::log2_SID_LIMIT);
  return vmSymbols::SID( (info >> shift) & mask );
}

vmIntrinsics::Flags vmIntrinsics::flags_for(vmIntrinsics::ID id) {
  jlong info = intrinsic_info(id);
  int shift = 0, mask = right_n_bits(log2_FLAG_LIMIT);
  return Flags( (info >> shift) & mask );
}
