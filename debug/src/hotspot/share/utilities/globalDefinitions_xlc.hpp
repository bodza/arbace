#ifndef SHARE_VM_UTILITIES_GLOBALDEFINITIONS_XLC_HPP
#define SHARE_VM_UTILITIES_GLOBALDEFINITIONS_XLC_HPP

#include "jni.h"

// This file holds compiler-dependent includes,
// globally used constants & types, class (forward)
// declarations and a few frequently used utility functions.

#include <ctype.h>
#include <string.h>
#include <stdarg.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <wchar.h>

#include <math.h>
#include <time.h>
#include <fcntl.h>
#include <dlfcn.h>
#include <pthread.h>

#include <limits.h>
#include <errno.h>

#include <stdint.h>

// Use XLC compiler builtins instead of inline assembler
#define USE_XLC_BUILTINS
#ifdef USE_XLC_BUILTINS
#include <builtins.h>
  #if __IBMCPP__ < 1000
  // the funtion prototype for __dcbtst(void *) is missing in XLC V8.0
  // I could compile a little test, where I provided the prototype.
  // The generated code was correct there. This is the prototype:
  // extern "builtin" void __dcbtst (void *);
  // For now we don't make use of it when compiling with XLC V8.0
  #else
  // __IBMCPP__ >= 1000
  // XLC V10 provides the prototype for __dcbtst (void *);
  #define USE_XLC_PREFETCH_WRITE_BUILTIN
  #endif
#endif

// NULL vs NULL_WORD:
// On Linux NULL is defined as a special type '__null'. Assigning __null to
// integer variable will cause gcc warning. Use NULL_WORD in places where a
// pointer is stored as integer value.  On some platforms, sizeof(intptr_t) >
// sizeof(void*), so here we want something which is integer type, but has the
// same size as a pointer.
#ifdef __GNUC__
  #error XLC and __GNUC__?
#else
  #define NULL_WORD  NULL
#endif

// Compiler-specific primitive types
// All defs of int (uint16_6 etc) are defined in AIX' /usr/include/stdint.h

// Additional Java basic types

typedef uint8_t  jubyte;
typedef uint16_t jushort;
typedef uint32_t juint;
typedef uint64_t julong;

// checking for nanness
#ifdef AIX
inline int g_isnan(float f) { return isnan(f); }
inline int g_isnan(double f) { return isnan(f); }
#else
#error "missing platform-specific definition here"
#endif

// Checking for finiteness

inline int g_isfinite(jfloat f) { return finite(f); }
inline int g_isfinite(jdouble f) { return finite(f); }

// Wide characters

inline int wcslen(const jchar* x) { return wcslen((const wchar_t*)x); }

// Portability macros
#define PRAGMA_INTERFACE             #pragma interface
#define PRAGMA_IMPLEMENTATION        #pragma implementation

// Formatting.
#define FORMAT64_MODIFIER "l"

// Cannot use xlc's offsetof as implementation of hotspot's
// offset_of(), because xlc warns about applying offsetof() to non-POD
// object and xlc cannot compile the expression offsetof(DataLayout,
// _cells[index]) in DataLayout::cell_offset() .  Therefore we define
// offset_of as it is defined for gcc.
#define offset_of(klass,field) (size_t)((intx)&(((klass*)16)->field) - 16)

// AIX 5.3 has buggy __thread support. (see JDK-8176442).
#define USE_LIBRARY_BASED_TLS_ONLY 1

#ifndef USE_LIBRARY_BASED_TLS_ONLY
#define THREAD_LOCAL_DECL __thread
#endif

// Inlining support
//
// Be aware that for function/method declarations, xlC only supports the following
// syntax (i.e. the attribute must be placed AFTER the function/method declarator):
//
//   void* operator new(size_t size) throw() NOINLINE;
//
// For function/method defintions, the more common placement BEFORE the
// function/method declarator seems to be supported as well:
//
//   NOINLINE void* CHeapObj<F>::operator new(size_t size) throw() {...}

#define NOINLINE     __attribute__((__noinline__))
#define ALWAYSINLINE inline __attribute__((__always_inline__))

#endif
