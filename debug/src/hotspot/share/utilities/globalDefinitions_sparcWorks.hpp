#ifndef SHARE_VM_UTILITIES_GLOBALDEFINITIONS_SPARCWORKS_HPP
#define SHARE_VM_UTILITIES_GLOBALDEFINITIONS_SPARCWORKS_HPP

#include "jni.h"

// This file holds compiler-dependent includes,
// globally used constants & types, class (forward)
// declarations and a few frequently used utility functions.

# include <ctype.h>
# include <string.h>
# include <strings.h>     // for bsd'isms
# include <stdarg.h>
# include <stddef.h>      // for offsetof
# include <stdio.h>
# include <stdlib.h>
# include <wchar.h>
# include <stdarg.h>
# include <math.h>
# include <time.h>
# include <fcntl.h>
# include <dlfcn.h>
# include <pthread.h>
# include <limits.h>
# include <errno.h>

#include <inttypes.h>

#ifdef LINUX
# include <signal.h>
# include <ucontext.h>
# include <sys/time.h>
#endif

// NULL vs NULL_WORD:
// On Linux NULL is defined as a special type '__null'. Assigning __null to
// integer variable will cause gcc warning. Use NULL_WORD in places where a
// pointer is stored as integer value. On some platforms, sizeof(intptr_t) >
// sizeof(void*), so here we want something which is integer type, but has the
// same size as a pointer.
#ifdef LINUX
  #define NULL_WORD  0L
#else
  #define NULL_WORD  NULL
#endif

#ifndef LINUX
// Compiler-specific primitive types
typedef unsigned short     uint16_t;
#ifndef _UINT32_T
#define _UINT32_T
typedef unsigned int       uint32_t;
#endif
#if !defined(_SYS_INT_TYPES_H)
#ifndef _UINT64_T
#define _UINT64_T
typedef unsigned long long uint64_t;
#endif
// %%%% how to access definition of intptr_t portably in 5.5 onward?
typedef int                     intptr_t;
typedef unsigned int            uintptr_t;
// If this gets an error, figure out a symbol XXX that implies the
// prior definition of intptr_t, and add "&& !defined(XXX)" above.
#endif
#endif

// On solaris 8, UINTPTR_MAX is defined as empty.
// Everywhere else it's an actual value.
#if UINTPTR_MAX - 1 == -1
#undef UINTPTR_MAX
#define UINTPTR_MAX UINT64_MAX
#endif

// Additional Java basic types

typedef unsigned char      jubyte;
typedef unsigned short     jushort;
typedef unsigned int       juint;
typedef unsigned long long julong;

// checking for nanness
#if LINUX
inline int g_isnan(float f) { return isnanf(f); }
inline int g_isnan(double f) { return isnan(f); }
#else
#error "missing platform-specific definition here"
#endif

// Checking for finiteness

inline int g_isfinite(jfloat f) { return finite(f); }
inline int g_isfinite(jdouble f) { return finite(f); }

// Wide characters

inline int wcslen(const jchar* x) { return wcslen((const wchar_t*)x); }

// Formatting.
#define FORMAT64_MODIFIER "l"

#define offset_of(klass,field) offsetof(klass,field)

#ifndef USE_LIBRARY_BASED_TLS_ONLY
#define THREAD_LOCAL_DECL __thread
#endif

// Inlining support
#define NOINLINE
#define ALWAYSINLINE inline __attribute__((always_inline))

// Alignment
#define ATTRIBUTE_ALIGNED(x) __attribute__((aligned(x)))

#endif
