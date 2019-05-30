#ifndef SHARE_VM_UTILITIES_GLOBALDEFINITIONS_GCC_HPP
#define SHARE_VM_UTILITIES_GLOBALDEFINITIONS_GCC_HPP

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

#ifdef SOLARIS
#include <ieeefp.h>
#endif

#include <math.h>
#include <time.h>
#include <fcntl.h>
#include <dlfcn.h>
#include <pthread.h>

#ifdef SOLARIS
#include <thread.h>
#endif

#include <limits.h>
#include <errno.h>

#ifdef SOLARIS
#include <sys/trap.h>
#include <sys/regset.h>
#include <sys/procset.h>
#include <ucontext.h>
#include <setjmp.h>
#endif

# ifdef SOLARIS_MUTATOR_LIBTHREAD
# include <sys/procfs.h>
# endif

#if defined(LINUX) || defined(_ALLBSD_SOURCE)
#include <inttypes.h>
#include <signal.h>
#ifndef __OpenBSD__
#include <ucontext.h>
#endif
#ifdef __APPLE__
  #include <AvailabilityMacros.h>
  #include <mach/mach.h>
#endif
#include <sys/time.h>
#endif

// NULL vs NULL_WORD:
// On Linux NULL is defined as a special type '__null'. Assigning __null to
// integer variable will cause gcc warning. Use NULL_WORD in places where a
// pointer is stored as integer value.  On some platforms, sizeof(intptr_t) >
// sizeof(void*), so here we want something which is integer type, but has the
// same size as a pointer.
#ifdef __GNUC__
  #define NULL_WORD  0L
#else
  #define NULL_WORD  NULL
#endif

#if !defined(LINUX) && !defined(_ALLBSD_SOURCE)
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

// Additional Java basic types

typedef uint8_t  jubyte;
typedef uint16_t jushort;
typedef uint32_t juint;
typedef uint64_t julong;

#ifdef SOLARIS
// ANSI C++ fixes
// NOTE:In the ANSI committee's continuing attempt to make each version
// of C++ incompatible with the previous version, you can no longer cast
// pointers to functions without specifying linkage unless you want to get
// warnings.
//
// This also means that pointers to functions can no longer be "hidden"
// in opaque types like void * because at the invokation point warnings
// will be generated. While this makes perfect sense from a type safety
// point of view it causes a lot of warnings on old code using C header
// files. Here are some typedefs to make the job of silencing warnings
// a bit easier.
//
// The final kick in the teeth is that you can only have extern "C" linkage
// specified at file scope. So these typedefs are here rather than in the
// .hpp for the class (os:Solaris usually) that needs them.

extern "C" {
   typedef int (*int_fnP_thread_t_iP_uP_stack_tP_gregset_t)(thread_t, int*, unsigned *, stack_t*, gregset_t);
   typedef int (*int_fnP_thread_t_i_gregset_t)(thread_t, int, gregset_t);
   typedef int (*int_fnP_thread_t_i)(thread_t, int);
   typedef int (*int_fnP_thread_t)(thread_t);

   typedef int (*int_fnP_cond_tP_mutex_tP_timestruc_tP)(cond_t *cv, mutex_t *mx, timestruc_t *abst);
   typedef int (*int_fnP_cond_tP_mutex_tP)(cond_t *cv, mutex_t *mx);

   // typedef for missing API in libc
   typedef int (*int_fnP_mutex_tP_i_vP)(mutex_t *, int, void *);
   typedef int (*int_fnP_mutex_tP)(mutex_t *);
   typedef int (*int_fnP_cond_tP_i_vP)(cond_t *cv, int scope, void *arg);
   typedef int (*int_fnP_cond_tP)(cond_t *cv);
};
#endif

// checking for nanness
#ifdef SOLARIS
#ifdef SPARC
inline int g_isnan(float  f) { return isnanf(f); }
#else
// isnanf() broken on Intel Solaris use isnand()
inline int g_isnan(float  f) { return isnand(f); }
#endif
inline int g_isnan(double f) { return isnand(f); }
#elif defined(__APPLE__)
inline int g_isnan(double f) { return isnan(f); }
#elif defined(LINUX) || defined(_ALLBSD_SOURCE)
inline int g_isnan(float  f) { return isnanf(f); }
inline int g_isnan(double f) { return isnan(f); }
#else
#error "missing platform-specific definition here"
#endif

// GCC 4.3 does not allow 0.0/0.0 to produce a NAN value
#if (__GNUC__ == 4) && (__GNUC_MINOR__ > 2)
#define CAN_USE_NAN_DEFINE 1
#endif

// Checking for finiteness

inline int g_isfinite(jfloat  f)                 { return finite(f); }
inline int g_isfinite(jdouble f)                 { return finite(f); }

// Wide characters

inline int wcslen(const jchar* x) { return wcslen((const wchar_t*)x); }

// Portability macros
#define PRAGMA_INTERFACE             #pragma interface
#define PRAGMA_IMPLEMENTATION        #pragma implementation

#if (__GNUC__ == 2) && (__GNUC_MINOR__ < 95)
#define TEMPLATE_TABLE_BUG
#endif
#if (__GNUC__ == 2) && (__GNUC_MINOR__ >= 96)
#define CONST_SDM_BUG
#endif

// Formatting.
#ifdef __APPLE__
#define FORMAT64_MODIFIER "ll"
#else
#define FORMAT64_MODIFIER "l"
#endif

// HACK: gcc warns about applying offsetof() to non-POD object or calculating
//       offset directly when base address is NULL. Use 16 to get around the
//       warning. gcc-3.4 has an option -Wno-invalid-offsetof to suppress
//       this warning.
#define offset_of(klass,field) (size_t)((intx)&(((klass*)16)->field) - 16)

#ifdef offsetof
# undef offsetof
#endif
#define offsetof(klass,field) offset_of(klass,field)

#if defined(__APPLE__)
#define JLONG_FORMAT           "%ld"
#endif

#ifndef USE_LIBRARY_BASED_TLS_ONLY
#define THREAD_LOCAL_DECL __thread
#endif

// Inlining support
#define NOINLINE     __attribute__ ((noinline))
#define ALWAYSINLINE inline __attribute__ ((always_inline))

// Alignment
//
// NOTE! The "+0" below is a workaround for a known bug in older GCC versions
// (known to fail with 4.6.0, fixed in 4.9.0). This bug affects systems such as
// RedHat/Oracle Linux 7.5, which ships with GCC 4.8.5. For more details, see
// https://gcc.gnu.org/bugzilla/show_bug.cgi?id=55382 and
// https://gcc.gnu.org/bugzilla/show_bug.cgi?id=53017
//
// GCC versions older than 4.6.4 would fail even with "+0", and needs additional
// cast to typeof(x) to work around the similar bug.
//
#define ATTRIBUTE_ALIGNED(x) __attribute__((aligned((typeof(x))x+0)))

#endif
