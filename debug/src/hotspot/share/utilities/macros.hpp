#ifndef SHARE_VM_UTILITIES_MACROS_HPP
#define SHARE_VM_UTILITIES_MACROS_HPP

// Use this to mark code that needs to be cleaned up (for development only)
#define NEEDS_CLEANUP

// Makes a string of the argument (which is not macro-expanded)
#define STR(a)  #a

// Makes a string of the macro expansion of a
#define XSTR(a) STR(a)

// Allow commas in macro arguments.
#define COMMA ,

// Apply pre-processor token pasting to the expansions of x and y.
// The token pasting operator (##) prevents its arguments from being
// expanded.  This macro allows expansion of its arguments before the
// concatenation is performed.  Note: One auxilliary level ought to be
// sufficient, but two are used because of bugs in some preprocesors.
#define PASTE_TOKENS(x, y) PASTE_TOKENS_AUX(x, y)
#define PASTE_TOKENS_AUX(x, y) PASTE_TOKENS_AUX2(x, y)
#define PASTE_TOKENS_AUX2(x, y) x ## y

#ifdef CHECK_UNHANDLED_OOPS
#define CHECK_UNHANDLED_OOPS_ONLY(code) code
#define NOT_CHECK_UNHANDLED_OOPS(code)
#else
#define CHECK_UNHANDLED_OOPS_ONLY(code)
#define NOT_CHECK_UNHANDLED_OOPS(code)  code
#endif

#ifdef CC_INTERP
#define CC_INTERP_ONLY(code) code
#define NOT_CC_INTERP(code)
#else
#define CC_INTERP_ONLY(code)
#define NOT_CC_INTERP(code) code
#endif

#ifdef LINUX
#define LINUX_ONLY(code) code
#define NOT_LINUX(code)
#else
#define LINUX_ONLY(code)
#define NOT_LINUX(code) code
#endif

#ifdef AIX
#define AIX_ONLY(code) code
#define NOT_AIX(code)
#else
#define AIX_ONLY(code)
#define NOT_AIX(code) code
#endif

#ifdef SOLARIS
#define SOLARIS_ONLY(code) code
#define NOT_SOLARIS(code)
#else
#define SOLARIS_ONLY(code)
#define NOT_SOLARIS(code) code
#endif

#if defined(__FreeBSD__) || defined(__NetBSD__) || defined(__OpenBSD__) || defined(__APPLE__)
#ifndef BSD
#define BSD
#endif
#define BSD_ONLY(code) code
#define NOT_BSD(code)
#else
#define BSD_ONLY(code)
#define NOT_BSD(code) code
#endif

#if defined(ZERO)
#define ZERO_ONLY(code) code
#define NOT_ZERO(code)
#else
#define ZERO_ONLY(code)
#define NOT_ZERO(code) code
#endif

#if defined(IA32) || defined(AMD64)
#define X86
#define X86_ONLY(code) code
#define NOT_X86(code)
#else
#undef X86
#define X86_ONLY(code)
#define NOT_X86(code) code
#endif

#ifdef IA32
#define IA32_ONLY(code) code
#define NOT_IA32(code)
#else
#define IA32_ONLY(code)
#define NOT_IA32(code) code
#endif

// This is a REALLY BIG HACK, but on AIX <sys/systemcfg.h> unconditionally defines IA64.
// At least on AIX 7.1 this is a real problem because 'systemcfg.h' is indirectly included
// by 'pthread.h' and other common system headers.

#if defined(IA64)
#define IA64_ONLY(code) code
#define NOT_IA64(code)
#else
#define IA64_ONLY(code)
#define NOT_IA64(code) code
#endif

#ifdef AMD64
#define AMD64_ONLY(code) code
#define NOT_AMD64(code)
#else
#define AMD64_ONLY(code)
#define NOT_AMD64(code) code
#endif

#ifdef S390
#define S390_ONLY(code) code
#define NOT_S390(code)
#else
#define S390_ONLY(code)
#define NOT_S390(code) code
#endif

#ifdef SPARC
#define SPARC_ONLY(code) code
#define NOT_SPARC(code)
#else
#define SPARC_ONLY(code)
#define NOT_SPARC(code) code
#endif

#if defined(PPC32) || defined(PPC64)
#ifndef PPC
#define PPC
#endif
#define PPC_ONLY(code) code
#define NOT_PPC(code)
#else
#undef PPC
#define PPC_ONLY(code)
#define NOT_PPC(code) code
#endif

#ifdef PPC32
#define PPC32_ONLY(code) code
#define NOT_PPC32(code)
#else
#define PPC32_ONLY(code)
#define NOT_PPC32(code) code
#endif

#ifdef PPC64
#define PPC64_ONLY(code) code
#define NOT_PPC64(code)
#else
#define PPC64_ONLY(code)
#define NOT_PPC64(code) code
#endif

#ifdef E500V2
#define E500V2_ONLY(code) code
#define NOT_E500V2(code)
#else
#define E500V2_ONLY(code)
#define NOT_E500V2(code) code
#endif

// Note: There are three ARM ports. They set the following in the makefiles:
// 1. Closed 32-bit port:   -DARM -DARM32           -DTARGET_ARCH_arm
// 2. Closed 64-bit port:   -DARM -DAARCH64 -D_LP64 -DTARGET_ARCH_arm
// 3. Open   64-bit port:         -DAARCH64 -D_LP64 -DTARGET_ARCH_aaarch64
#ifdef ARM
#define ARM_ONLY(code) code
#define NOT_ARM(code)
#else
#define ARM_ONLY(code)
#define NOT_ARM(code) code
#endif

#ifdef ARM32
#define ARM32_ONLY(code) code
#define NOT_ARM32(code)
#else
#define ARM32_ONLY(code)
#define NOT_ARM32(code) code
#endif

#ifdef AARCH64
#define AARCH64_ONLY(code) code
#define NOT_AARCH64(code)
#else
#define AARCH64_ONLY(code)
#define NOT_AARCH64(code) code
#endif

#ifdef VM_LITTLE_ENDIAN
#define LITTLE_ENDIAN_ONLY(code) code
#define BIG_ENDIAN_ONLY(code)
#else
#define LITTLE_ENDIAN_ONLY(code)
#define BIG_ENDIAN_ONLY(code) code
#endif

#define define_pd_global(type, name, value) const type pd_##name = value;

// Helper macros for constructing file names for includes.
#define CPU_HEADER_STEM(basename) PASTE_TOKENS(basename, INCLUDE_SUFFIX_CPU)
#define OS_HEADER_STEM(basename) PASTE_TOKENS(basename, INCLUDE_SUFFIX_OS)
#define OS_CPU_HEADER_STEM(basename) PASTE_TOKENS(basename, PASTE_TOKENS(INCLUDE_SUFFIX_OS, INCLUDE_SUFFIX_CPU))
#define COMPILER_HEADER_STEM(basename) PASTE_TOKENS(basename, INCLUDE_SUFFIX_COMPILER)

// Include platform dependent files.
//
// This macro constructs from basename and INCLUDE_SUFFIX_OS /
// INCLUDE_SUFFIX_CPU / INCLUDE_SUFFIX_COMPILER, which are set on
// the command line, the name of platform dependent files to be included.
// Example: INCLUDE_SUFFIX_OS=_linux / INCLUDE_SUFFIX_CPU=_sparc
//   CPU_HEADER_INLINE(macroAssembler) --> macroAssembler_sparc.inline.hpp
//   OS_CPU_HEADER(vmStructs)          --> vmStructs_linux_sparc.hpp
//
// basename<cpu>.hpp / basename<cpu>.inline.hpp
#define CPU_HEADER_H(basename)         XSTR(CPU_HEADER_STEM(basename).h)
#define CPU_HEADER(basename)           XSTR(CPU_HEADER_STEM(basename).hpp)
#define CPU_HEADER_INLINE(basename)    XSTR(CPU_HEADER_STEM(basename).inline.hpp)
// basename<os>.hpp / basename<os>.inline.hpp
#define OS_HEADER_H(basename)          XSTR(OS_HEADER_STEM(basename).h)
#define OS_HEADER(basename)            XSTR(OS_HEADER_STEM(basename).hpp)
#define OS_HEADER_INLINE(basename)     XSTR(OS_HEADER_STEM(basename).inline.hpp)
// basename<os><cpu>.hpp / basename<os><cpu>.inline.hpp
#define OS_CPU_HEADER(basename)        XSTR(OS_CPU_HEADER_STEM(basename).hpp)
#define OS_CPU_HEADER_INLINE(basename) XSTR(OS_CPU_HEADER_STEM(basename).inline.hpp)
// basename<compiler>.hpp / basename<compiler>.inline.hpp
#define COMPILER_HEADER(basename)        XSTR(COMPILER_HEADER_STEM(basename).hpp)
#define COMPILER_HEADER_INLINE(basename) XSTR(COMPILER_HEADER_STEM(basename).inline.hpp)

// To use Atomic::inc(jshort* dest) and Atomic::dec(jshort* dest), the address must be specially
// aligned, such that (*dest) occupies the upper 16 bits of an aligned 32-bit word. The best way to
// achieve is to place your short value next to another short value, which doesn't need atomic ops.
//
// Example
//  ATOMIC_SHORT_PAIR(
//    volatile short _refcount,  // needs atomic operation
//    unsigned short _length     // number of UTF8 characters in the symbol (does not need atomic op)
//  );

#ifdef VM_LITTLE_ENDIAN
  #define ATOMIC_SHORT_PAIR(atomic_decl, non_atomic_decl) \
    non_atomic_decl; \
    atomic_decl
#else
  #define ATOMIC_SHORT_PAIR(atomic_decl, non_atomic_decl) \
    atomic_decl; \
    non_atomic_decl
#endif

#endif
