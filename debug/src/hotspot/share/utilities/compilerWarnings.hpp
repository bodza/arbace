#ifndef SHARE_VM_UTILITIES_COMPILERWARNINGS_HPP
#define SHARE_VM_UTILITIES_COMPILERWARNINGS_HPP

// Macros related to control of compiler warnings.

// We presently only have interesting macros here for gcc and variants,
// so it's not worth going through the COMPILER_HEADER() dispatch, with
// all the non-gcc files being empty.
#ifdef TARGET_COMPILER_gcc

// Diagnostic pragmas like the ones defined below in PRAGMA_FORMAT_NONLITERAL_IGNORED
// were only introduced in GCC 4.2. Because we have no other possibility to ignore
// these warnings for older versions of GCC, we simply don't decorate our printf-style
// functions with __attribute__(format) in that case.
#if ((__GNUC__ == 4) && (__GNUC_MINOR__ >= 2)) || (__GNUC__ > 4)
#ifndef ATTRIBUTE_PRINTF
#define ATTRIBUTE_PRINTF(fmt,vargs)  __attribute__((format(printf, fmt, vargs)))
#endif
#ifndef ATTRIBUTE_SCANF
#define ATTRIBUTE_SCANF(fmt,vargs)  __attribute__((format(scanf, fmt, vargs)))
#endif
#endif

#define PRAGMA_FORMAT_NONLITERAL_IGNORED _Pragma("GCC diagnostic ignored \"-Wformat-nonliteral\"") \
                                         _Pragma("GCC diagnostic ignored \"-Wformat-security\"")
#define PRAGMA_FORMAT_IGNORED _Pragma("GCC diagnostic ignored \"-Wformat\"")

#if defined(__clang_major__) && \
      (__clang_major__ >= 4 || \
      (__clang_major__ >= 3 && __clang_minor__ >= 1)) || \
    ((__GNUC__ == 4) && (__GNUC_MINOR__ >= 6)) || (__GNUC__ > 4)
// Tested to work with clang version 3.1 and better.
#define PRAGMA_DIAG_PUSH             _Pragma("GCC diagnostic push")
#define PRAGMA_DIAG_POP              _Pragma("GCC diagnostic pop")

#endif

#endif

// Defaults when not defined for the TARGET_COMPILER_xxx.

#ifndef ATTRIBUTE_PRINTF
#define ATTRIBUTE_PRINTF(fmt, vargs)
#endif
#ifndef ATTRIBUTE_SCANF
#define ATTRIBUTE_SCANF(fmt, vargs)
#endif

#ifndef PRAGMA_FORMAT_NONLITERAL_IGNORED
#define PRAGMA_FORMAT_NONLITERAL_IGNORED
#endif
#ifndef PRAGMA_FORMAT_IGNORED
#define PRAGMA_FORMAT_IGNORED
#endif

#ifndef PRAGMA_DIAG_PUSH
#define PRAGMA_DIAG_PUSH
#endif
#ifndef PRAGMA_DIAG_POP
#define PRAGMA_DIAG_POP
#endif

#endif
