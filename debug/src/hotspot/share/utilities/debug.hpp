#ifndef SHARE_VM_UTILITIES_DEBUG_HPP
#define SHARE_VM_UTILITIES_DEBUG_HPP

#include "utilities/breakpoint.hpp"
#include "utilities/compilerWarnings.hpp"
#include "utilities/macros.hpp"

#include <stddef.h>

// assertions
#define vmassert(p, ...)

// For backward compatibility.
#define assert(p, ...) vmassert(p, __VA_ARGS__)

// guarantee is like vmassert except it's always executed -- use it for
// cheap tests that catch errors that would otherwise be hard to find.
// guarantee is also used for Verify options.
#define guarantee(p, ...) \
do { \
  if (!(p)) { \
    report_vm_error(__FILE__, __LINE__, "guarantee(" #p ") failed", __VA_ARGS__); \
    BREAKPOINT; \
  } \
} while (false)

#define fatal(...) \
do { \
  report_fatal(__FILE__, __LINE__, __VA_ARGS__); \
  BREAKPOINT; \
} while (false)

// out of memory
#define vm_exit_out_of_memory(size, vm_err_type, ...) \
do { \
  report_vm_out_of_memory(__FILE__, __LINE__, size, vm_err_type, __VA_ARGS__); \
  BREAKPOINT; \
} while (false)

#define ShouldNotCallThis() \
do { \
  report_should_not_call(__FILE__, __LINE__); \
  BREAKPOINT; \
} while (false)

#define ShouldNotReachHere() \
do { \
  report_should_not_reach_here(__FILE__, __LINE__); \
  BREAKPOINT; \
} while (false)

#define Unimplemented() \
do { \
  report_unimplemented(__FILE__, __LINE__); \
  BREAKPOINT; \
} while (false)

#define Untested(msg) \
do { \
  report_untested(__FILE__, __LINE__, msg); \
  BREAKPOINT; \
} while (false);

// types of VM error - originally in vmError.hpp
enum VMErrorType {
  INTERNAL_ERROR   = 0xe0000000,
  OOM_MALLOC_ERROR = 0xe0000001,
  OOM_MMAP_ERROR   = 0xe0000002
};

// error reporting helper functions
void report_vm_error(const char* file, int line, const char* error_msg);
#if !defined(__GNUC__) || defined (__clang_major__) || (((__GNUC__ == 4) && (__GNUC_MINOR__ >= 8)) || __GNUC__ > 4)
// ATTRIBUTE_PRINTF works with gcc >= 4.8 and any other compiler.
void report_vm_error(const char* file, int line, const char* error_msg, const char* detail_fmt, ...) ATTRIBUTE_PRINTF(4, 5);
#else
// GCC < 4.8 warns because of empty format string.  Warning can not be switched off selectively.
void report_vm_error(const char* file, int line, const char* error_msg, const char* detail_fmt, ...);
#endif
void report_vm_status_error(const char* file, int line, const char* error_msg, int status, const char* detail);
void report_fatal(const char* file, int line, const char* detail_fmt, ...) ATTRIBUTE_PRINTF(3, 4);
void report_vm_out_of_memory(const char* file, int line, size_t size, VMErrorType vm_err_type, const char* detail_fmt, ...) ATTRIBUTE_PRINTF(5, 6);
void report_should_not_call(const char* file, int line);
void report_should_not_reach_here(const char* file, int line);
void report_unimplemented(const char* file, int line);
void report_untested(const char* file, int line, const char* message);

void warning(const char* format, ...) ATTRIBUTE_PRINTF(1, 2);

// out of memory reporting
void report_java_out_of_memory(const char* message);

#endif
