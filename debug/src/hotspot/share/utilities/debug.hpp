#ifndef SHARE_VM_UTILITIES_DEBUG_HPP
#define SHARE_VM_UTILITIES_DEBUG_HPP

#include "utilities/breakpoint.hpp"
#include "utilities/compilerWarnings.hpp"
#include "utilities/macros.hpp"

#include <stddef.h>

// ShowRegistersOnAssert support (for now Linux only)
#if defined(LINUX) && !defined(ZERO)
#define CAN_SHOW_REGISTERS_ON_ASSERT
extern char* g_assert_poison;
#define TOUCH_ASSERT_POISON (*g_assert_poison) = 'X';
void initialize_assert_poison();
bool handle_assert_poison_fault(const void* ucVoid, const void* faulting_address);
#else
#define TOUCH_ASSERT_POISON
#endif

// assertions
#define vmassert(p, ...)

// For backward compatibility.
#define assert(p, ...) vmassert(p, __VA_ARGS__)

#define vmassert_status(p, status, msg)

// For backward compatibility.
#define assert_status(p, status, msg) vmassert_status(p, status, msg)

// guarantee is like vmassert except it's always executed -- use it for
// cheap tests that catch errors that would otherwise be hard to find.
// guarantee is also used for Verify options.
#define guarantee(p, ...) \
do { \
  if (!(p)) { \
    TOUCH_ASSERT_POISON; \
    report_vm_error(__FILE__, __LINE__, "guarantee(" #p ") failed", __VA_ARGS__); \
    BREAKPOINT; \
  } \
} while (0)

#define fatal(...) \
do { \
  TOUCH_ASSERT_POISON; \
  report_fatal(__FILE__, __LINE__, __VA_ARGS__); \
  BREAKPOINT; \
} while (0)

// out of memory
#define vm_exit_out_of_memory(size, vm_err_type, ...) \
do { \
  report_vm_out_of_memory(__FILE__, __LINE__, size, vm_err_type, __VA_ARGS__); \
  BREAKPOINT; \
} while (0)

#define ShouldNotCallThis() \
do { \
  TOUCH_ASSERT_POISON; \
  report_should_not_call(__FILE__, __LINE__); \
  BREAKPOINT; \
} while (0)

#define ShouldNotReachHere() \
do { \
  TOUCH_ASSERT_POISON; \
  report_should_not_reach_here(__FILE__, __LINE__); \
  BREAKPOINT; \
} while (0)

#define Unimplemented() \
do { \
  TOUCH_ASSERT_POISON; \
  report_unimplemented(__FILE__, __LINE__); \
  BREAKPOINT; \
} while (0)

#define Untested(msg) \
do { \
  report_untested(__FILE__, __LINE__, msg); \
  BREAKPOINT; \
} while (0);

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
void report_vm_error(const char* file, int line, const char* error_msg,
                     const char* detail_fmt, ...) ATTRIBUTE_PRINTF(4, 5);
#else
// GCC < 4.8 warns because of empty format string.  Warning can not be switched off selectively.
void report_vm_error(const char* file, int line, const char* error_msg,
                     const char* detail_fmt, ...);
#endif
void report_vm_status_error(const char* file, int line, const char* error_msg,
                            int status, const char* detail);
void report_fatal(const char* file, int line, const char* detail_fmt, ...) ATTRIBUTE_PRINTF(3, 4);
void report_vm_out_of_memory(const char* file, int line, size_t size, VMErrorType vm_err_type,
                             const char* detail_fmt, ...) ATTRIBUTE_PRINTF(5, 6);
void report_should_not_call(const char* file, int line);
void report_should_not_reach_here(const char* file, int line);
void report_unimplemented(const char* file, int line);
void report_untested(const char* file, int line, const char* message);

void warning(const char* format, ...) ATTRIBUTE_PRINTF(1, 2);

// Compile-time asserts.  Cond must be a compile-time constant expression that
// is convertible to bool.  STATIC_ASSERT() can be used anywhere a declaration
// may appear.
//
// Implementation Note: STATIC_ASSERT_FAILURE<true> provides a value member
// rather than type member that could be used directly in the typedef, because
// a type member would require conditional use of "typename", depending on
// whether Cond is dependent or not.  The use of a value member leads to the
// use of an array type.

template<bool x> struct STATIC_ASSERT_FAILURE;
template<> struct STATIC_ASSERT_FAILURE<true> { enum { value = 1 }; };

#define STATIC_ASSERT(Cond) \
  typedef char PASTE_TOKENS(STATIC_ASSERT_DUMMY_TYPE_, __LINE__)[ \
    STATIC_ASSERT_FAILURE< (Cond) >::value ]

// out of memory reporting
void report_java_out_of_memory(const char* message);

#endif
