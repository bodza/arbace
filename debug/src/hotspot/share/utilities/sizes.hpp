#ifndef SHARE_VM_UTILITIES_SIZES_HPP
#define SHARE_VM_UTILITIES_SIZES_HPP

#include "utilities/globalDefinitions.hpp"

// The following two classes are used to represent 'sizes' and 'offsets' in the VM;
// they serve as 'unit' types. ByteSize is used for sizes measured in bytes, while
// WordSize is used for sizes measured in machine words (i.e., 32bit or 64bit words
// depending on platform).
//
// The classes are defined with friend functions operating on them instead of member
// functions so that they (the classes) can be re-#define'd to int types in optimized
// mode. This allows full type checking and maximum safety in debug mode, and full
// optimizations (constant folding) and zero overhead (time and space wise) in the
// optimized build (some compilers do not optimize one-element value classes but
// instead create an object in memory - thus the overhead may be significant).
//
// Note: 1) DO NOT add new overloaded friend functions that do not have a unique function
//          function name but require signature types for resolution. This will not work
//          in optimized mode as both, ByteSize and WordSize are mapped to the same type
//          and thus the distinction would not be possible anymore (=> compiler errors).
//
//       2) DO NOT add non-static member functions as they cannot be mapped so something
//          compilable in the optimized build. Static member functions could be added
//          but require a corresponding class definition in the optimized build.
//
// These classes should help doing a transition from (currently) word-size based offsets
// to byte-size based offsets in the VM (this will be important if we desire to pack
// objects more densely in the VM for 64bit machines). Such a transition should proceed
// in two steps to minimize the risk of introducing hard-to-find bugs:
//
// a) first transition the whole VM into a form where all sizes are strongly typed
// b) change all WordSize's to ByteSize's where desired and fix the compilation errors

// The following definitions must match the corresponding friend declarations
// in the Byte/WordSize classes if they are typedef'ed to be int. This will
// be the case in optimized mode to ensure zero overhead for these types.
//
// Note: If a compiler does not inline these function calls away, one may
//       want to use #define's to make sure full optimization (constant
//       folding in particular) is possible.

typedef int ByteSize;
inline ByteSize in_ByteSize(int size) { return size; }
inline int in_bytes (ByteSize x) { return x; }

typedef int WordSize;
inline WordSize in_WordSize(int size) { return size; }
inline int in_words (WordSize x) { return x; }

// Use the following #define to get C++ field member offsets

#define byte_offset_of(klass,field)   in_ByteSize((int)offset_of(klass, field))

#endif
