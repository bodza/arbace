#ifndef CPU_ZERO_VM_JNITYPES_ZERO_HPP
#define CPU_ZERO_VM_JNITYPES_ZERO_HPP

#include "jni.h"
#include "memory/allocation.hpp"
#include "oops/oop.hpp"

// This file holds platform-dependent routines used to write primitive jni
// types to the array of arguments passed into JavaCalls::call

class JNITypes : AllStatic {
  // These functions write a java primitive type (in native format)
  // to a java stack slot array to be passed as an argument to JavaCalls:calls.
  // I.e., they are functionally 'push' operations if they have a 'pos'
  // formal parameter.  Note that jlong's and jdouble's are written
  // _in reverse_ of the order in which they appear in the interpreter
  // stack.  This is because call stubs (see stubGenerator_zero.cpp)
  // reverse the argument list constructed by JavaCallArguments (see
  // javaCalls.hpp).

private:
  // Helper routines.
  static inline void    put_int2 (jint *from, jint *to)           { to[0] = from[0]; to[1] = from[1]; }
  static inline void    put_int2 (jint *from, jint *to, int& pos) { put_int2 (from, (jint *)((intptr_t *)to + pos)); pos += 2; }
  static inline void    put_int2r(jint *from, jint *to)           { to[0] = from[1]; to[1] = from[0]; }
  static inline void    put_int2r(jint *from, jint *to, int& pos) { put_int2r(from, (jint *)((intptr_t *)to + pos)); pos += 2; }

public:
  // Ints are stored in native format in one JavaCallArgument slot at *to.
  static inline void    put_int(jint  from, intptr_t *to)               { *(jint *)(to +   0  ) =  from; }
  static inline void    put_int(jint  from, intptr_t *to, int& pos)     { *(jint *)(to + pos++) =  from; }
  static inline void    put_int(jint *from, intptr_t *to, int& pos)     { *(jint *)(to + pos++) = *from; }

  // Longs are stored in native format in one JavaCallArgument slot at *(to+1).
  static inline void    put_long(jlong  from, intptr_t *to)             { *(jlong *)(to + 1 +   0) =  from; }
  static inline void    put_long(jlong  from, intptr_t *to, int& pos)   { *(jlong *)(to + 1 + pos) =  from; pos += 2; }
  static inline void    put_long(jlong *from, intptr_t *to, int& pos)   { *(jlong *)(to + 1 + pos) = *from; pos += 2; }

  // Oops are stored in native format in one JavaCallArgument slot at *to.
  static inline void    put_obj(oop  from, intptr_t *to)                { *(oop *)(to +   0  ) =  from; }
  static inline void    put_obj(oop  from, intptr_t *to, int& pos)      { *(oop *)(to + pos++) =  from; }
  static inline void    put_obj(oop *from, intptr_t *to, int& pos)      { *(oop *)(to + pos++) = *from; }

  // Floats are stored in native format in one JavaCallArgument slot at *to.
  static inline void    put_float(jfloat  from, intptr_t *to)           { *(jfloat *)(to +   0  ) =  from; }
  static inline void    put_float(jfloat  from, intptr_t *to, int& pos) { *(jfloat *)(to + pos++) =  from; }
  static inline void    put_float(jfloat *from, intptr_t *to, int& pos) { *(jfloat *)(to + pos++) = *from; }

  // Doubles are stored in native word format in one JavaCallArgument slot at *(to+1).
  static inline void    put_double(jdouble  from, intptr_t *to)           { *(jdouble *)(to + 1 +   0) =  from; }
  static inline void    put_double(jdouble  from, intptr_t *to, int& pos) { *(jdouble *)(to + 1 + pos) =  from; pos += 2; }
  static inline void    put_double(jdouble *from, intptr_t *to, int& pos) { *(jdouble *)(to + 1 + pos) = *from; pos += 2; }

  // The get_xxx routines, on the other hand, actually _do_ fetch
  // java primitive types from the interpreter stack.
  static inline jint    get_int(intptr_t *from)         { return *(jint *)from; }

  static inline jlong   get_long(intptr_t *from)        { return *(jlong *)from; }

  static inline oop     get_obj(intptr_t *from)         { return *(oop *)from; }
  static inline jfloat  get_float(intptr_t *from)       { return *(jfloat *)from; }

  static inline jdouble get_double(intptr_t *from)      { return *(jdouble *)from; }
};

#endif
