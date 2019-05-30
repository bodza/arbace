#ifndef SHARE_VM_CLASSFILE_ALTHASHING_HPP
#define SHARE_VM_CLASSFILE_ALTHASHING_HPP

#include "jni.h"
#include "classfile/symbolTable.hpp"

/**
 * Hashing utilities.
 *
 * Implementation of Murmur3 hashing.
 * This code was translated from src/share/classes/sun/misc/Hashing.java
 * code in the JDK.
 */

class AltHashing : AllStatic {
  friend class AltHashingTest;

  // utility function copied from java/lang/Integer
  static juint Integer_rotateLeft(juint i, int distance) {
    return (i << distance) | (i >> (32 - distance));
  }
  static juint murmur3_32(const jint* data, int len);
  static juint murmur3_32(juint seed, const jint* data, int len);

 public:
  static juint compute_seed();
  static juint murmur3_32(juint seed, const jbyte* data, int len);
  static juint murmur3_32(juint seed, const jchar* data, int len);
};
#endif
