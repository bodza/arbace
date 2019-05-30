#ifndef SHARE_VM_PRIMS_UNSAFE_HPP
#define SHARE_VM_PRIMS_UNSAFE_HPP

#include "jni.h"

extern "C" {
  void JNICALL JVM_RegisterJDKInternalMiscUnsafeMethods(JNIEnv *env, jclass unsafecls);
}

jlong Unsafe_field_offset_to_byte_offset(jlong field_offset);

jlong Unsafe_field_offset_from_byte_offset(jlong byte_offset);

#endif
