#ifndef SHARE_VM_PRIMS_JVM_MISC_HPP
#define SHARE_VM_PRIMS_JVM_MISC_HPP

#include "jni.h"
#include "runtime/handles.hpp"

// Useful entry points shared by JNI and JVM interface.
// We do not allow real JNI or JVM entry point to call each other.

jclass find_class_from_class_loader(JNIEnv* env, Symbol* name, jboolean init, Handle loader, Handle protection_domain, jboolean throwError, TRAPS);

void trace_class_resolution(Klass* to_class);

/*
 * Support for -Xcheck:jni
 */

extern struct JNINativeInterface_* jni_functions_nocheck();
extern struct JNINativeInterface_* jni_functions_check();

/*
 * Support for swappable jni function table.
 */
extern struct JNINativeInterface_* jni_functions();
extern void copy_jni_function_table(const struct JNINativeInterface_* new_function_table);

// Support for fast JNI accessors
extern "C" {
  typedef jboolean (JNICALL *GetBooleanField_t)
      (JNIEnv *env, jobject obj, jfieldID fieldID);
  typedef jbyte (JNICALL *GetByteField_t)
      (JNIEnv *env, jobject obj, jfieldID fieldID);
  typedef jchar (JNICALL *GetCharField_t)
      (JNIEnv *env, jobject obj, jfieldID fieldID);
  typedef jshort (JNICALL *GetShortField_t)
      (JNIEnv *env, jobject obj, jfieldID fieldID);
  typedef jint (JNICALL *GetIntField_t)
      (JNIEnv *env, jobject obj, jfieldID fieldID);
  typedef jlong (JNICALL *GetLongField_t)
      (JNIEnv *env, jobject obj, jfieldID fieldID);
  typedef jfloat (JNICALL *GetFloatField_t)
      (JNIEnv *env, jobject obj, jfieldID fieldID);
  typedef jdouble (JNICALL *GetDoubleField_t)
    (JNIEnv *env, jobject obj, jfieldID fieldID);
}

void    quicken_jni_functions();
address jni_GetBooleanField_addr();
address jni_GetByteField_addr();
address jni_GetCharField_addr();
address jni_GetShortField_addr();
address jni_GetIntField_addr();
address jni_GetLongField_addr();
address jni_GetFloatField_addr();
address jni_GetDoubleField_addr();

#endif
