#ifndef SHARE_RUNTIME_JNIHANDLES_INLINE_HPP
#define SHARE_RUNTIME_JNIHANDLES_INLINE_HPP

#include "oops/access.inline.hpp"
#include "oops/oop.hpp"
#include "runtime/jniHandles.hpp"
#include "utilities/debug.hpp"
#include "utilities/globalDefinitions.hpp"

inline bool JNIHandles::is_jweak(jobject handle) {
  STATIC_ASSERT(weak_tag_size == 1);
  STATIC_ASSERT(weak_tag_value == 1);
  return (reinterpret_cast<uintptr_t>(handle) & weak_tag_mask) != 0;
}

inline oop* JNIHandles::jobject_ptr(jobject handle) {
  assert(!is_jweak(handle), "precondition");
  return reinterpret_cast<oop*>(handle);
}

inline oop* JNIHandles::jweak_ptr(jobject handle) {
  assert(is_jweak(handle), "precondition");
  char* ptr = reinterpret_cast<char*>(handle) - weak_tag_value;
  return reinterpret_cast<oop*>(ptr);
}

// external_guard is true if called from resolve_external_guard.
template<bool external_guard>
inline oop JNIHandles::resolve_impl(jobject handle) {
  assert(handle != NULL, "precondition");
  assert(!current_thread_in_native(), "must not be in native");
  oop result;
  if (is_jweak(handle)) {       // Unlikely
    result = resolve_jweak(handle);
  } else {
    result = NativeAccess<>::oop_load(jobject_ptr(handle));
    // Construction of jobjects canonicalize a null value into a null
    // jobject, so for non-jweak the pointee should never be null.
    assert(external_guard || result != NULL, "Invalid JNI handle");
  }
  return result;
}

inline oop JNIHandles::resolve(jobject handle) {
  oop result = NULL;
  if (handle != NULL) {
    result = resolve_impl<false /* external_guard */ >(handle);
  }
  return result;
}

inline oop JNIHandles::resolve_non_null(jobject handle) {
  assert(handle != NULL, "JNI handle should not be null");
  oop result = resolve_impl<false /* external_guard */ >(handle);
  assert(result != NULL, "NULL read from jni handle");
  return result;
}

inline void JNIHandles::destroy_local(jobject handle) {
  if (handle != NULL) {
    assert(!is_jweak(handle), "Invalid JNI local handle");
    NativeAccess<>::oop_store(jobject_ptr(handle), (oop)NULL);
  }
}

#endif
