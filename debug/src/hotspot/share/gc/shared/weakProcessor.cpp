#include "precompiled.hpp"

#include "classfile/systemDictionary.hpp"
#include "gc/shared/oopStorage.inline.hpp"
#include "gc/shared/weakProcessor.hpp"
#include "runtime/jniHandles.hpp"
#include "utilities/macros.hpp"

void WeakProcessor::weak_oops_do(BoolObjectClosure* is_alive, OopClosure* keep_alive) {
  JNIHandles::weak_oops_do(is_alive, keep_alive);
  SystemDictionary::vm_weak_oop_storage()->weak_oops_do(is_alive, keep_alive);
}

void WeakProcessor::oops_do(OopClosure* closure) {
  AlwaysTrueClosure always_true;
  weak_oops_do(&always_true, closure);
}
