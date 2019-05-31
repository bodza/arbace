#ifndef SHARE_VM_AOT_AOTLOADER_HPP
#define SHARE_VM_AOT_AOTLOADER_HPP

#include "runtime/globals_extension.hpp"
#include "runtime/handles.hpp"

class AOTCodeHeap;
class AOTCompiledMethod;
class AOTLib;
class CodeBlob;
template <class T> class GrowableArray;
class InstanceKlass;
class JavaThread;
class Metadata;
class OopClosure;

class AOTLoader {
private:
  static void load_library(const char* name, bool exit_on_error);

public:
  static void initialize() { FLAG_SET_ERGO(bool, UseAOT, false); };

  static void universe_init() { };
  static void set_narrow_oop_shift() { };
  static void set_narrow_klass_shift() { };
  static bool contains(address p) { return false; };
  static void load_for_klass(InstanceKlass* ik, Thread* thread) { };
  static bool find_klass(InstanceKlass* ik) { return false; };
  static uint64_t get_saved_fingerprint(InstanceKlass* ik) { return 0; };
  static void oops_do(OopClosure* f) { };
  static void metadata_do(void f(Metadata*)) { };

  static bool reconcile_dynamic_invoke(InstanceKlass* holder, int index, Method* adapter_method, Klass *appendix_klass) { return true; };
};

#endif
