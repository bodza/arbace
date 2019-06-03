#include "precompiled.hpp"

#include "memory/universe.hpp"
#include "oops/oop.inline.hpp"
#include "runtime/arguments.hpp"
#include "runtime/vm_version.hpp"

const char* Abstract_VM_Version::_s_vm_release = Abstract_VM_Version::vm_release();
const char* Abstract_VM_Version::_s_internal_vm_info_string = Abstract_VM_Version::internal_vm_info_string();

uint64_t Abstract_VM_Version::_features = 0;
const char* Abstract_VM_Version::_features_string = "";

bool Abstract_VM_Version::_supports_cx8 = false;
bool Abstract_VM_Version::_supports_atomic_getset4 = false;
bool Abstract_VM_Version::_supports_atomic_getset8 = false;
bool Abstract_VM_Version::_supports_atomic_getadd4 = false;
bool Abstract_VM_Version::_supports_atomic_getadd8 = false;
unsigned int Abstract_VM_Version::_logical_processors_per_package = 1U;
unsigned int Abstract_VM_Version::_L1_data_cache_line_size = 0;

#ifndef HOTSPOT_VERSION_STRING
  #error HOTSPOT_VERSION_STRING must be defined
#endif

#ifndef VERSION_FEATURE
  #error VERSION_FEATURE must be defined
#endif
#ifndef VERSION_INTERIM
  #error VERSION_INTERIM must be defined
#endif
#ifndef VERSION_UPDATE
  #error VERSION_UPDATE must be defined
#endif
#ifndef VERSION_PATCH
  #error VERSION_PATCH must be defined
#endif
#ifndef VERSION_BUILD
  #error VERSION_BUILD must be defined
#endif

#ifndef VERSION_STRING
  #error VERSION_STRING must be defined
#endif

#ifndef DEBUG_LEVEL
  #error DEBUG_LEVEL must be defined
#endif

#define VM_RELEASE HOTSPOT_VERSION_STRING

// HOTSPOT_VERSION_STRING equals the JDK VERSION_STRING (unless overridden
// in a standalone build).
int Abstract_VM_Version::_vm_major_version = VERSION_FEATURE;
int Abstract_VM_Version::_vm_minor_version = VERSION_INTERIM;
int Abstract_VM_Version::_vm_security_version = VERSION_UPDATE;
int Abstract_VM_Version::_vm_patch_version = VERSION_PATCH;
int Abstract_VM_Version::_vm_build_number = VERSION_BUILD;
unsigned int Abstract_VM_Version::_parallel_worker_threads = 0;
bool Abstract_VM_Version::_parallel_worker_threads_initialized = false;

#define VMLP "64-Bit "

#ifndef VMTYPE
  #define VMTYPE "Client"
#endif

#ifndef HOTSPOT_VM_DISTRO
  #error HOTSPOT_VM_DISTRO must be defined
#endif
#define VMNAME HOTSPOT_VM_DISTRO " " VMLP VMTYPE " VM"

const char* Abstract_VM_Version::vm_name() {
  return VMNAME;
}

const char* Abstract_VM_Version::vm_vendor() {
#ifdef VENDOR
  return VENDOR;
#else
  return "Oracle Corporation";
#endif
}

const char* Abstract_VM_Version::vm_info_string() {
  switch (Arguments::mode()) {
    case Arguments::_int:
      return "interpreted mode";
    case Arguments::_mixed:
      {
        if (UseAOT) {
          return "mixed mode, aot";
        } else {
          return "mixed mode";
        }
      }
    case Arguments::_comp:
      return "compiled mode";
  };
  ShouldNotReachHere();
  return "";
}

// NOTE: do *not* use stringStream. this function is called by
//       fatal error handler. if the crash is in native thread,
//       stringStream cannot get resource allocated and will SEGV.
const char* Abstract_VM_Version::vm_release() {
  return VM_RELEASE;
}

// NOTE: do *not* use stringStream. this function is called by
//       fatal error handlers. if the crash is in native thread,
//       stringStream cannot get resource allocated and will SEGV.
const char* Abstract_VM_Version::jre_release_version() {
  return VERSION_STRING;
}

#define OS       LINUX_ONLY("linux") \
                 BSD_ONLY("bsd")

#ifndef CPU
#define CPU      AARCH64_ONLY("aarch64") \
                 AMD64_ONLY("amd64") \
                 IA32_ONLY("x86") \
                 IA64_ONLY("ia64")
#endif

const char *Abstract_VM_Version::vm_platform_string() {
  return OS "-" CPU;
}

const char* Abstract_VM_Version::internal_vm_info_string() {
  #ifndef HOTSPOT_BUILD_USER
    #define HOTSPOT_BUILD_USER unknown
  #endif

  #ifndef HOTSPOT_BUILD_COMPILER
    #if defined(__GNUC__)
        #define HOTSPOT_BUILD_COMPILER "gcc " __VERSION__
    #else
      #define HOTSPOT_BUILD_COMPILER "unknown compiler"
    #endif
  #endif

  #ifndef FLOAT_ARCH
    #if defined(__SOFTFP__)
      #define FLOAT_ARCH_STR "-sflt"
    #else
      #define FLOAT_ARCH_STR ""
    #endif
  #else
    #define FLOAT_ARCH_STR XSTR(FLOAT_ARCH)
  #endif

  #define INTERNAL_VERSION_SUFFIX VM_RELEASE ")" \
         " for " OS "-" CPU FLOAT_ARCH_STR \
         " JRE (" VERSION_STRING "), built on " __DATE__ " " __TIME__ \
         " by " XSTR(HOTSPOT_BUILD_USER) " with " HOTSPOT_BUILD_COMPILER

  return strcmp(DEBUG_LEVEL, "release") == 0 ? VMNAME " (" INTERNAL_VERSION_SUFFIX : VMNAME " (" DEBUG_LEVEL " " INTERNAL_VERSION_SUFFIX;
}

const char *Abstract_VM_Version::vm_build_user() {
  return HOTSPOT_BUILD_USER;
}

const char *Abstract_VM_Version::jdk_debug_level() {
  return DEBUG_LEVEL;
}

const char *Abstract_VM_Version::printable_jdk_debug_level() {
  // Debug level is not printed for "release" builds
  return strcmp(DEBUG_LEVEL, "release") == 0 ? "" : DEBUG_LEVEL " ";
}

unsigned int Abstract_VM_Version::jvm_version() {
  return ((Abstract_VM_Version::vm_major_version() & 0xFF) << 24) |
         ((Abstract_VM_Version::vm_minor_version() & 0xFF) << 16) |
         ((Abstract_VM_Version::vm_security_version() & 0xFF) << 8) |
         (Abstract_VM_Version::vm_build_number() & 0xFF);
}

void VM_Version_init() {
  VM_Version::initialize();
}

unsigned int Abstract_VM_Version::nof_parallel_worker_threads(unsigned int num, unsigned int den, unsigned int switch_pt) {
  if (FLAG_IS_DEFAULT(ParallelGCThreads)) {
    unsigned int threads;
    // For very large machines, there are diminishing returns
    // for large numbers of worker threads.  Instead of
    // hogging the whole system, use a fraction of the workers for every
    // processor after the first 8.  For example, on a 72 cpu machine
    // and a chosen fraction of 5/8
    // use 8 + (72 - 8) * (5/8) == 48 worker threads.
    unsigned int ncpus = (unsigned int) os::initial_active_processor_count();
    threads = (ncpus <= switch_pt) ?
             ncpus :
             (switch_pt + ((ncpus - switch_pt) * num) / den);
    return threads;
  } else {
    return ParallelGCThreads;
  }
}

unsigned int Abstract_VM_Version::calc_parallel_worker_threads() {
  return nof_parallel_worker_threads(5, 8, 8);
}

// Does not set the _initialized flag since it is
// a global flag.
unsigned int Abstract_VM_Version::parallel_worker_threads() {
  if (!_parallel_worker_threads_initialized) {
    if (FLAG_IS_DEFAULT(ParallelGCThreads)) {
      _parallel_worker_threads = VM_Version::calc_parallel_worker_threads();
    } else {
      _parallel_worker_threads = ParallelGCThreads;
    }
    _parallel_worker_threads_initialized = true;
  }
  return _parallel_worker_threads;
}
