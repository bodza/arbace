#include "precompiled.hpp"

#include "jvm.h"
#include "jvmci/jvmci_globals.hpp"
#include "gc/shared/gcConfig.hpp"
#include "utilities/defaultStream.hpp"
#include "runtime/globals_extension.hpp"

JVMCI_FLAGS(MATERIALIZE_DEVELOPER_FLAG, \
            MATERIALIZE_PD_DEVELOPER_FLAG, \
            MATERIALIZE_PRODUCT_FLAG, \
            MATERIALIZE_PD_PRODUCT_FLAG, \
            MATERIALIZE_DIAGNOSTIC_FLAG, \
            MATERIALIZE_PD_DIAGNOSTIC_FLAG, \
            MATERIALIZE_EXPERIMENTAL_FLAG, \
            MATERIALIZE_NOTPRODUCT_FLAG,
            IGNORE_RANGE, \
            IGNORE_CONSTRAINT, \
            IGNORE_WRITEABLE)

// Return true if jvmci flags are consistent.
bool JVMCIGlobals::check_jvmci_flags_are_consistent() {
#define JVMCI_FLAG_CHECKED(name)

  // Checks that a given flag is not set if a given guard flag is false.
#define CHECK_NOT_SET(FLAG, GUARD) \
  JVMCI_FLAG_CHECKED(FLAG) \
  if (!GUARD && !FLAG_IS_DEFAULT(FLAG)) { \
    jio_fprintf(defaultStream::error_stream(), "Improperly specified VM option '%s': '%s' must be enabled\n", #FLAG, #GUARD); \
    return false; \
  }

  JVMCI_FLAG_CHECKED(UseJVMCICompiler)
  JVMCI_FLAG_CHECKED(EnableJVMCI)

  CHECK_NOT_SET(BootstrapJVMCI,   UseJVMCICompiler)
  CHECK_NOT_SET(PrintBootstrap,   UseJVMCICompiler)
  CHECK_NOT_SET(JVMCIThreads,     UseJVMCICompiler)
  CHECK_NOT_SET(JVMCIHostThreads, UseJVMCICompiler)

  if (UseJVMCICompiler) {
    if (!FLAG_IS_DEFAULT(EnableJVMCI) && !EnableJVMCI) {
      jio_fprintf(defaultStream::error_stream(), "Improperly specified VM option UseJVMCICompiler: EnableJVMCI cannot be disabled\n");
      return false;
    }
    FLAG_SET_DEFAULT(EnableJVMCI, true);
  }

  if (!EnableJVMCI) {
    // Switch off eager JVMCI initialization if JVMCI is disabled.
    // Don't throw error if EagerJVMCI is set to allow testing.
    if (EagerJVMCI) {
      FLAG_SET_DEFAULT(EagerJVMCI, false);
    }
  }
  JVMCI_FLAG_CHECKED(EagerJVMCI)

  CHECK_NOT_SET(JVMCITraceLevel,              EnableJVMCI)
  CHECK_NOT_SET(JVMCICounterSize,             EnableJVMCI)
  CHECK_NOT_SET(JVMCICountersExcludeCompiler, EnableJVMCI)
  CHECK_NOT_SET(JVMCIUseFastLocking,          EnableJVMCI)
  CHECK_NOT_SET(JVMCINMethodSizeLimit,        EnableJVMCI)
  CHECK_NOT_SET(MethodProfileWidth,           EnableJVMCI)
  CHECK_NOT_SET(JVMCIPrintProperties,         EnableJVMCI)
  CHECK_NOT_SET(TraceUncollectedSpeculations, EnableJVMCI)

#undef CHECK_NOT_SET
  return true;
}
void JVMCIGlobals::check_jvmci_supported_gc() {
  if (EnableJVMCI) {
    // Check if selected GC is supported by JVMCI and Java compiler
    if (!(UseSerialGC || UseParallelGC || UseParallelOldGC || UseG1GC)) {
      vm_exit_during_initialization("JVMCI Compiler does not support selected GC", GCConfig::hs_err_name());
      FLAG_SET_DEFAULT(EnableJVMCI, false);
      FLAG_SET_DEFAULT(UseJVMCICompiler, false);
    }
  }
}
