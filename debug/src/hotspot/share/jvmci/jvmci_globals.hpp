#ifndef SHARE_VM_JVMCI_JVMCIGLOBALS_HPP
#define SHARE_VM_JVMCI_JVMCIGLOBALS_HPP

#include "runtime/globals.hpp"

//
// Defines all global flags used by the JVMCI compiler. Only flags that need
// to be accessible to the JVMCI C++ code should be defined here.
//
#define JVMCI_FLAGS(develop, \
                    develop_pd, \
                    product, \
                    product_pd, \
                    diagnostic, \
                    diagnostic_pd, \
                    experimental, \
                    notproduct, \
                    range, \
                    constraint, \
                    writeable) \
 \
  experimental(bool, EnableJVMCI, false, \
          "Enable JVMCI") \
 \
  experimental(bool, UseJVMCICompiler, false, \
          "Use JVMCI as the default compiler") \
 \
  experimental(bool, JVMCIPrintProperties, false, \
          "Prints properties used by the JVMCI compiler and exits") \
 \
  experimental(bool, BootstrapJVMCI, false, \
          "Bootstrap JVMCI before running Java main method") \
 \
  experimental(bool, EagerJVMCI, false, \
          "Force eager JVMCI initialization") \
 \
  experimental(bool, PrintBootstrap, true, \
          "Print JVMCI bootstrap progress and summary") \
 \
  experimental(intx, JVMCIThreads, 1, \
          "Force number of JVMCI compiler threads to use") \
          range(1, max_jint) \
 \
  experimental(intx, JVMCIHostThreads, 1, \
          "Force number of compiler threads for JVMCI host compiler") \
          range(1, max_jint) \
 \
  product(intx, MaxVectorSize, 64, \
          "Max vector size in bytes, actual size could be less depending on elements type") \
 \
  product(bool, ReduceInitialCardMarks, true, \
          "Defer write barriers of young objects") \
 \
  experimental(intx, JVMCITraceLevel, 0, \
          "Trace level for JVMCI: " \
          "1 means emit a message for each CompilerToVM call," \
          "levels greater than 1 provide progressively greater detail") \
 \
  experimental(intx, JVMCICounterSize, 0, \
          "Reserved size for benchmark counters") \
          range(0, max_jint) \
 \
  experimental(bool, JVMCICountersExcludeCompiler, true, \
          "Exclude JVMCI compiler threads from benchmark counters") \
 \
  develop(bool, JVMCIUseFastLocking, true, \
          "Use fast inlined locking code") \
 \
  experimental(intx, JVMCINMethodSizeLimit, (80*K)*wordSize, \
          "Maximum size of a compiled method.") \
 \
  experimental(intx, MethodProfileWidth, 0, \
          "Number of methods to record in call profile") \
 \
  develop(bool, TraceUncollectedSpeculations, false, \
          "Print message when a failed speculation was not collected")

// Read default values for JVMCI globals

JVMCI_FLAGS(DECLARE_DEVELOPER_FLAG, \
            DECLARE_PD_DEVELOPER_FLAG, \
            DECLARE_PRODUCT_FLAG, \
            DECLARE_PD_PRODUCT_FLAG, \
            DECLARE_DIAGNOSTIC_FLAG, \
            DECLARE_PD_DIAGNOSTIC_FLAG, \
            DECLARE_EXPERIMENTAL_FLAG, \
            DECLARE_NOTPRODUCT_FLAG, \
            IGNORE_RANGE, \
            IGNORE_CONSTRAINT, \
            IGNORE_WRITEABLE)

class JVMCIGlobals {
 public:
  // Return true if jvmci flags are consistent. If not consistent,
  // an error message describing the inconsistency is printed before
  // returning false.
  static bool check_jvmci_flags_are_consistent();

  // Check and exit VM with error if selected GC is not supported by JVMCI.
  static void check_jvmci_supported_gc();
};
#endif
