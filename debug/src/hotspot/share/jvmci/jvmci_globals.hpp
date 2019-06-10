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
  experimental(bool, EnableJVMCI, false, "Enable JVMCI") \
  experimental(bool, EagerJVMCI, false, "Force eager JVMCI initialization") \
  product(intx, MaxVectorSize, 64, "Max vector size in bytes, actual size could be less depending on elements type") \
  product(bool, ReduceInitialCardMarks, true, "Defer write barriers of young objects") \
  develop(bool, JVMCIUseFastLocking, true, "Use fast inlined locking code") \
  experimental(intx, JVMCINMethodSizeLimit, (80*K)*wordSize, "Maximum size of a compiled method.")

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
