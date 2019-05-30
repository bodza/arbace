#include "precompiled.hpp"
#include "runtime/arguments.hpp"
#include "runtime/flags/jvmFlag.hpp"
#include "runtime/flags/jvmFlagConstraintsRuntime.hpp"
#include "runtime/globals.hpp"
#include "runtime/safepointMechanism.hpp"
#include "runtime/task.hpp"

JVMFlag::Error ObjectAlignmentInBytesConstraintFunc(intx value, bool verbose) {
  if (!is_power_of_2(value)) {
    JVMFlag::printError(verbose,
                        "ObjectAlignmentInBytes (" INTX_FORMAT ") must be "
                        "power of 2\n",
                        value);
    return JVMFlag::VIOLATES_CONSTRAINT;
  }
  // In case page size is very small.
  if (value >= (intx)os::vm_page_size()) {
    JVMFlag::printError(verbose,
                        "ObjectAlignmentInBytes (" INTX_FORMAT ") must be "
                        "less than page size (" INTX_FORMAT ")\n",
                        value, (intx)os::vm_page_size());
    return JVMFlag::VIOLATES_CONSTRAINT;
  }
  return JVMFlag::SUCCESS;
}

// Need to enforce the padding not to break the existing field alignments.
// It is sufficient to check against the largest type size.
JVMFlag::Error ContendedPaddingWidthConstraintFunc(intx value, bool verbose) {
  if ((value % BytesPerLong) != 0) {
    JVMFlag::printError(verbose,
                        "ContendedPaddingWidth (" INTX_FORMAT ") must be "
                        "a multiple of %d\n",
                        value, BytesPerLong);
    return JVMFlag::VIOLATES_CONSTRAINT;
  } else {
    return JVMFlag::SUCCESS;
  }
}

JVMFlag::Error BiasedLockingBulkRebiasThresholdFunc(intx value, bool verbose) {
  if (value > BiasedLockingBulkRevokeThreshold) {
    JVMFlag::printError(verbose,
                        "BiasedLockingBulkRebiasThreshold (" INTX_FORMAT ") must be "
                        "less than or equal to BiasedLockingBulkRevokeThreshold (" INTX_FORMAT ")\n",
                        value, BiasedLockingBulkRevokeThreshold);
    return JVMFlag::VIOLATES_CONSTRAINT;
  } else {
    return JVMFlag::SUCCESS;
  }
}

JVMFlag::Error BiasedLockingStartupDelayFunc(intx value, bool verbose) {
  if ((value % PeriodicTask::interval_gran) != 0) {
    JVMFlag::printError(verbose,
                        "BiasedLockingStartupDelay (" INTX_FORMAT ") must be "
                        "evenly divisible by PeriodicTask::interval_gran (" INTX_FORMAT ")\n",
                        value, PeriodicTask::interval_gran);
    return JVMFlag::VIOLATES_CONSTRAINT;
  } else {
    return JVMFlag::SUCCESS;
  }
}

JVMFlag::Error BiasedLockingBulkRevokeThresholdFunc(intx value, bool verbose) {
  if (value < BiasedLockingBulkRebiasThreshold) {
    JVMFlag::printError(verbose,
                        "BiasedLockingBulkRevokeThreshold (" INTX_FORMAT ") must be "
                        "greater than or equal to BiasedLockingBulkRebiasThreshold (" INTX_FORMAT ")\n",
                        value, BiasedLockingBulkRebiasThreshold);
    return JVMFlag::VIOLATES_CONSTRAINT;
  } else if ((double)value/(double)BiasedLockingDecayTime > 0.1) {
    JVMFlag::printError(verbose,
                        "The ratio of BiasedLockingBulkRevokeThreshold (" INTX_FORMAT ")"
                        " to BiasedLockingDecayTime (" INTX_FORMAT ") must be "
                        "less than or equal to 0.1\n",
                        value, BiasedLockingBulkRebiasThreshold);
    return JVMFlag::VIOLATES_CONSTRAINT;
  } else {
    return JVMFlag::SUCCESS;
  }
}

JVMFlag::Error BiasedLockingDecayTimeFunc(intx value, bool verbose) {
  if (BiasedLockingBulkRebiasThreshold/(double)value > 0.1) {
    JVMFlag::printError(verbose,
                        "The ratio of BiasedLockingBulkRebiasThreshold (" INTX_FORMAT ")"
                        " to BiasedLockingDecayTime (" INTX_FORMAT ") must be "
                        "less than or equal to 0.1\n",
                        BiasedLockingBulkRebiasThreshold, value);
    return JVMFlag::VIOLATES_CONSTRAINT;
  } else {
    return JVMFlag::SUCCESS;
  }
}

JVMFlag::Error PerfDataSamplingIntervalFunc(intx value, bool verbose) {
  if ((value % PeriodicTask::interval_gran != 0)) {
    JVMFlag::printError(verbose,
                        "PerfDataSamplingInterval (" INTX_FORMAT ") must be "
                        "evenly divisible by PeriodicTask::interval_gran (" INTX_FORMAT ")\n",
                        value, PeriodicTask::interval_gran);
    return JVMFlag::VIOLATES_CONSTRAINT;
  } else {
    return JVMFlag::SUCCESS;
  }
}

JVMFlag::Error ThreadLocalHandshakesConstraintFunc(bool value, bool verbose) {
  if (value) {
    if (!SafepointMechanism::supports_thread_local_poll()) {
      JVMFlag::printError(verbose, "ThreadLocalHandshakes not yet supported on this platform\n");
      return JVMFlag::VIOLATES_CONSTRAINT;
    }
  }
  return JVMFlag::SUCCESS;
}
