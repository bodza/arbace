#ifndef SHARE_VM_RUNTIME_JVMFLAGCONSTRAINTSRUNTIME_HPP
#define SHARE_VM_RUNTIME_JVMFLAGCONSTRAINTSRUNTIME_HPP

#include "runtime/flags/jvmFlag.hpp"

/*
 * Here we have runtime arguments constraints functions, which are called automatically
 * whenever flag's value changes. If the constraint fails the function should return
 * an appropriate error value.
 */

JVMFlag::Error ObjectAlignmentInBytesConstraintFunc(intx value, bool verbose);

JVMFlag::Error ContendedPaddingWidthConstraintFunc(intx value, bool verbose);

JVMFlag::Error BiasedLockingBulkRebiasThresholdFunc(intx value, bool verbose);
JVMFlag::Error BiasedLockingStartupDelayFunc(intx value, bool verbose);
JVMFlag::Error BiasedLockingBulkRevokeThresholdFunc(intx value, bool verbose);
JVMFlag::Error BiasedLockingDecayTimeFunc(intx value, bool verbose);

JVMFlag::Error PerfDataSamplingIntervalFunc(intx value, bool verbose);

JVMFlag::Error ThreadLocalHandshakesConstraintFunc(bool value, bool verbose);

#endif
