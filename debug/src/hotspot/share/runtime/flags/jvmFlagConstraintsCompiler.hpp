#ifndef SHARE_VM_RUNTIME_JVMFLAGCONSTRAINTSCOMPILER_HPP
#define SHARE_VM_RUNTIME_JVMFLAGCONSTRAINTSCOMPILER_HPP

#include "runtime/flags/jvmFlag.hpp"

/*
 * Here we have compiler arguments constraints functions, which are called automatically
 * whenever flag's value changes. If the constraint fails the function should return
 * an appropriate error value.
 */

JVMFlag::Error AliasLevelConstraintFunc(intx value, bool verbose);
JVMFlag::Error CICompilerCountConstraintFunc(intx value, bool verbose);
JVMFlag::Error AllocatePrefetchDistanceConstraintFunc(intx value, bool verbose);
JVMFlag::Error AllocatePrefetchInstrConstraintFunc(intx value, bool verbose);
JVMFlag::Error AllocatePrefetchStepSizeConstraintFunc(intx value, bool verbose);
JVMFlag::Error CompileThresholdConstraintFunc(intx value, bool verbose);
JVMFlag::Error CodeCacheSegmentSizeConstraintFunc(uintx value, bool verbose);
JVMFlag::Error CompilerThreadPriorityConstraintFunc(intx value, bool verbose);
JVMFlag::Error CodeEntryAlignmentConstraintFunc(intx value, bool verbose);
JVMFlag::Error OptoLoopAlignmentConstraintFunc(intx value, bool verbose);
JVMFlag::Error ArraycopyDstPrefetchDistanceConstraintFunc(uintx value, bool verbose);
JVMFlag::Error ArraycopySrcPrefetchDistanceConstraintFunc(uintx value, bool verbose);
JVMFlag::Error InitArrayShortSizeConstraintFunc(intx value, bool verbose);

#endif
