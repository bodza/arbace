#ifndef SHARE_GC_SHARED_COMMANDLINEFLAGCONSTRAINTSGC_HPP
#define SHARE_GC_SHARED_COMMANDLINEFLAGCONSTRAINTSGC_HPP

#include "utilities/globalDefinitions.hpp"
#include "utilities/macros.hpp"
#include "gc/g1/jvmFlagConstraintsG1.hpp"

/*
 * Here we have GC arguments constraints functions, which are called automatically
 * whenever flag's value changes. If the constraint fails the function should return
 * an appropriate error value.
 */

JVMFlag::Error ParallelGCThreadsConstraintFunc(uint value, bool verbose);
JVMFlag::Error ConcGCThreadsConstraintFunc(uint value, bool verbose);
JVMFlag::Error YoungPLABSizeConstraintFunc(size_t value, bool verbose);
JVMFlag::Error OldPLABSizeConstraintFunc(size_t value, bool verbose);
JVMFlag::Error MinHeapFreeRatioConstraintFunc(uintx value, bool verbose);
JVMFlag::Error MaxHeapFreeRatioConstraintFunc(uintx value, bool verbose);
JVMFlag::Error SoftRefLRUPolicyMSPerMBConstraintFunc(intx value, bool verbose);
JVMFlag::Error MarkStackSizeConstraintFunc(size_t value, bool verbose);
JVMFlag::Error MinMetaspaceFreeRatioConstraintFunc(uintx value, bool verbose);
JVMFlag::Error MaxMetaspaceFreeRatioConstraintFunc(uintx value, bool verbose);
JVMFlag::Error InitialTenuringThresholdConstraintFunc(uintx value, bool verbose);
JVMFlag::Error MaxTenuringThresholdConstraintFunc(uintx value, bool verbose);

JVMFlag::Error MaxGCPauseMillisConstraintFunc(uintx value, bool verbose);
JVMFlag::Error GCPauseIntervalMillisConstraintFunc(uintx value, bool verbose);
JVMFlag::Error InitialBootClassLoaderMetaspaceSizeConstraintFunc(size_t value, bool verbose);
JVMFlag::Error InitialHeapSizeConstraintFunc(size_t value, bool verbose);
JVMFlag::Error MaxHeapSizeConstraintFunc(size_t value, bool verbose);
JVMFlag::Error HeapBaseMinAddressConstraintFunc(size_t value, bool verbose);
JVMFlag::Error NewSizeConstraintFunc(size_t value, bool verbose);
JVMFlag::Error MinTLABSizeConstraintFunc(size_t value, bool verbose);
JVMFlag::Error TLABSizeConstraintFunc(size_t value, bool verbose);
JVMFlag::Error TLABWasteIncrementConstraintFunc(uintx value, bool verbose);
JVMFlag::Error SurvivorRatioConstraintFunc(uintx value, bool verbose);
JVMFlag::Error MetaspaceSizeConstraintFunc(size_t value, bool verbose);
JVMFlag::Error MaxMetaspaceSizeConstraintFunc(size_t value, bool verbose);
JVMFlag::Error SurvivorAlignmentInBytesConstraintFunc(intx value, bool verbose);

// Internal
JVMFlag::Error MaxPLABSizeBounds(const char* name, size_t value, bool verbose);

#endif
