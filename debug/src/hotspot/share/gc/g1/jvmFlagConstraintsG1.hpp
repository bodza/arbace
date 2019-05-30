#ifndef SHARE_GC_G1_COMMANDLINEFLAGCONSTRAINTSG1_HPP
#define SHARE_GC_G1_COMMANDLINEFLAGCONSTRAINTSG1_HPP

#include "runtime/globals.hpp"
#include "utilities/globalDefinitions.hpp"

// G1 Flag Constraints
JVMFlag::Error G1RSetRegionEntriesConstraintFunc(intx value, bool verbose);
JVMFlag::Error G1RSetSparseRegionEntriesConstraintFunc(intx value, bool verbose);
JVMFlag::Error G1HeapRegionSizeConstraintFunc(size_t value, bool verbose);
JVMFlag::Error G1NewSizePercentConstraintFunc(uintx value, bool verbose);
JVMFlag::Error G1MaxNewSizePercentConstraintFunc(uintx value, bool verbose);

// G1 Subconstraints
JVMFlag::Error MaxGCPauseMillisConstraintFuncG1(uintx value, bool verbose);
JVMFlag::Error GCPauseIntervalMillisConstraintFuncG1(uintx value, bool verbose);
JVMFlag::Error MaxSizeForHeapAlignmentG1(const char* name, size_t value, bool verbose);
JVMFlag::Error NewSizeConstraintFuncG1(size_t value, bool verbose);

size_t MaxSizeForHeapAlignmentG1();

#endif
