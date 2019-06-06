#ifndef CPU_AARCH64_VM_C1_GLOBALS_AARCH64_HPP
#define CPU_AARCH64_VM_C1_GLOBALS_AARCH64_HPP

#include "utilities/globalDefinitions.hpp"
#include "utilities/macros.hpp"

// Sets the default values for platform dependent flags used by the client compiler.
// (see c1_globals.hpp)

define_pd_global(bool,  BackgroundCompilation,         true);
define_pd_global(bool,  UseTLAB,                       true);
define_pd_global(bool,  ResizeTLAB,                    true);
define_pd_global(bool,  InlineIntrinsics,              true);
define_pd_global(bool,  PreferInterpreterNativeStubs, false);
define_pd_global(bool,  UseOnStackReplacement,         true);
define_pd_global(intx,  CompileThreshold,              1500);

define_pd_global(intx,  OnStackReplacePercentage,       933);
define_pd_global(intx,  FreqInlineSize,                 325);
define_pd_global(intx,  NewSizeThreadIncrease,          4*K);
define_pd_global(intx,  InitialCodeCacheSize,         160*K);
define_pd_global(intx,  ReservedCodeCacheSize,         32*M);
define_pd_global(intx,  NonProfiledCodeHeapSize,       13*M);
define_pd_global(intx,  ProfiledCodeHeapSize,          14*M);
define_pd_global(intx,  NonNMethodCodeHeapSize,         5*M);
define_pd_global(intx,  CodeCacheExpansionSize,        32*K);
define_pd_global(uintx, CodeCacheMinBlockLength,          1);
define_pd_global(uintx, CodeCacheMinimumUseSpace,     400*K);
define_pd_global(uintx, MetaspaceSize,                 12*M);
define_pd_global(bool,  NeverActAsServerClassMachine,  true);
define_pd_global(uint64_t, MaxRAM,                   1ULL*G);
define_pd_global(bool,  CICompileOSR,                  true);
define_pd_global(bool,  UseTypeProfile,               false);
define_pd_global(bool,  RoundFPResults,                true);

define_pd_global(bool,  LIRFillDelaySlots,            false);
define_pd_global(bool,  OptimizeSinglePrecision,       true);
define_pd_global(bool,  CSEArrayLength,               false);
define_pd_global(bool,  TwoOperandLIRForm,            false);

#endif
