#ifndef CPU_AARCH64_VM_C2_GLOBALS_AARCH64_HPP
#define CPU_AARCH64_VM_C2_GLOBALS_AARCH64_HPP

#include "utilities/globalDefinitions.hpp"
#include "utilities/macros.hpp"

// Sets the default values for platform dependent flags used by the server compiler.
// (see c2_globals.hpp).  Alpha-sorted.

define_pd_global(bool, BackgroundCompilation,        true);
define_pd_global(bool, UseTLAB,                      true);
define_pd_global(bool, ResizeTLAB,                   true);
define_pd_global(bool, InlineIntrinsics,             true);
define_pd_global(intx, CompileThreshold,             10000);

define_pd_global(intx, OnStackReplacePercentage,     140);
define_pd_global(intx, ConditionalMoveLimit,         3);
define_pd_global(intx, FLOATPRESSURE,                64);
define_pd_global(intx, FreqInlineSize,               325);
define_pd_global(intx, MinJumpTableSize,             10);
define_pd_global(intx, INTPRESSURE,                  24);
define_pd_global(intx, InteriorEntryAlignment,       16);
define_pd_global(intx, NewSizeThreadIncrease, ScaleForWordSize(4*K));
define_pd_global(intx, LoopUnrollLimit,              60);
define_pd_global(intx, LoopPercentProfileLimit,      10);
// InitialCodeCacheSize derived from specjbb2000 run.
define_pd_global(intx, InitialCodeCacheSize,         2496*K); // Integral multiple of CodeCacheExpansionSize
define_pd_global(intx, CodeCacheExpansionSize,       64*K);

// Ergonomics related flags
define_pd_global(uint64_t,MaxRAM,                    128ULL*G);
define_pd_global(intx, RegisterCostAreaRatio,        16000);

// Peephole and CISC spilling both break the graph, and so makes the
// scheduler sick.
define_pd_global(bool, OptoPeephole,                 false);
define_pd_global(bool, UseCISCSpill,                 false);
define_pd_global(bool, OptoScheduling,               false);
define_pd_global(bool, OptoBundling,                 false);
define_pd_global(bool, OptoRegScheduling,            false);
define_pd_global(bool, SuperWordLoopUnrollAnalysis,  true);
define_pd_global(bool, IdealizeClearArrayNode,       true);

define_pd_global(intx, ReservedCodeCacheSize,        48*M);
define_pd_global(intx, NonProfiledCodeHeapSize,      21*M);
define_pd_global(intx, ProfiledCodeHeapSize,         22*M);
define_pd_global(intx, NonNMethodCodeHeapSize,       5*M );
define_pd_global(uintx, CodeCacheMinBlockLength,     4);
define_pd_global(uintx, CodeCacheMinimumUseSpace,    400*K);

// Heap related flags
define_pd_global(uintx,MetaspaceSize,    ScaleForWordSize(16*M));

// Ergonomics related flags
define_pd_global(bool, NeverActAsServerClassMachine, false);

define_pd_global(bool,  TrapBasedRangeChecks,        false); // Not needed.

#endif
