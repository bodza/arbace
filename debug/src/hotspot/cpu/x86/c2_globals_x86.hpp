#ifndef CPU_X86_VM_C2_GLOBALS_X86_HPP
#define CPU_X86_VM_C2_GLOBALS_X86_HPP

#include "utilities/globalDefinitions.hpp"
#include "utilities/macros.hpp"

// Sets the default values for platform dependent flags used by the server compiler.
// (see c2_globals.hpp).  Alpha-sorted.
define_pd_global(bool, BackgroundCompilation,        true);
define_pd_global(bool, UseTLAB,                      true);
define_pd_global(bool, ResizeTLAB,                   true);
define_pd_global(bool, CICompileOSR,                 true);
define_pd_global(bool, InlineIntrinsics,             true);
define_pd_global(bool, PreferInterpreterNativeStubs, false);
define_pd_global(bool, UseOnStackReplacement,        true);
define_pd_global(intx, CompileThreshold,             10000);

define_pd_global(intx, OnStackReplacePercentage,     140);
define_pd_global(intx, ConditionalMoveLimit,         3);
define_pd_global(intx, FreqInlineSize,               325);
define_pd_global(intx, MinJumpTableSize,             10);
define_pd_global(intx, LoopPercentProfileLimit,      30);
#ifdef AMD64
define_pd_global(intx,  INTPRESSURE,                 13);
define_pd_global(intx,  FLOATPRESSURE,               14);
define_pd_global(intx,  InteriorEntryAlignment,      16);
define_pd_global(size_t, NewSizeThreadIncrease,     ScaleForWordSize(4*K));
define_pd_global(intx,  LoopUnrollLimit,             60);
// InitialCodeCacheSize derived from specjbb2000 run.
define_pd_global(uintx, InitialCodeCacheSize,        2496*K); // Integral multiple of CodeCacheExpansionSize
define_pd_global(uintx, CodeCacheExpansionSize,      64*K);

// Ergonomics related flags
define_pd_global(uint64_t, MaxRAM,                   128ULL*G);
#else
define_pd_global(intx,  INTPRESSURE,                 6);
define_pd_global(intx,  FLOATPRESSURE,               6);
define_pd_global(intx,  InteriorEntryAlignment,      4);
define_pd_global(size_t, NewSizeThreadIncrease,      4*K);
define_pd_global(intx,  LoopUnrollLimit,             50);     // Design center runs on 1.3.1
// InitialCodeCacheSize derived from specjbb2000 run.
define_pd_global(uintx, InitialCodeCacheSize,        2304*K); // Integral multiple of CodeCacheExpansionSize
define_pd_global(uintx, CodeCacheExpansionSize,      32*K);

// Ergonomics related flags
define_pd_global(uint64_t, MaxRAM,                   4ULL*G);
#endif
define_pd_global(intx, RegisterCostAreaRatio,        16000);

// Peephole and CISC spilling both break the graph, and so makes the
// scheduler sick.
define_pd_global(bool, OptoPeephole,                 true);
define_pd_global(bool, UseCISCSpill,                 true);
define_pd_global(bool, OptoScheduling,               false);
define_pd_global(bool, OptoBundling,                 false);
define_pd_global(bool, OptoRegScheduling,            true);
define_pd_global(bool, SuperWordLoopUnrollAnalysis,  true);
define_pd_global(bool, IdealizeClearArrayNode,       true);

define_pd_global(uintx, ReservedCodeCacheSize,       48*M);
define_pd_global(uintx, NonProfiledCodeHeapSize,     21*M);
define_pd_global(uintx, ProfiledCodeHeapSize,        22*M);
define_pd_global(uintx, NonNMethodCodeHeapSize,      5*M );
define_pd_global(uintx, CodeCacheMinBlockLength,     4);
define_pd_global(uintx, CodeCacheMinimumUseSpace,    400*K);

define_pd_global(bool,  TrapBasedRangeChecks,        false); // Not needed on x86.

// Heap related flags
define_pd_global(size_t, MetaspaceSize,              ScaleForWordSize(16*M));

// Ergonomics related flags
define_pd_global(bool, NeverActAsServerClassMachine, false);

#endif
