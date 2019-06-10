#ifndef SHARE_VM_C1_C1_GLOBALS_HPP
#define SHARE_VM_C1_C1_GLOBALS_HPP

#include "runtime/globals.hpp"
#include "utilities/macros.hpp"

#include CPU_HEADER(c1_globals)
#include OS_HEADER(c1_globals)

//
// Defines all global flags used by the client compiler.
//
#define C1_FLAGS(develop, \
                 develop_pd, \
                 product, \
                 product_pd, \
                 diagnostic, \
                 diagnostic_pd, \
                 notproduct, \
                 range, \
                 constraint, \
                 writeable) \
 \
  /* C1 optimizations */ \
 \
  develop(bool,    UseC1Optimizations,          true, "Turn on C1 optimizations") \
  develop(bool,    SelectivePhiFunctions,       true, "create phi functions at loop headers only when necessary") \
  develop(bool,    OptimizeIfOps,               true, "Optimize multiple IfOps") \
  develop(bool,    DoCEE,                       true, "Do Conditional Expression Elimination to simplify CFG") \
  develop(bool,    UseLocalValueNumbering,      true, "Use Local Value Numbering (embedded in GraphBuilder)") \
  product(bool,    UseLoopInvariantCodeMotion,  true, "Simple loop invariant code motion for short loops during GVN") \
  product(intx,    ValueMapInitialSize,           11, "Initial size of a value map") range(1, 32*K) \
  product(intx,    ValueMapMaxLoopSize,            8, "maximum size of a loop optimized by global value numbering") range(0, 128) \
  develop(bool,    EliminateBlocks,             true, "Eliminate unneccessary basic blocks") \
  develop(bool,    EliminateNullChecks,         true, "Eliminate unneccessary null checks") \
  develop(bool,    EliminateFieldAccess,        true, "Optimize field loads and stores") \
  develop(bool,    InlineMethodsWithExceptionHandlers, true, "Inline methods containing exception handlers (NOTE: does not work with current backend)") \
  product(bool,    InlineSynchronizedMethods,   true, "Inline synchronized methods") \
  diagnostic(bool, InlineNIOCheckIndex,         true, "Intrinsify java.nio.Buffer.checkIndex") \
  develop(bool,    CanonicalizeNodes,           true, "Canonicalize graph nodes") \
  develop(bool,    UseTableRanges,              true, "Faster versions of lookup table using ranges") \
  develop_pd(bool, RoundFPResults,                    "Indicates whether rounding is needed for floating point results") \
  develop(intx,    NestedInliningSizeRatio,       90, "Percentage of prev. allowed inline size in recursive inlining") range(0, 100) \
  develop_pd(bool, CSEArrayLength,                    "Create separate nodes for length in array accesses") \
  develop_pd(bool, TwoOperandLIRForm,                 "true if LIR requires src1 and dst to match in binary LIR ops") \
 \
  /* C1 variable */ \
 \
  develop(bool, C1Breakpoint,                  false, "Sets a breakpoint at entry of each compiled method") \
  develop(bool, ImplicitDiv0Checks,             true, "Use implicit division by zero checks") \
  develop(bool, PinAllInstructions,            false, "All instructions are pinned") \
  develop(bool, UseFastNewInstance,             true, "Use fast inlined instance allocation") \
  develop(bool, UseFastNewTypeArray,            true, "Use fast inlined type array allocation") \
  develop(bool, UseFastNewObjectArray,          true, "Use fast inlined object array allocation") \
  develop(bool, UseFastLocking,                 true, "Use fast inlined locking code") \
  develop(bool, GenerateArrayStoreCheck,        true, "Generates code for array store checks") \
  develop(bool, BailoutAfterHIR,               false, "bailout of compilation after building of HIR") \
  develop(bool, BailoutAfterLIR,               false, "bailout of compilation after building of LIR") \
  develop(bool, BailoutOnExceptionHandlers,    false, "bailout of compilation for methods with exception handlers") \
  develop(bool, InstallMethods,                 true, "Install methods at the end of successful compilations") \
  develop(intx, NMethodSizeLimit,    (64*K)*wordSize, "Maximum size of a compiled method.") range(0, max_jint) \
  develop(bool, OptimizeUnsafes,                true, "Optimize raw unsafe ops") \
  develop(intx, InstructionCountCutoff,        37000, "If GraphBuilder adds this many instructions, bails out") range(0, max_jint) \
  develop(bool, ComputeExactFPURegisterUsage,   true, "Compute additional live set for fpu registers to simplify fpu stack merge (Intel only)")

// Read default values for c1 globals

C1_FLAGS(DECLARE_DEVELOPER_FLAG, \
         DECLARE_PD_DEVELOPER_FLAG, \
         DECLARE_PRODUCT_FLAG, \
         DECLARE_PD_PRODUCT_FLAG, \
         DECLARE_DIAGNOSTIC_FLAG, \
         DECLARE_PD_DIAGNOSTIC_FLAG, \
         DECLARE_NOTPRODUCT_FLAG, \
         IGNORE_RANGE, \
         IGNORE_CONSTRAINT, \
         IGNORE_WRITEABLE)

#endif
