#include "precompiled.hpp"

#include "runtime/globals.hpp"
#include "runtime/globals_extension.hpp"
#include "compiler/compilerDefinitions.hpp"
#include "gc/shared/gcConfig.hpp"
#include "utilities/defaultStream.hpp"

const char* compilertype2name_tab[compiler_number_of_types] = {
  "",
  "c1",
  "c2",
  "jvmci"
};

CompLevel  CompLevel_highest_tier      = CompLevel_simple;             // pure C1 or JVMCI

CompLevel  CompLevel_initial_compile   = CompLevel_simple;              // pure C1 or JVMCI

CompMode  Compilation_mode             = CompMode_client;

// Returns threshold scaled with CompileThresholdScaling
intx CompilerConfig::scaled_compile_threshold(intx threshold) {
  return scaled_compile_threshold(threshold, CompileThresholdScaling);
}

// Returns freq_log scaled with CompileThresholdScaling
intx CompilerConfig::scaled_freq_log(intx freq_log) {
  return scaled_freq_log(freq_log, CompileThresholdScaling);
}

// Returns threshold scaled with the value of scale.
// If scale < 0.0, threshold is returned without scaling.
intx CompilerConfig::scaled_compile_threshold(intx threshold, double scale) {
  if (scale == 1.0 || scale < 0.0) {
    return threshold;
  } else {
    return (intx)(threshold * scale);
  }
}

// Returns freq_log scaled with the value of scale.
// Returned values are in the range of [0, InvocationCounter::number_of_count_bits + 1].
// If scale < 0.0, freq_log is returned without scaling.
intx CompilerConfig::scaled_freq_log(intx freq_log, double scale) {
  // Check if scaling is necessary or if negative value was specified.
  if (scale == 1.0 || scale < 0.0) {
    return freq_log;
  }
  // Check values to avoid calculating log2 of 0.
  if (scale == 0.0 || freq_log == 0) {
    return 0;
  }
  // Determine the maximum notification frequency value currently supported.
  // The largest mask value that the interpreter/C1 can handle is
  // of length InvocationCounter::number_of_count_bits. Mask values are always
  // one bit shorter then the value of the notification frequency. Set
  // max_freq_bits accordingly.
  intx max_freq_bits = InvocationCounter::number_of_count_bits + 1;
  intx scaled_freq = scaled_compile_threshold((intx)1 << freq_log, scale);
  if (scaled_freq == 0) {
    // Return 0 right away to avoid calculating log2 of 0.
    return 0;
  } else if (scaled_freq > nth_bit(max_freq_bits)) {
    return max_freq_bits;
  } else {
    return log2_intptr(scaled_freq);
  }
}

void CompilerConfig::set_tiered_flags() {
  // With tiered, set default policy to SimpleThresholdPolicy, which is 2.
  if (FLAG_IS_DEFAULT(CompilationPolicyChoice)) {
    FLAG_SET_DEFAULT(CompilationPolicyChoice, 2);
  }
  if (CompilationPolicyChoice < 2) {
    vm_exit_during_initialization("Incompatible compilation policy selected", NULL);
  }
  // Increase the code cache size - tiered compiles a lot more.
  if (FLAG_IS_DEFAULT(ReservedCodeCacheSize)) {
    FLAG_SET_ERGO(uintx, ReservedCodeCacheSize, MIN2(CODE_CACHE_DEFAULT_LIMIT, (size_t)ReservedCodeCacheSize * 5));
  }
  // Enable SegmentedCodeCache if ... is enabled and ReservedCodeCacheSize >= 240M
  if (FLAG_IS_DEFAULT(SegmentedCodeCache) && ReservedCodeCacheSize >= 240*M) {
    FLAG_SET_ERGO(bool, SegmentedCodeCache, true);
  }
  { // -Xcomp
    Tier3InvokeNotifyFreqLog = 0;
    Tier4InvocationThreshold = 0;
  }

  if (CompileThresholdScaling < 0) {
    vm_exit_during_initialization("Negative value specified for CompileThresholdScaling", NULL);
  }

  // Scale tiered compilation thresholds.
  // CompileThresholdScaling == 0.0 is equivalent to -Xint and leaves compilation thresholds unchanged.
  if (!FLAG_IS_DEFAULT(CompileThresholdScaling) && CompileThresholdScaling > 0.0) {
    FLAG_SET_ERGO(intx, Tier0InvokeNotifyFreqLog, scaled_freq_log(Tier0InvokeNotifyFreqLog));
    FLAG_SET_ERGO(intx, Tier0BackedgeNotifyFreqLog, scaled_freq_log(Tier0BackedgeNotifyFreqLog));

    FLAG_SET_ERGO(intx, Tier3InvocationThreshold, scaled_compile_threshold(Tier3InvocationThreshold));
    FLAG_SET_ERGO(intx, Tier3MinInvocationThreshold, scaled_compile_threshold(Tier3MinInvocationThreshold));
    FLAG_SET_ERGO(intx, Tier3CompileThreshold, scaled_compile_threshold(Tier3CompileThreshold));
    FLAG_SET_ERGO(intx, Tier3BackEdgeThreshold, scaled_compile_threshold(Tier3BackEdgeThreshold));

    // Tier2{Invocation,MinInvocation,Compile,Backedge}Threshold should be scaled here
    // once these thresholds become supported.

    FLAG_SET_ERGO(intx, Tier2InvokeNotifyFreqLog, scaled_freq_log(Tier2InvokeNotifyFreqLog));
    FLAG_SET_ERGO(intx, Tier2BackedgeNotifyFreqLog, scaled_freq_log(Tier2BackedgeNotifyFreqLog));

    FLAG_SET_ERGO(intx, Tier3InvokeNotifyFreqLog, scaled_freq_log(Tier3InvokeNotifyFreqLog));
    FLAG_SET_ERGO(intx, Tier3BackedgeNotifyFreqLog, scaled_freq_log(Tier3BackedgeNotifyFreqLog));

    FLAG_SET_ERGO(intx, Tier23InlineeNotifyFreqLog, scaled_freq_log(Tier23InlineeNotifyFreqLog));

    FLAG_SET_ERGO(intx, Tier4InvocationThreshold, scaled_compile_threshold(Tier4InvocationThreshold));
    FLAG_SET_ERGO(intx, Tier4MinInvocationThreshold, scaled_compile_threshold(Tier4MinInvocationThreshold));
    FLAG_SET_ERGO(intx, Tier4CompileThreshold, scaled_compile_threshold(Tier4CompileThreshold));
    FLAG_SET_ERGO(intx, Tier4BackEdgeThreshold, scaled_compile_threshold(Tier4BackEdgeThreshold));
  }
}

void set_jvmci_specific_flags() {
  if (UseJVMCICompiler) {
    Compilation_mode = CompMode_server;

    if (FLAG_IS_DEFAULT(TypeProfileWidth)) {
      FLAG_SET_DEFAULT(TypeProfileWidth, 8);
    }
    if (FLAG_IS_DEFAULT(OnStackReplacePercentage)) {
      FLAG_SET_DEFAULT(OnStackReplacePercentage, 933);
    }
    // JVMCI needs values not less than defaults
    if (FLAG_IS_DEFAULT(ReservedCodeCacheSize)) {
      FLAG_SET_DEFAULT(ReservedCodeCacheSize, MAX2(64*M, ReservedCodeCacheSize));
    }
    if (FLAG_IS_DEFAULT(InitialCodeCacheSize)) {
      FLAG_SET_DEFAULT(InitialCodeCacheSize, MAX2(16*M, InitialCodeCacheSize));
    }
    if (FLAG_IS_DEFAULT(MetaspaceSize)) {
      FLAG_SET_DEFAULT(MetaspaceSize, MAX2(12*M, MetaspaceSize));
    }
    if (FLAG_IS_DEFAULT(NewSizeThreadIncrease)) {
      FLAG_SET_DEFAULT(NewSizeThreadIncrease, MAX2(4*K, NewSizeThreadIncrease));
    }
    if (TieredStopAtLevel != CompLevel_full_optimization) {
      // Currently JVMCI compiler can only work at the full optimization level
      warning("forcing TieredStopAtLevel to full optimization because JVMCI is enabled");
      FLAG_SET_ERGO(intx, TieredStopAtLevel, CompLevel_full_optimization);
    }
    if (FLAG_IS_DEFAULT(TypeProfileLevel)) {
      FLAG_SET_DEFAULT(TypeProfileLevel, 0);
    }
  }
}

bool CompilerConfig::check_args_consistency(bool status) {
  // Check lower bounds of the code cache
  uint min_code_cache_size = CodeCacheMinimumUseSpace;
  if (ReservedCodeCacheSize < InitialCodeCacheSize) {
    jio_fprintf(defaultStream::error_stream(), "Invalid ReservedCodeCacheSize: %dK. Must be at least InitialCodeCacheSize=%dK.\n", ReservedCodeCacheSize/K, InitialCodeCacheSize/K);
    status = false;
  } else if (ReservedCodeCacheSize < min_code_cache_size) {
    jio_fprintf(defaultStream::error_stream(), "Invalid ReservedCodeCacheSize=%dK. Must be at least %uK.\n", ReservedCodeCacheSize/K, min_code_cache_size/K);
    status = false;
  } else if (ReservedCodeCacheSize > CODE_CACHE_SIZE_LIMIT) {
    // Code cache size larger than CODE_CACHE_SIZE_LIMIT is not supported.
    jio_fprintf(defaultStream::error_stream(), "Invalid ReservedCodeCacheSize=%dM. Must be at most %uM.\n", ReservedCodeCacheSize/M, CODE_CACHE_SIZE_LIMIT/M);
    status = false;
  } else if (NonNMethodCodeHeapSize < min_code_cache_size) {
    jio_fprintf(defaultStream::error_stream(), "Invalid NonNMethodCodeHeapSize=%dK. Must be at least %uK.\n", NonNMethodCodeHeapSize/K, min_code_cache_size/K);
    status = false;
  }

  if (!FLAG_IS_DEFAULT(CICompilerCount) && !FLAG_IS_DEFAULT(CICompilerCountPerCPU) && CICompilerCountPerCPU) {
    warning("The VM option CICompilerCountPerCPU overrides CICompilerCount.");
  }

  if (BackgroundCompilation && (CompileTheWorld || ReplayCompiles)) {
    if (!FLAG_IS_DEFAULT(BackgroundCompilation)) {
      warning("BackgroundCompilation disabled due to CompileTheWorld or ReplayCompiles options.");
    }
    FLAG_SET_CMDLINE(bool, BackgroundCompilation, false);
  }

  return status && JVMCIGlobals::check_jvmci_flags_are_consistent();
}

void CompilerConfig::ergo_initialize() {
  // Check that JVMCI compiler supports selested GC.
  // Should be done after GCConfig::initialize() was called.
  JVMCIGlobals::check_jvmci_supported_gc();
  set_jvmci_specific_flags();

  {
    int max_compilation_policy_choice = 1;
    // Check if the policy is valid.
    if (CompilationPolicyChoice >= max_compilation_policy_choice) {
      vm_exit_during_initialization("Incompatible compilation policy selected", NULL);
    }
    // Scale CompileThreshold
    // CompileThresholdScaling == 0.0 is equivalent to -Xint and leaves CompileThreshold unchanged.
    if (!FLAG_IS_DEFAULT(CompileThresholdScaling) && CompileThresholdScaling > 0.0) {
      FLAG_SET_ERGO(intx, CompileThreshold, scaled_compile_threshold(CompileThreshold));
    }
  }

  if (UseOnStackReplacement && !UseLoopCounter) {
    warning("On-stack-replacement requires loop counters; enabling loop counters");
    FLAG_SET_DEFAULT(UseLoopCounter, true);
  }
}
