#include "precompiled.hpp"
#include "gc/g1/heapRegionBounds.inline.hpp"
#include "gc/g1/jvmFlagConstraintsG1.hpp"
#include "runtime/globals_extension.hpp"
#include "utilities/globalDefinitions.hpp"

JVMFlag::Error G1RSetRegionEntriesConstraintFunc(intx value, bool verbose) {
  if (!UseG1GC) return JVMFlag::SUCCESS;

  // Default value of G1RSetRegionEntries=0 means will be set ergonomically.
  // Minimum value is 1.
  if (FLAG_IS_CMDLINE(G1RSetRegionEntries) && (value < 1)) {
    JVMFlag::printError(verbose,
                        "G1RSetRegionEntries (" INTX_FORMAT ") must be "
                        "greater than or equal to 1\n",
                        value);
    return JVMFlag::VIOLATES_CONSTRAINT;
  } else {
    return JVMFlag::SUCCESS;
  }
}

JVMFlag::Error G1RSetSparseRegionEntriesConstraintFunc(intx value, bool verbose) {
  if (!UseG1GC) return JVMFlag::SUCCESS;

  // Default value of G1RSetSparseRegionEntries=0 means will be set ergonomically.
  // Minimum value is 1.
  if (FLAG_IS_CMDLINE(G1RSetSparseRegionEntries) && (value < 1)) {
    JVMFlag::printError(verbose,
                        "G1RSetSparseRegionEntries (" INTX_FORMAT ") must be "
                        "greater than or equal to 1\n",
                        value);
    return JVMFlag::VIOLATES_CONSTRAINT;
  } else {
    return JVMFlag::SUCCESS;
  }
}

JVMFlag::Error G1HeapRegionSizeConstraintFunc(size_t value, bool verbose) {
  if (!UseG1GC) return JVMFlag::SUCCESS;

  // Default value of G1HeapRegionSize=0 means will be set ergonomically.
  if (FLAG_IS_CMDLINE(G1HeapRegionSize) && (value < HeapRegionBounds::min_size())) {
    JVMFlag::printError(verbose,
                        "G1HeapRegionSize (" SIZE_FORMAT ") must be "
                        "greater than or equal to ergonomic heap region minimum size\n",
                        value);
    return JVMFlag::VIOLATES_CONSTRAINT;
  } else {
    return JVMFlag::SUCCESS;
  }
}

JVMFlag::Error G1NewSizePercentConstraintFunc(uintx value, bool verbose) {
  if (!UseG1GC) return JVMFlag::SUCCESS;

  if (value > G1MaxNewSizePercent) {
    JVMFlag::printError(verbose,
                        "G1NewSizePercent (" UINTX_FORMAT ") must be "
                        "less than or equal to G1MaxNewSizePercent (" UINTX_FORMAT ")\n",
                        value, G1MaxNewSizePercent);
    return JVMFlag::VIOLATES_CONSTRAINT;
  } else {
    return JVMFlag::SUCCESS;
  }
}

JVMFlag::Error G1MaxNewSizePercentConstraintFunc(uintx value, bool verbose) {
  if (!UseG1GC) return JVMFlag::SUCCESS;

  if (value < G1NewSizePercent) {
    JVMFlag::printError(verbose,
                        "G1MaxNewSizePercent (" UINTX_FORMAT ") must be "
                        "greater than or equal to G1NewSizePercent (" UINTX_FORMAT ")\n",
                        value, G1NewSizePercent);
    return JVMFlag::VIOLATES_CONSTRAINT;
  } else {
    return JVMFlag::SUCCESS;
  }
}

JVMFlag::Error MaxGCPauseMillisConstraintFuncG1(uintx value, bool verbose) {
  if (UseG1GC && FLAG_IS_CMDLINE(MaxGCPauseMillis) && (value >= GCPauseIntervalMillis)) {
    JVMFlag::printError(verbose,
                        "MaxGCPauseMillis (" UINTX_FORMAT ") must be "
                        "less than GCPauseIntervalMillis (" UINTX_FORMAT ")\n",
                        value, GCPauseIntervalMillis);
    return JVMFlag::VIOLATES_CONSTRAINT;
  }

  return JVMFlag::SUCCESS;
}

JVMFlag::Error GCPauseIntervalMillisConstraintFuncG1(uintx value, bool verbose) {
  if (UseG1GC) {
    if (FLAG_IS_CMDLINE(GCPauseIntervalMillis)) {
      if (value < 1) {
        JVMFlag::printError(verbose,
                            "GCPauseIntervalMillis (" UINTX_FORMAT ") must be "
                            "greater than or equal to 1\n",
                            value);
        return JVMFlag::VIOLATES_CONSTRAINT;
      }

      if (FLAG_IS_DEFAULT(MaxGCPauseMillis)) {
        JVMFlag::printError(verbose,
                            "GCPauseIntervalMillis cannot be set "
                            "without setting MaxGCPauseMillis\n");
        return JVMFlag::VIOLATES_CONSTRAINT;
      }

      if (value <= MaxGCPauseMillis) {
        JVMFlag::printError(verbose,
                            "GCPauseIntervalMillis (" UINTX_FORMAT ") must be "
                            "greater than MaxGCPauseMillis (" UINTX_FORMAT ")\n",
                            value, MaxGCPauseMillis);
        return JVMFlag::VIOLATES_CONSTRAINT;
      }
    }
  }

  return JVMFlag::SUCCESS;
}

JVMFlag::Error NewSizeConstraintFuncG1(size_t value, bool verbose) {
  // Overflow would happen for uint type variable of YoungGenSizer::_min_desired_young_length
  // when the value to be assigned exceeds uint range.
  // i.e. result of '(uint)(NewSize / region size(1~32MB))'
  // So maximum of NewSize should be 'max_juint * 1M'
  if (UseG1GC && (value > (max_juint * 1 * M))) {
    JVMFlag::printError(verbose,
                        "NewSize (" SIZE_FORMAT ") must be less than ergonomic maximum value\n",
                        value);
    return JVMFlag::VIOLATES_CONSTRAINT;
  }
  return JVMFlag::SUCCESS;
}

size_t MaxSizeForHeapAlignmentG1() {
  return HeapRegionBounds::max_size();
}
