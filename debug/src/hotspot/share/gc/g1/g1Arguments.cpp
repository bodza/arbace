#include "precompiled.hpp"

#include "gc/g1/g1Arguments.hpp"
#include "gc/g1/g1CollectedHeap.inline.hpp"
#include "gc/g1/g1CollectorPolicy.hpp"
#include "gc/g1/g1HeapVerifier.hpp"
#include "gc/g1/heapRegion.hpp"
#include "gc/shared/gcArguments.inline.hpp"
#include "runtime/globals.hpp"
#include "runtime/globals_extension.hpp"
#include "runtime/vm_version.hpp"

size_t G1Arguments::conservative_max_heap_alignment() {
  return HeapRegion::max_region_size();
}

void G1Arguments::initialize_verification_types() {
  if (strlen(VerifyGCType) > 0) {
    const char delimiter[] = " ,\n";
    size_t length = strlen(VerifyGCType);
    char* type_list = NEW_C_HEAP_ARRAY(char, length + 1, mtInternal);
    strncpy(type_list, VerifyGCType, length + 1);
    char* token = strtok(type_list, delimiter);
    while (token != NULL) {
      parse_verification_type(token);
      token = strtok(NULL, delimiter);
    }
    FREE_C_HEAP_ARRAY(char, type_list);
  }
}

void G1Arguments::parse_verification_type(const char* type) {
  if (strcmp(type, "young-normal") == 0) {
    G1HeapVerifier::enable_verification_type(G1HeapVerifier::G1VerifyYoungNormal);
  } else if (strcmp(type, "concurrent-start") == 0) {
    G1HeapVerifier::enable_verification_type(G1HeapVerifier::G1VerifyConcurrentStart);
  } else if (strcmp(type, "mixed") == 0) {
    G1HeapVerifier::enable_verification_type(G1HeapVerifier::G1VerifyMixed);
  } else if (strcmp(type, "remark") == 0) {
    G1HeapVerifier::enable_verification_type(G1HeapVerifier::G1VerifyRemark);
  } else if (strcmp(type, "cleanup") == 0) {
    G1HeapVerifier::enable_verification_type(G1HeapVerifier::G1VerifyCleanup);
  } else if (strcmp(type, "full") == 0) {
    G1HeapVerifier::enable_verification_type(G1HeapVerifier::G1VerifyFull);
  }
}

void G1Arguments::initialize() {
  GCArguments::initialize();
  FLAG_SET_DEFAULT(ParallelGCThreads, Abstract_VM_Version::parallel_worker_threads());
  if (ParallelGCThreads == 0) {
    vm_exit_during_initialization("The flag -XX:+UseG1GC can not be combined with -XX:ParallelGCThreads=0", NULL);
  }

  if (FLAG_IS_DEFAULT(G1ConcRefinementThreads)) {
    FLAG_SET_ERGO(uint, G1ConcRefinementThreads, ParallelGCThreads);
  }

  // MarkStackSize will be set (if it hasn't been set by the user)
  // when concurrent marking is initialized.
  // Its value will be based upon the number of parallel marking threads.
  // But we do set the maximum mark stack size here.
  if (FLAG_IS_DEFAULT(MarkStackSizeMax)) {
    FLAG_SET_DEFAULT(MarkStackSizeMax, 128 * TASKQUEUE_SIZE);
  }

  if (FLAG_IS_DEFAULT(GCTimeRatio) || GCTimeRatio == 0) {
    // In G1, we want the default GC overhead goal to be higher than
    // it is for PS, or the heap might be expanded too aggressively.
    // We set it here to ~8%.
    FLAG_SET_DEFAULT(GCTimeRatio, 12);
  }

  // Below, we might need to calculate the pause time interval based on
  // the pause target. When we do so we are going to give G1 maximum
  // flexibility and allow it to do pauses when it needs to. So, we'll
  // arrange that the pause interval to be pause time target + 1 to
  // ensure that a) the pause time target is maximized with respect to
  // the pause interval and b) we maintain the invariant that pause
  // time target < pause interval. If the user does not want this
  // maximum flexibility, they will have to set the pause interval
  // explicitly.

  if (FLAG_IS_DEFAULT(MaxGCPauseMillis)) {
    // The default pause time target in G1 is 200ms
    FLAG_SET_DEFAULT(MaxGCPauseMillis, 200);
  }

  // Then, if the interval parameter was not set, set it according to
  // the pause time target (this will also deal with the case when the
  // pause time target is the default value).
  if (FLAG_IS_DEFAULT(GCPauseIntervalMillis)) {
    FLAG_SET_DEFAULT(GCPauseIntervalMillis, MaxGCPauseMillis + 1);
  }

  if (FLAG_IS_DEFAULT(ParallelRefProcEnabled) && ParallelGCThreads > 1) {
    FLAG_SET_DEFAULT(ParallelRefProcEnabled, true);
  }

  // By default do not let the target stack size to be more than 1/4 of the entries
  if (FLAG_IS_DEFAULT(GCDrainStackTargetSize)) {
    FLAG_SET_ERGO(uintx, GCDrainStackTargetSize, MIN2(GCDrainStackTargetSize, (uintx)TASKQUEUE_SIZE / 4));
  }

  initialize_verification_types();
}

CollectedHeap* G1Arguments::create_heap() {
  return create_heap_with_policy<G1CollectedHeap, G1CollectorPolicy>();
}
