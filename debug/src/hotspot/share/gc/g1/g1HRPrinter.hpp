#ifndef SHARE_VM_GC_G1_G1HRPRINTER_HPP
#define SHARE_VM_GC_G1_G1HRPRINTER_HPP

#include "gc/g1/heapRegion.hpp"
#include "logging/log.hpp"

#define SKIP_RETIRED_FULL_REGIONS 1

class G1HRPrinter {

private:

  // Print an action event.
  static void print(const char* action, HeapRegion* hr) {
    log_trace(gc, region)("G1HR %s(%s) [" PTR_FORMAT ", " PTR_FORMAT ", " PTR_FORMAT "]",
                          action, hr->get_type_str(), p2i(hr->bottom()), p2i(hr->top()), p2i(hr->end()));
  }

public:
  // In some places we iterate over a list in order to generate output
  // for the list's elements. By exposing this we can avoid this
  // iteration if the printer is not active.
  const bool is_active() { return log_is_enabled(Trace, gc, region); }

  // The methods below are convenient wrappers for the print() method.

  void alloc(HeapRegion* hr, bool force = false) {
    if (is_active()) {
      print((force) ? "ALLOC-FORCE" : "ALLOC", hr);
    }
  }

  void retire(HeapRegion* hr) {
    if (is_active()) {
      if (!SKIP_RETIRED_FULL_REGIONS || hr->top() < hr->end()) {
        print("RETIRE", hr);
      }
    }
  }

  void reuse(HeapRegion* hr) {
    if (is_active()) {
      print("REUSE", hr);
    }
  }

  void cset(HeapRegion* hr) {
    if (is_active()) {
      print("CSET", hr);
    }
  }

  void evac_failure(HeapRegion* hr) {
    if (is_active()) {
      print("EVAC-FAILURE", hr);
    }
  }

  void cleanup(HeapRegion* hr) {
    if (is_active()) {
      print("CLEANUP", hr);
    }
  }

  void post_compaction(HeapRegion* hr) {
    if (is_active()) {
      print("POST-COMPACTION", hr);
    }
  }

  void commit(HeapRegion* hr) {
    if (is_active()) {
      print("COMMIT", hr);
    }
  }

  void uncommit(HeapRegion* hr) {
    if (is_active()) {
      print("UNCOMMIT", hr);
    }
  }
};

#endif
