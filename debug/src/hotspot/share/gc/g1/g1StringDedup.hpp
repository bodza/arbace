#ifndef SHARE_VM_GC_G1_G1STRINGDEDUP_HPP
#define SHARE_VM_GC_G1_G1STRINGDEDUP_HPP

//
// G1 string deduplication candidate selection
//
// An object is considered a deduplication candidate if all of the following
// statements are true:
//
// - The object is an instance of java.lang.String
//
// - The object is being evacuated from a young heap region
//
// - The object is being evacuated to a young/survivor heap region and the
//   object's age is equal to the deduplication age threshold
//
//   or
//
//   The object is being evacuated to an old heap region and the object's age is
//   less than the deduplication age threshold
//
// Once an string object has been promoted to an old region, or its age is higher
// than the deduplication age threshold, is will never become a candidate again.
// This approach avoids making the same object a candidate more than once.
//

#include "gc/shared/stringdedup/stringDedup.hpp"
#include "memory/allocation.hpp"
#include "oops/oop.hpp"

class OopClosure;
class BoolObjectClosure;
class G1GCPhaseTimes;
class G1StringDedupUnlinkOrOopsDoClosure;

//
// G1 interface for interacting with string deduplication.
//
class G1StringDedup : public StringDedup {
private:

  // Candidate selection policies, returns true if the given object is
  // candidate for string deduplication.
  static bool is_candidate_from_mark(oop obj);
  static bool is_candidate_from_evacuation(bool from_young, bool to_young, oop obj);

public:
  // Initialize string deduplication.
  static void initialize();

  // Enqueues a deduplication candidate for later processing by the deduplication
  // thread. Before enqueuing, these functions apply the appropriate candidate
  // selection policy to filters out non-candidates.
  static void enqueue_from_mark(oop java_string, uint worker_id);
  static void enqueue_from_evacuation(bool from_young, bool to_young,
                                      unsigned int queue, oop java_string);

  static void oops_do(OopClosure* keep_alive);
  static void parallel_unlink(G1StringDedupUnlinkOrOopsDoClosure* unlink, uint worker_id);
  static void unlink_or_oops_do(BoolObjectClosure* is_alive, OopClosure* keep_alive,
                                bool allow_resize_and_rehash, G1GCPhaseTimes* phase_times = NULL);
};

//
// This closure encapsulates the state and the closures needed when scanning
// the deduplication queue and table during the unlink_or_oops_do() operation.
// A single instance of this closure is created and then shared by all worker
// threads participating in the scan.
//
class G1StringDedupUnlinkOrOopsDoClosure : public StringDedupUnlinkOrOopsDoClosure {
public:
  G1StringDedupUnlinkOrOopsDoClosure(BoolObjectClosure* is_alive,
                                     OopClosure* keep_alive,
                                     bool allow_resize_and_rehash) :
    StringDedupUnlinkOrOopsDoClosure(is_alive, keep_alive) {
      if (G1StringDedup::is_enabled()) {
        G1StringDedup::gc_prologue(allow_resize_and_rehash);
      }
    }

  ~G1StringDedupUnlinkOrOopsDoClosure() {
    if (G1StringDedup::is_enabled()) {
      G1StringDedup::gc_epilogue();
    }
  }
};

#endif
