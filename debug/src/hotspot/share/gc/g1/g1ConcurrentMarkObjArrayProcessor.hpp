#ifndef SHARE_VM_GC_G1_G1CONCURRENTMARKOBJARRAYPROCESSOR_HPP
#define SHARE_VM_GC_G1_G1CONCURRENTMARKOBJARRAYPROCESSOR_HPP

#include "oops/oopsHierarchy.hpp"

class G1CMTask;

// Helper class to mark through large objArrays during marking in an efficient way.
// Instead of pushing large object arrays, we push continuations onto the
// mark stack. These continuations are identified by having their LSB set.
// This allows incremental processing of large objects.
class G1CMObjArrayProcessor {
private:
  // Reference to the task for doing the actual work.
  G1CMTask* _task;

  // Push the continuation at the given address onto the mark stack.
  void push_array_slice(HeapWord* addr);

  // Process (apply the closure) on the given continuation of the given objArray.
  size_t process_array_slice(objArrayOop const obj, HeapWord* start_from, size_t remaining);
public:
  static bool should_be_sliced(oop obj);

  G1CMObjArrayProcessor(G1CMTask* task) : _task(task) {
  }

  // Process the given continuation. Returns the number of words scanned.
  size_t process_slice(HeapWord* slice);
  // Start processing the given objArrayOop by scanning the header and pushing its
  // continuation.
  size_t process_obj(oop obj);
};

#endif
